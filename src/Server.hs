module Server
(
  server
) where

import Control.Exception.Base (bracket, catch, throwIO)
import Data.Bits ((.|.))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy as HM
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Pool (createPool, destroyAllResources)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Database.MySQL.Base (ConnectInfo (connectOptions))
import Database.MySQL.Base.Types (Option (ReadDefaultGroup))
import qualified Database.MySQL.Simple as MySQL
import Network.Socket (Family (AF_INET, AF_UNIX),
                       SockAddr (SockAddrInet, SockAddrUnix), Socket,
                       SocketOption (ReuseAddr), SocketType (Stream), bind,
                       close, getSocketName, inet_addr, listen, maxListenQueue,
                       setSocketOption, socket)
import Network.Wai.Handler.Warp (Port, defaultSettings, runSettingsSocket)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (groupReadMode, groupWriteMode, ownerReadMode,
                           ownerWriteMode, removeLink, setFileMode, socketMode)

import Application (app)

type Listen = Either Port FilePath


server :: Listen -> [ConnectInfo] -> FilePath -> IO ()
server socketSpec mysqlConnInfo dataDir =
  bracket
    ( do
      sock <- createSocket socketSpec
      mysql <- HM.fromList <$> mapM (\c -> do
          p <- createPool (MySQL.connect c) MySQL.close 1 60 10
          return (getGroup c, p)) mysqlConnInfo
      return (sock, mysql) )
    ( \(sock, mysql) -> do
      closeSocket sock
      mapM_ destroyAllResources $ HM.elems mysql )
    ( \(sock, mysql) -> do
      listen sock maxListenQueue
      hPutStrLn stderr $ "Static files from `" ++ dataDir ++ "'"
      runSettingsSocket defaultSettings sock =<< app mysql dataDir )

getGroup :: ConnectInfo -> Text
getGroup = decodeUtf8 . getName . fromJust . find isGroup . connectOptions
  where
    isGroup (ReadDefaultGroup _) = True
    isGroup _                    = False
    -- FIXME: Removing trailing zero added for buggy mysql in Main.hs.
    getName (ReadDefaultGroup n) = LBS.takeWhile (0 /=) . LBS.fromStrict $ n
    getName _                    = error "Cannot happen"


createSocket :: Listen -> IO Socket
createSocket (Right path) = do
  removeIfExists path
  sock <- socket AF_UNIX Stream 0
  bind sock $ SockAddrUnix path
  setFileMode path $ socketMode
                  .|. ownerWriteMode .|. ownerReadMode
                  .|. groupWriteMode .|. groupReadMode
  hPutStrLn stderr $ "Listening on UNIX socket `" ++ path ++ "'"
  return sock
createSocket (Left port) = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  addr <- inet_addr "127.0.0.1"
  bind sock $ SockAddrInet (fromIntegral port) addr
  hPutStrLn stderr $ "Listening on localhost:" ++ show port
  return sock


closeSocket :: Socket -> IO ()
closeSocket sock = do
  name <- getSocketName sock
  close sock
  case name of
    SockAddrUnix path -> removeIfExists path
    _                 -> return ()


removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeLink fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

