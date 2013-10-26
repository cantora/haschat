-- adapted from tutorial http://www.catonmat.net/blog/simple-haskell-tcp-server/
--                   and http://www.haskell.org/haskellwiki/Implement_a_chat_server
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (
  hClose,
  hSetBuffering,
  hGetLine,
  hPutStrLn,
  hPutStr,
  BufferMode(..),
  Handle
  )
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan
import Control.Monad.Fix
import Control.Monad (when, liftM)
import qualified Control.Exception as Exception
import Control.Exception (SomeException(..))
import qualified Data.Map as Map
import Safe
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Digest.Pure.MD5
import Data.Maybe

data Account = Account String String deriving (Eq, Show, Read)
type AccountDB = Map.Map String String

type Msg = String

accountDB :: [String] -> AccountDB
accountDB lns =
  foldl add_account Map.empty $ catMaybes $ map readMay lns
  where
    add_account mp (Account user passhash) = Map.insert user passhash mp

parseArgs :: (Num a) => [String] -> a
parseArgs []     = 9393
parseArgs (x:xs) = fromIntegral $ read x 

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  let port = parseArgs args

  putStrLn "importing account data from stdin"
  account_data <- getContents
  let account_db = accountDB $ lines account_data
  putStrLn $ "account db" ++ (show account_db) 

  chan <- newChan
  --reader for original channel, needed prevent memory leak
  forkIO $ fix $ \loop -> do
    msg <- readChan chan
    loop

  sock <- listenOn $ PortNumber port
  putStrLn $ "listening on " ++ (show port)
  sockHandler sock account_db chan

sockHandler :: Socket -> AccountDB -> Chan Msg -> IO ()
sockHandler sock account_db chan = do
  (handle, hostname, pn) <- accept sock
  putStrLn $ "connection from " ++ (show hostname) ++ ":" ++ (show pn)
  hSetBuffering handle NoBuffering
  forkIO $ login handle account_db chan
  sockHandler sock account_db chan

validatePass :: String -> String -> String -> Bool
validatePass user pass passhash =
  (show $ (md5 . Char8.pack) pass)  == passhash

login :: Handle -> AccountDB -> Chan Msg -> IO ()
login handle account_db chan = do
  hPutStr handle "i can has user? "
  user <- hGetLine handle
  hPutStr handle "i can has password? "
  pass <- hGetLine handle
  putStrLn $ "login attempt from " ++ (show $ take 14 user) ++ " " ++ (show $ take 14 pass)

  case (Map.lookup user account_db) of
    Nothing       -> do hPutStrLn handle "invalid login"
    Just passhash -> do 
      if (validatePass user pass passhash)
        then do run chan handle user 
        else do hPutStrLn handle "invlaid login"

  hClose handle

run :: Chan Msg -> Handle -> String -> IO ()
run chan handle user = 
  do
    putStrLn $ user ++ " logged in"
    chan' <- dupChan chan
    chanreader <- chanloop chan'
    broadcast $ "<" ++ user ++ " can haschat now>"
    Exception.handle ignore readloop 
    
    killThread chanreader
    broadcast $ "<" ++ user ++ " not can haschat now>"
    putStrLn $ user ++ " left"
  where
    broadcast msg = writeChan chan msg
    reply msg     = hPutStrLn handle msg

    chanloop ch = forkIO $ fix $ \loop -> do
      readChan ch >>= reply
      loop

    readloop = fix $ \loop -> do
      line <- hGetLine handle
      broadcast (user ++ ": " ++ line)
      loop

    ignore (SomeException _) = return ()
