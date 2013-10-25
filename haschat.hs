-- adapted from tutorial http://www.catonmat.net/blog/simple-haskell-tcp-server/
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
import Control.Concurrent (forkIO)
import qualified Data.Map as Map
import Safe

data Account = Account String String deriving (Eq, Show, Read)
type AccountDB = Map.Map String String

accountDB :: [String] -> AccountDB
accountDB lns =
  foldl add_account Map.empty accounts
  where
    add_account mp (Account user passhash) = Map.insert user passhash mp
    accounts = 
      let
        fn Nothing xs  = xs
        fn (Just x) xs = x:xs
      in foldr (fn . readMay) [] lns

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
  sock <- listenOn $ PortNumber port
  putStrLn $ "listening on " ++ (show port)
  sockHandler sock account_db

sockHandler :: Socket -> AccountDB -> IO ()
sockHandler sock account_db = do
  (handle, _, _) <- accept sock
  hSetBuffering handle NoBuffering
  forkIO $ login handle account_db
  sockHandler sock account_db

validatePass :: String -> String -> String -> Bool
validatePass user pass passhash = pass == passhash

login :: Handle -> AccountDB -> IO ()
login handle account_db = do
  hPutStr handle "i can has user? "
  user <- hGetLine handle
  hPutStr handle "i can has password? "
  pass <- hGetLine handle

  case (Map.lookup user account_db) of
    Nothing       -> do hPutStrLn handle "invalid login"
    Just passhash -> do 
      if (validatePass user pass passhash)
        then do run handle user
        else do hPutStrLn handle "invlaid login"

  hClose handle

run :: Handle -> String -> IO ()
run handle user = do
  hPutStrLn handle $ "welcome " ++ user ++ ", u can haschat now."
