-- adapted from tutorial http://www.catonmat.net/blog/simple-haskell-tcp-server/
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

parseArgs :: (Num a) => [String] -> a
parseArgs []     = 9393
parseArgs (x:xs) = fromIntegral $ read x 

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  let port = parseArgs args
  sock <- listenOn $ PortNumber port
  putStrLn $ "listening on " ++ (show port)
  sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
  (handle, _, _) <- accept sock
  hSetBuffering handle NoBuffering
  forkIO $ commandProcessor handle
  sockHandler sock

commandProcessor :: Handle -> IO ()
commandProcessor handle = do
  line <- hGetLine handle
  let cmd = words line
  case (head cmd) of
    ("echo") -> echoCommand handle cmd
    _ -> do hPutStrLn handle "unknown command"
  commandProcessor handle

echoCommand :: Handle -> [String] -> IO ()
echoCommand handle cmd = do
  hPutStrLn handle (unwords $ tail cmd)
