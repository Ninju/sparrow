module Server where
import System.IO
import Control.Exception hiding (catch)
import Control.Concurrent
import Control.Monad (forM_)
import Network
import Data.List
import Text.Regex.Posix ((=~))

type Message = (String, String)
type Connection = (Handle, String, PortNumber)

run :: IO ()
run = bracket (listenOn $ PortNumber portNumber) sClose setup

portNumber :: PortNumber
portNumber = 3333

prompt :: String
prompt = "> "

removeCR :: String -> String
removeCR = (\\ "\r")

hClearScreen :: Handle -> IO ()
hClearScreen h = hPutStrLn h "\ESC[2J\ESC[H"

updateMVar :: MVar a -> (a -> a) -> IO ()
updateMVar mv f = takeMVar mv >>= putMVar mv . f

setup :: Socket -> IO ()
setup socket = do mvMessages   <- newMVar []
                  mvHandles    <- newMVar []
                  mvNewMessage <- newEmptyMVar
                  putStrLn "Created MVars"
                  let mvs = (mvMessages, mvHandles, mvNewMessage)
                  forkIO $ displayMessages mvs
                  acceptConnections socket mvs

acceptConnections :: Socket -> (MVar [Message], MVar [Handle], MVar ()) -> IO ()
acceptConnections socket mvs@(mvMessages, mvHandles, mvNewMessage) = do connection@(handle, host, port) <- accept socket
                                                                        addHandle handle mvs
                                                                        hPutStrLn handle "What is your name?"
                                                                        name <- hGetName handle
                                                                        messages <- readMVar mvMessages
                                                                        printMessages handle messages
                                                                        forkIO $ catch (handleClient connection mvs name `finally` closeConnection connection mvs) (\exception -> print exception)
                                                                        acceptConnections socket mvs

hGetName :: Handle -> IO String
hGetName handle = do hPutStr handle prompt
                     hFlush handle
                     name <- hGetLine handle
                     if removeCR name =~ " " :: Bool
                       then do hPutStrLn handle "Name must not contain spaces."
                               hGetName handle
                       else if null (removeCR name)
                              then do hPutStrLn handle "Name must not be blank."
                                      hGetName handle
                              else return (removeCR name)

handleClient :: Connection -> (MVar [Message], MVar [Handle], MVar ()) -> String -> IO ()
handleClient connection@(handle, _, _) mvs name = hGetLine handle >>= handleInput connection mvs name

handleInput :: Connection -> (MVar [Message], MVar [Handle], MVar ()) -> String -> String -> IO ()
handleInput connection@(handle, _, _) mvs@(mvMessages, _, mvNewMessage) name input = case removeCR input of
                                                                                       "q" -> return ()
                                                                                       _   -> do addMessage name input mvs
                                                                                                 handleClient connection mvs name

addMessage :: String -> String -> (MVar [Message], MVar [Handle], MVar ()) -> IO ()
addMessage name message (mvMessages, _, mvNewMessage) = do putStrLn $ "Adding message: " ++ show message
                                                           updateMVar mvMessages ((name, message):)
                                                           putMVar mvNewMessage ()
                                                           return ()

addHandle :: Handle -> (MVar [Message], MVar [Handle], MVar ()) -> IO ()
addHandle handle (_, mvHandles, _) = do putStrLn $ "Adding handle: " ++ show handle
                                        updateMVar mvHandles (handle:)

displayMessages :: (MVar [Message], MVar [Handle], MVar ()) -> IO ()
displayMessages mvs@(mvMessages, mvHandles, mvNewMessage) = do putStrLn "Waiting for messages..."
                                                               takeMVar mvNewMessage
                                                               messages <- readMVar mvMessages
                                                               handles  <- readMVar mvHandles
                                                               putStrLn (show handles)
                                                               putStrLn (show messages)
                                                               mapM_ (flip printMessages messages) handles
                                                               displayMessages mvs

printMessages :: Handle -> [Message] -> IO ()
printMessages h ms = do hClearScreen h
                        mapM_ (printMessage h) $ (reverse . take 10) ms
                        hPutStr h prompt
                        hFlush h

printMessage :: Handle -> Message -> IO ()
printMessage h (n, m) = hPutStrLn h (n ++ ": " ++ m) >> hFlush h

closeConnection :: Connection -> (MVar [Message], MVar [Handle], MVar ()) -> IO ()
closeConnection connection@(handle, _, _)  mvs@(_, mvHandles, _) = do putStrLn $ show handle ++ " is leaving."
                                                                      updateMVar mvHandles (delete handle)
                                                                      hClose handle

