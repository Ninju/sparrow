module Server where
import System.IO
import Control.Exception hiding (catch)
import Control.Concurrent
import Control.Monad (forM_)
import Network
import Data.List

type Connection = (Handle, String, PortNumber)

run :: IO ()
run = bracket (listenOn $ PortNumber portNumber) sClose setup

portNumber :: PortNumber
portNumber = 3333

prompt :: String
prompt = "> "

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

acceptConnections :: Socket -> (MVar [String], MVar [Handle], MVar ()) -> IO ()
acceptConnections socket mvs@(mvMessages, mvHandles, mvNewMessage) = do connection@(handle, name, port) <- accept socket
                                                                        addHandle handle mvs
                                                                        messages <- readMVar mvMessages
                                                                        printMessages handle messages
                                                                        forkIO $ catch (handleClient connection mvs `finally` closeConnection connection mvs) (\exception -> print exception)
                                                                        acceptConnections socket mvs


handleClient :: Connection -> (MVar [String], MVar [Handle], MVar ()) -> IO ()
handleClient connection@(handle, _, _) mvs = hGetLine handle >>= handleInput connection mvs

handleInput :: Connection -> (MVar [String], MVar [Handle], MVar ()) -> String -> IO ()
handleInput connection@(handle, _, _) mvs@(mvMessages, _, mvNewMessage) input = case (input \\ "\r") of
                                                                                  "q" -> return ()
                                                                                  _   -> do addMessage input mvs
                                                                                            handleClient connection mvs

addMessage :: String -> (MVar [String], MVar [Handle], MVar ()) -> IO ()
addMessage message (mvMessages, _, mvNewMessage) = do putStrLn $ "Adding message: " ++ show message
                                                      updateMVar mvMessages (message:)
                                                      putMVar mvNewMessage ()
                                                      return ()

addHandle :: Handle -> (MVar [String], MVar [Handle], MVar ()) -> IO ()
addHandle handle (_, mvHandles, _) = do putStrLn $ "Adding handle: " ++ show handle
                                        updateMVar mvHandles (handle:)

displayMessages :: (MVar [String], MVar [Handle], MVar ()) -> IO ()
displayMessages mvs@(mvMessages, mvHandles, mvNewMessage) = do putStrLn "Waiting for messages..."
                                                               takeMVar mvNewMessage
                                                               messages <- readMVar mvMessages
                                                               handles  <- readMVar mvHandles
                                                               putStrLn (show handles)
                                                               putStrLn (show messages)
                                                               mapM_ (flip printMessages messages) handles
                                                               displayMessages mvs

printMessages :: Handle -> [String] -> IO ()
printMessages h ms = do hClearScreen h
                        mapM_ (printMessage h) $ (reverse . take 10) ms
                        hPutStr h prompt
                        hFlush h

printMessage :: Handle -> String -> IO ()
printMessage h m = hPutStrLn h m >> hFlush h

closeConnection :: Connection -> (MVar [String], MVar [Handle], MVar ()) -> IO ()
closeConnection connection@(handle, _, _)  mvs@(_, mvHandles, _) = do putStrLn $ show handle ++ " is leaving."
                                                                      updateMVar mvHandles (delete handle)
                                                                      hClose handle

