{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Control.Monad
import System.Environment
import Language.Haskell.Interpreter
import Parser
import Data.Function (fix)
import System.FilePath (dropExtension)
import Network.Socket
import Foreign.C.Types
import Foreign.Ptr
import qualified Data.ByteString as BS
import System.Posix.Types
import System.Posix.IO.ByteString
import qualified Data.ByteString.Internal as BS (createUptoN)
import Foreign.C.Error
import System.Posix.Internals (setNonBlockingFD)
import Control.Exception

main :: IO ()
main = do
    modules <- getArgs
    result <- runInterpreter $ do
        liftIO $ putStrLn "loadModules"
        loadModules modules
        liftIO $ putStrLn "setTopLevelModules"
        setTopLevelModules (map dropExtension modules)
        set [languageExtensions := [DataKinds, TypeOperators]]

        let go state = do
                liftIO $ putStrLn $ "-----------------------------"
                liftIO $ putStrLn $ "eval:   " ++ state
                ty <- normalizeType $ "Force (" ++ state ++ ")"
                liftIO $ putStrLn $ "result: " ++ ty
                case parseOperation ty of
                  Left err -> liftIO (print err)
                  Right Exit -> pure ()
                  Right op -> liftIO (execute op) >>= go

        go "Main"

    case result of
        Left (WontCompile errors) -> mapM_ (putStrLn . errMsg) errors
        Left err -> print err
        Right _ -> pure ()

anyaddr = tupleToHostAddress (0, 0, 0, 0)

execute (Listen port k) = do
  let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just "0.0.0.0") (Just (show port))
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 50
  sockfd <- socketToFd sock
  setNonBlockingFD sockfd False
  apply k (show sockfd)

execute (Accept server_fd k) = do
  client_fd <- throwErrnoIfMinus1Retry "accept" $
    c_accept (fromIntegral server_fd) nullPtr nullPtr
  apply k (show client_fd)

execute (Write fd bytes k) = do
  BS.useAsCStringLen (BS.pack $ map fromIntegral bytes) $ \(ptr, len) ->
    fdWriteBuf (mkFd fd) (castPtr ptr) (fromIntegral len)
  pure k

execute (Read fd len k) = do
    bytes <- BS.createUptoN len $ \ptr ->
        fromIntegral <$> fdReadBuf (mkFd fd) ptr (fromIntegral len)
    apply k (showBytes bytes)

execute (Open filename k) = do
    let filenameBS = BS.pack (map fromIntegral filename)
    fd <- fromTry <$> try (openFd filenameBS ReadOnly Nothing defaultFileFlags)
    apply k (show fd)

    where
      fromTry (Right (Fd fd)) = Just fd
      fromTry (Left (SomeException _)) = Nothing

execute (Close fd k) = do
    closeFd (mkFd fd)
    pure k

showBytes bs = "'" ++ show (BS.unpack bs)

apply k result = pure $ "(" ++ k ++ ") $$ (" ++ result ++ ")"

mkFd = Fd . fromIntegral
unFd (Fd fd) = fd

foreign import ccall "accept" c_accept :: CInt -> Ptr () -> Ptr () -> IO CInt
