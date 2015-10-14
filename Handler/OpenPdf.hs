module Handler.OpenPdf where

import Import

import Control.Concurrent (forkIO)
import System.Process     (readProcess)


getOpenPdfR :: FilePath -> Handler Value
getOpenPdfR filePath = do
  liftIO $ openPdf filePath
  return $ object ["success" .= True]


-- | Just open the PDF locally; totally disregarding any
--   error codes.
openPdf :: FilePath -> IO ()
openPdf filePath = do
    _ <- forkIO $ do
        -- TOOD: Suppress stderr/stdout writing. Apparently "readProcess"
        -- captures and returns stderr, but this doesn't appear to be the
        -- case.
        _ <- readProcess "xdg-open" [filePath] ""
        return ()
    return ()
