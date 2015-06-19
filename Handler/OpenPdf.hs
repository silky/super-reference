module Handler.OpenPdf where

import Import

import Control.Concurrent (forkIO)
import System.Process     (readProcess)

getOpenPdfR :: Text -> Handler Html
getOpenPdfR filePath = do
    -- TODO: Return some JSON here instead.
    defaultLayout $ do
        liftIO $ openPdf filePath
        [whamlet||]


-- | Just open the PDF locally; totally disregarding any
--   error codes.
openPdf :: Text -> IO ()
openPdf filePath = do
    -- Equivalent statement:
    -- _ <- forkIO $ readProcess "okular" [filename] "" >>= const (return ())
    _ <- forkIO $ do
        -- TOOD: Suppress stderr/stdout writing. Apparently "readProcess"
        -- captures and returns stderr, but this doesn't appear to be the
        -- case.
        _ <- readProcess "okular" [(unpack filePath)] ""
        return ()
    return ()
    
