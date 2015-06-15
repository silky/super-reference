module Handler.OpenPdf where

import Import

import Control.Concurrent (forkIO)
import System.Process     (readProcess)

getOpenPdfR :: String -> Handler Html
getOpenPdfR s = do
    -- TODO: Find the filename from the bibtex key.
    -- TODO: Return some JSON here instead.
    defaultLayout $ do
        liftIO $ openPdf "/home/noon/t.pdf"
        [whamlet||]


-- | Just open the PDF locally; totally disregarding any
--   error codes.
openPdf :: String -> IO ()
openPdf filename = do
    -- Equivalent statement:
    -- _ <- forkIO $ readProcess "okular" [filename] "" >>= const (return ())
    _ <- forkIO $ do
        -- TOOD: Suppress stderr/stdout writing.
        _ <- readProcess "okular" [filename] ""
        return ()
    return ()
    
