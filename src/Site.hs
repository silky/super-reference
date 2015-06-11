{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
-- import           Data.ByteString (ByteString)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe (serveDirectory)
import           Application (H
                    , doSearch
                )
import           Snap.Core (getParam)
------------------------------------------------------------------------------

handleHomepage :: H
handleHomepage = error "Meh"


searchHandler :: H
searchHandler = do
    q <- getParam "query"
    case q of
         Nothing -> handleHomepage
         Just q' -> doSearch q'


routes :: [(B.ByteString, H)]
routes = [ ("/home",           handleHomepage)
         , ("/search/:query",  searchHandler)
         -- | Serves static files like images, css, and js.
         , ("",                serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "super-reference!" Nothing $ do
    h <- nestSnaplet "heist" heist (heistInit "templates")
    addRoutes routes
    return $ App h

