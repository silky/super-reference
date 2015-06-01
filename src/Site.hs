{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe (serveDirectory)
import           Application
------------------------------------------------------------------------------

type H = Handler App App ()


handleHomepage :: H
handleHomepage = error "Meh"


routes :: [(ByteString, H)]
routes = [ ("/home",           handleHomepage)
         , ("/search/:query",  undefined)
         -- | Serves static files like images, css, and js.
         , ("",                serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "super-reference!" Nothing $ do
    h <- nestSnaplet "" heist (heistInit "templates")
    addRoutes routes
    return $ App h

