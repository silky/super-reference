module Handler.About where

import Import

getAboutR :: Handler Html
getAboutR = defaultLayout $(widgetFile "about")
-- getAboutR = error "Not yet implemented: getAboutR"
