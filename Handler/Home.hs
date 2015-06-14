module Handler.Home where

import Import
import Yesod.Form.Bootstrap3    ( BootstrapFormLayout (..)
                                , renderBootstrap3
                                , withSmallInput
                                )

import Text.Parsec.String       (parseFromFile)
import Text.BibTeX.Parse        (file)
import Text.BibTeX.Entry        (T (..)
                                , lowerCaseFieldNames)
import Data.List.Split          (splitOn)
import qualified GHC.List as    L

data Bib = Bib {
      _title     :: String
    , _key       :: String
    , _entryType :: String
    , _author    :: String
    , _file      :: String
    , _url       :: String
    , _year      :: String
    }

normalise :: Text.BibTeX.Entry.T -> Bib
normalise (Cons entryType id fields) = Bib title id entryType author file url year
  where
      title   = findOrEmpty "title" fields
      author  = L.head (splitAuthors (findOrEmpty "author" fields))
      file    = findOrEmpty "file" fields
      url     = findOrEmpty "url" fields
      year    = findOrEmpty "year" fields


-- | Split authors based on how we think the strings
--   are formatted.
splitAuthors :: String -> [String]
splitAuthors s = splitOn " and " s


-- | In the given list, look for a specific string
--   and if we find it return that string, otherwise
--   return the empty string.
findOrEmpty :: String -> [(String, String)] -> String
findOrEmpty s xs = r
  where
      t = lookup s xs
      r = case t of
                Just a  -> a
                Nothing -> ""


getHomeR :: Handler Html
getHomeR = do 
    defaultLayout $ do
        bs <- liftIO bibEntries
        let entries = map normalise bs
        aDomId <- newIdent
        setTitle "super-reference!"
        $(widgetFile "homepage")


testBibEntries :: [Text.BibTeX.Entry.T]
testBibEntries = [
      Cons "article" "nsilk2010" [("title", "Something")]
    , Cons "article" "nsilk2015" [("title", "Another thing")]
    , Cons "article" "emptyTitle" [("abstract", "Another thing")]
    ]


bibEntries :: IO [Text.BibTeX.Entry.T]
bibEntries = do
      result  <- parseFromFile file "quant.bib"
      entries <- case result of
                     Left  _  -> error (show result) -- $return []
                     Right xs -> return (map lowerCaseFieldNames xs)
      return entries


postHomeR :: Handler Html
postHomeR = error "Not implemented."
-- postHomeR = do
--     ((result, formWidget), formEnctype) <- runFormPost sampleForm
--     let handlerName = "postHomeR" :: Text
--         msg = "ok" :: Text-- bibEntries
--         entries = bibEntries
--         submission = case result of
--             FormSuccess res -> Just res
--             _ -> Nothing
--     defaultLayout $ do
--         aDomId <- newIdent
--         setTitle "Welcome To Yesod!"
--         $(widgetFile "homepage")


sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing
