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
import Network.HTTP.Types.URI   (urlEncode)

data Bib = Bib {
      _title     :: String
    , _key       :: String
    , _entryType :: String
    , _author    :: String
    , _filePath  :: String
    , _url       :: String
    , _year      :: String
    }

pageSize :: Int
pageSize = 200

baseDir :: String
baseDir = "/home/noon/research/library/"


normalise :: Text.BibTeX.Entry.T -> Bib
normalise (Cons entryType id fields) = Bib title id entryType author filePath url year
  where
      title    = findOrEmpty "title" fields
      author   = L.head (splitAuthors (findOrEmpty "author" fields))
      filePath = fullPath $ findOrEmpty "file" fields
      url      = findOrEmpty "url" fields
      year     = findOrEmpty "year" fields

fullPath :: String -> String
fullPath f = baseDir ++ L.head (splitOn ":" f)


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


getPagedHomeR :: Int -> Handler Html
getPagedHomeR k = do
    defaultLayout $ do
        bs <- liftIO bibEntries
        let bibs = map normalise bs
        let entries = take pageSize (drop ((k-1) * pageSize) bibs)
        let numEntries = length bibs
        aDomId <- newIdent
        setTitle "super-reference!"
        $(widgetFile "homepage")


getHomeR :: Handler Html
getHomeR = getPagedHomeR 1

bibEntries :: IO [Text.BibTeX.Entry.T]
bibEntries = do
      result  <- parseFromFile file "quant.bib"
      entries <- case result of
                     Left  _  -> error (show result)
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
