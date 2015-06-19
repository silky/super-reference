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
import Data.Text                (splitOn)
import qualified GHC.List as    L

data Bib = Bib {
      _title     :: Text
    , _key       :: Text
    , _entryType :: Text
    , _author    :: Text
    , _filePath  :: Text
    , _url       :: Text
    , _year      :: Text
    }

-- | BibTeX-reading things ...

pageSize :: Int
pageSize = 200

pagesList :: Int -> [Int]
pagesList total = [1..div total pageSize + 1]

baseDir :: Text
baseDir = "/home/noon/research/library/"

normalise :: Text.BibTeX.Entry.T -> Bib
normalise (Cons entryType id fields) = Bib title (pack id) (pack entryType) author filePath url year
  where
      title    = findOrEmpty "title" fields
      author   = L.head (splitAuthors (findOrEmpty "author" fields))
      filePath = fullPath $ findOrEmpty "file" fields
      url      = findOrEmpty "url" fields
      year     = findOrEmpty "year" fields
      --
      -- | Get the full path to the file
      fullPath f = baseDir ++ L.head (splitOn ":" f)
      --
      -- | Split authors based on how we think the strings
      --   are formatted.
      splitAuthors s = splitOn " and " s


-- | In the given list, look for a specific string
--   and if we find it return that string, otherwise
--   return the empty string.
findOrEmpty :: Text -> [(String, String)] -> Text
findOrEmpty s xs = r
  where
      t = lookup (unpack s) xs
      r = case t of
                Just a  -> (pack a)
                Nothing -> ""


-- | Read in the list of BibTeX entries.
bibEntries :: IO [Text.BibTeX.Entry.T]
bibEntries = do
      result  <- parseFromFile file "quant.bib"
      entries <- case result of
                     Left  _  -> error (show result)
                     Right xs -> return (map lowerCaseFieldNames xs)
      return entries


-- | Search the title case-insensitively.
search :: Text -> [Bib] -> [Bib]
search str bs =
    filter (\b -> (toLower str) `isInfixOf` (toLower (_title b))) bs


getPagedHomeR :: Int -> Handler Html
getPagedHomeR k = do
    --
    -- | Form things
    ((result, formWidget), formEncType) <- runFormGet searchForm
    let searchString = case result of
             FormSuccess res -> Just res
             _               -> Nothing
    defaultLayout $ do
        --
        -- | Obtain BibTeX data
        bs <- liftIO bibEntries
        let bibs        = case searchString of
                            -- | Filtered
                            Just s -> search s (map normalise bs)
                            --
                            -- | Everything
                            _      -> map normalise bs
            entries     = take pageSize (drop ((k-1) * pageSize) bibs)
            numEntries  = length bibs
            pages       = pagesList numEntries
        --
        -- | Render
        setTitle "super-reference!"
        $(widgetFile "homepage")


getHomeR :: Handler Html
getHomeR = getPagedHomeR 1


-- postHomeR :: Handler Html
-- postHomeR = do
--     ((result, formWidget), formEnctype) <- runFormPost searchForm
--     let submission = case result of
--             FormSuccess res -> Just res
--             _ -> Nothing
--     defaultLayout $ do
--         --
--         -- | Obtain BibTeX data
--         bs <- liftIO bibEntries
--         let bibs        = map normalise bs
--             entries     = take pageSize (drop ((k-1) * pageSize) bibs)
--             numEntries  = length bibs
--             pages       = pagesList numEntries
--         --
--         -- | Render
--         setTitle "super-reference!"
--         $(widgetFile "homepage")
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


searchForm :: Form Text
searchForm = renderBootstrap3 BootstrapBasicForm $
    areq textField (withSmallInput "Search") Nothing
