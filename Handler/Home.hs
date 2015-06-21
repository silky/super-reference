module Handler.Home where

import Import
import Yesod.Form.Bootstrap3    ( BootstrapFormLayout (..)
                                , renderBootstrap3
                                , withSmallInput
                                )

import qualified Text.BibTeX.Entry as BibTeX
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


stripChars :: Text -> Text -> Text
stripChars = filter . flip onotElem


pagesList :: Int -> [Int]
pagesList total = [1..div total pageSize + 1]


baseDir :: Text
baseDir = "/home/noon/research/library/"


-- | TODO: Rename for "forDisplay" or something.
normalise :: BibTeX.T -> Bib
normalise (BibTeX.Cons entryType id fields) = Bib title (pack id) (pack entryType) author filePath url year
  where
      -- | Note: Might be a poor choice.
      title    = stripChars "{}" (findOrEmpty "title" fields)
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


-- | Search the title case-insensitively.
search :: Text -> [Bib] -> [Bib]
search str bs =
    filter (\b -> (toLower str) `isInfixOf` (toLower (_title b))) bs


getPagedHomeR :: Int -> Handler Html
getPagedHomeR k = do
    yesod <- getYesod
    --
    -- | Form things
    ((result, formWidget), formEncType) <- runFormGet searchForm
    let searchString = case result of
             FormSuccess res -> Just res
             _               -> Nothing
    defaultLayout $ do
        --
        -- | Obtain BibTeX data
        let bs = bibtexDb yesod
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


searchForm :: Form Text
searchForm = renderBootstrap3 BootstrapBasicForm $
    areq textField (withSmallInput "Search") Nothing
