module Handler.Home where

import           Import
import           Yesod.Form.Bootstrap3 ( BootstrapFormLayout (..)
                                , renderBootstrap3
                                , withSmallInput
                                )
import           Data.Text (splitOn)
import qualified GHC.List          as L
import qualified Text.BibTeX.Entry as BibTeX
import           Control.Lens (element, (&), (.~))
import           Data.List ((!!))
import           Data.Aeson (object, (.=))
import           Control.Concurrent (forkIO)

data Bib = Bib {
      _index     :: Int
    , _title     :: Text
    , _key       :: Text
    , _entryType :: Text
    , _author    :: Text
    , _filePath  :: Text
    , _url       :: Text
    , _year      :: Text
    , _starred   :: Bool
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
normalise :: Int -> BibTeX.T -> Bib
normalise idx (BibTeX.Cons entryType id fields) =
      Bib idx title (pack id) (pack entryType) author filePath url year starred
  where
      -- | Note: Might be a poor choice.
      title    = stripChars "{}" (findOrEmpty "title" fields)
      author   = L.head (splitAuthors (findOrEmpty "author" fields))
      filePath = fullPath $ findOrEmpty "file" fields
      url      = findOrEmpty "url" fields
      year     = findOrEmpty "year" fields
      starred  = fromMaybe "" (lookup starKey fields) == "starred"
      --
      -- | Get the full path to the file
      fullPath f = baseDir ++ L.head (splitOn ":" f)
      --
      -- | Split authors based on how we think the strings
      --   are formatted.
      splitAuthors = splitOn " and "


-- | In the given list, look for a specific string
--   and if we find it return that string, otherwise
--   return the empty string.
findOrEmpty :: Text -> [(String, String)] -> Text
findOrEmpty s xs = r
  where
      t = lookup (unpack s) xs
      r = case t of
                Just a  -> pack a
                Nothing -> ""


-- | Search the title case-insensitively.
search :: Text -> [Bib] -> [Bib]
search str =
    filter (\b -> toLower str `isInfixOf` toLower (_title b))


getPagedHomeR :: Int -> Handler Html
getPagedHomeR k = do
    yesod <- getYesod
    --
    -- | Form things
    ((result, formWidget), formEncType) <- runFormGet searchForm

    -- This is magic right here.
    let listEntries entries = $(widgetFile "listEntries")
    
    let searchString = case result of
             FormSuccess res -> Just res
             _               -> Nothing
    bibDb <- liftIO $ readIORef (bibtexDb yesod)
    defaultLayout $ do
        --
        -- | Obtain BibTeX data
        --

        let bs   = zip [1..] bibDb
        let rawS = filter (\(_, b) -> isStarred b) bs
        let rawU = filter (\(_, b) -> (not . isStarred) b) bs
        let bibS = map (uncurry normalise) rawS
        let bibU = map (uncurry normalise) rawU
        --
        let bibs        = case searchString of
                            -- | Filtered
                            Just s -> search s bibU
                            --
                            -- | Everything
                            _      -> bibU
            entries     = take pageSize (drop ((k-1) * pageSize) bibs)
            numEntries  = length bibs
            pages       = pagesList numEntries
        --
        -- | Starred
        let starredBibs = bibS
        -- | Render
        setTitle "super-reference!"
        $(widgetFile "homepage")


getHomeR :: Handler Html
getHomeR = getPagedHomeR 1


-- | Here we take the index in the "bibtexDb" thing, and we write
--   a value into it.
getStarR :: Int -> Handler Value
getStarR idx = do
  yesod <- getYesod
  bs <- liftIO $ readIORef $ bibtexDb yesod
  -- Update the database -- the one that was starred, change it
  -- so that it isn't.
  let bs' = bs & element (idx-1) .~ toggleStarred (bs !! (idx - 1))
  --
  -- Write it, but don't wait, because we'll risk it.
  liftIO $ do
    _ <- forkIO $ BibTeX.writeToFile "out.bib" bs'
    writeIORef (bibtexDb yesod) bs'
    return ()
  --
  -- and save it in the IORef
  --
  return $ object ["success" .= True]


isStarred :: BibTeX.T -> Bool
isStarred (BibTeX.Cons _ _ fields) = case lookup starKey fields of
  Just x -> x == "starred"
  _      -> False


-- | Note: This is lowercase, because we only deal with lower case.
starKey :: String
starKey = "superreference:starred"


-- | For our special field "superReference_starred", let's either
--   mark a thing as starred or unstarred.
toggleStarred :: BibTeX.T -> BibTeX.T
toggleStarred (BibTeX.Cons x y fields) = BibTeX.Cons x y fields'
  where
    fields'  = toggle starKey fields
    toggle k = insertWithKey (\_ _ old -> case old of
                                  "starred" -> "unstarred"
                                  _         -> "starred") k "starred"


searchForm :: Form Text
searchForm = renderBootstrap3 BootstrapBasicForm $
    areq textField (withSmallInput "Search") Nothing
