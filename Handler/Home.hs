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
    , _abstract  :: Text
    }


-- TODO: Use lenses here.
data SearchTerms = SearchTerms {
      _stTitle      :: Text
    , _stEntryType  :: Maybe Text
}

-- | Homepage is paged homepage at page 1.
getHomeR :: Handler Html
getHomeR = getPagedHomeR 1


-- | Show whatever page we've been asked to, and build up all
--   the data we want to display.
getPagedHomeR :: Int -> Handler Html
getPagedHomeR k = do
    yesod <- getYesod
    req   <- getRequest
    let queryString = intercalate "&" $ map (\(s, v) -> concat [s, "=", v]) (reqGetParams req)
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
        let bs = zip [1..] bibDb
            (rawS, rawU) = partition (\(_, b) -> isStarred b) bs
            bibS = map (uncurry normalise) rawS
            bibU = map (uncurry normalise) rawU
        --
        let bibs        = case searchString of
                            -- | Filtered
                            Just s -> search (searchTerms s) bibU
                            --
                            -- | Everything
                            _      -> bibU
            entries     = take pageSize (drop ((k-1) * pageSize) bibs)
            numEntries  = length bibs
            pages       = pagesList numEntries
        --
        -- | Render
        setTitle "super-reference!"
        $(widgetFile "homepage")



-- | Here we take the index in the "bibtexDb" thing, and we write
--   a value into it.
getStarR :: Int -> Handler Value
getStarR idx = do
  yesod <- getYesod
  bs <- liftIO $ readIORef $ bibtexDb yesod
  --
  -- Update the database
  let bs' = bs & element (idx-1) .~ toggleStarred (bs !! (idx - 1))
  --
  -- Write it, but don't wait, because we'll risk it.
  liftIO $ do
    _ <- forkIO $ BibTeX.writeToFile "out.bib" bs'
    writeIORef (bibtexDb yesod) bs'
    return ()
  --
  return $ object ["success" .= True]


searchForm :: Form Text
searchForm = renderBootstrap3 BootstrapBasicForm $
    areq textField (withSmallInput "Search") Nothing


-- =====================================================================
-- BibTeX-reading things ...


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
normalise idx (BibTeX.Cons entryType ident fields) =
      Bib idx title (pack ident) (pack entryType) author filePath url year starred abstract
  where
      title    = stripChars "{}" (findOrEmpty "title" fields)
      author   = L.head (splitAuthors (findOrEmpty "author" fields))
      filePath = fullPath $ findOrEmpty "file" fields
      url      = findOrEmpty "url" fields
      year     = findOrEmpty "year" fields
      starred  = fromMaybe "" (lookup starKey fields) == "starred"
      -- Note: We're not using this at the moment.
      abstract = findOrEmpty "abstract" fields
      --
      -- | Get the full path to the file
      fullPath f = baseDir ++ L.head (splitOn ":" f)
      --
      -- | Split authors based on how we think the strings
      --   are formatted.
      splitAuthors = splitOn " and "
      --
      -- | Find the thing in the list, otherwise return the empty string.
      findOrEmpty :: Text -> [(String, String)] -> Text
      findOrEmpty s xs = pack $ fromMaybe "" (lookup (unpack s) xs)


searchTerms :: Text -> SearchTerms
searchTerms s = SearchTerms (unwords terms) articleType
  where
    spaces = splitOn " " s
    tokens = map (splitOn ":") spaces
    --
    -- Parse special terms
    f ["type", val] (xs, _) = (xs, Just val)
    f [val]         (xs, t) = (val : xs, t)
    f _             (_,  t) = error $ "Invalid token: `" ++ show t ++ "`."
    --
    (terms, articleType) = foldr f ([], Nothing) tokens


-- | Filter based on the search terms.
search :: SearchTerms -> [Bib] -> [Bib]
search = go
  where
    go (SearchTerms title Nothing)  = filter (f title)
    go (SearchTerms title (Just t)) = filter (\b -> f title b && g t b)
    -- title filter
    f title b     = toLower title `isInfixOf` toLower (_title b)
    -- type filter
    g entryType b = toLower entryType == toLower (_entryType b)


isStarred :: BibTeX.T -> Bool
isStarred (BibTeX.Cons _ _ fields) =
  fromMaybe "" (lookup starKey fields) == "starred"


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
