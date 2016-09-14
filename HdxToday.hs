{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- from 'sandi'
import qualified Codec.Binary.QuotedPrintable as QP
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import           Text.StringLike

import           Codec.MIME.Parse
import           Codec.MIME.Type

import           Control.Monad                (when)
import qualified Data.ByteString              as BS
import           Data.List                    (find, intercalate)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Data.Text.IO                 as TIO
import           Data.Time
import           Data.Time.Calendar
import           System.IO                    (hFlush, stdout)
import           Text.Printf                  (printf)
import           Text.Read                    (readMaybe)

import           System.Environment

data Item = Item { itemTitle :: String, itemDescription :: String }
  deriving (Show, Read)

data Seen = Seen { seenTitle :: String, lastSeen :: Day }
  deriving (Show, Read)

mimeHTML :: MIMEValue -> [T.Text]
mimeHTML (MIMEValue (Type (Text "html") _) _ (Single c) _ _) = [c]
mimeHTML (MIMEValue _ _ (Multi vs) _ _) = concatMap mimeHTML vs
mimeHTML _ = []

mkItem :: [BS.ByteString] -> Item
mkItem (title:rest) =
  Item (toString title) (concat . map toString $ rest)

getItems :: FilePath -> IO ([Item], Maybe Day)
getItems fn = do
  raw <- TIO.readFile fn
  let [html] = mimeHTML $ parseMIMEMessage raw
  case QP.decode (TE.encodeUtf8 html) of
    Left (parsed, leftovers) -> error "can't parse"
    Right parsed -> do
      let tags = parseTags parsed
          items = map mkItem
                . (map . map) fromTagText
                . map (filter (~== TagText ("" :: BS.ByteString)))
                . partitions (~== to "h2")
                . takeWhile (~/= tt "MENU")
                . concat
                . map dropEmptyH2
                . map (filter (~/= tc "u"))
                . map (filter (~/= to "u"))
                . map (filter (~/= tt "\r\n"))
                . map (filter (~/= tt "\194\160"))
                . partitions (~== to "h2")
                $ tags
          mday = parseTimeM True defaultTimeLocale "%A, %B %-e, %Y"
               . toString
               . fromTagText
               . head . filter (tagText hasMonthName) $ tags
      return (items, mday)

tt :: BS.ByteString -> Tag BS.ByteString
tt = TagText

to :: BS.ByteString -> Tag BS.ByteString
to s = TagOpen s []

tc :: BS.ByteString -> Tag BS.ByteString
tc = TagClose

dropEmptyH2 :: StringLike str => [Tag str] -> [Tag str]
dropEmptyH2 (a:b:cs)
  | a ~== to "h2" && b ~== tc "h2"  = cs
dropEmptyH2 tags = tags

hasMonthName :: BS.ByteString -> Bool
hasMonthName s = any (\m -> m `BS.isInfixOf` s) monthNames

monthNames :: [BS.ByteString]
monthNames
  = ["January", "February", "March", "April", "May", "June"
    , "July", "August", "September", "October", "November", "December"
    ]

main = do
  args <- getArgs
  case args of
    []     -> error "no file name"  -- read from stdin
    (fn:_) -> do
      (items, mday) <- getItems fn
      case mday of
        Nothing -> putStrLn "Can't parse date, XXX use today?"
        Just day -> do
          seenDat <- readFile "seen.dat"
          let seen :: [Seen]
              seen = read seenDat
              newItems = filter (isNew day seen) items
          browseItems newItems
          let seen' = updateSeen day items seen
          writeFile "seen.dat" (show seen')

isNew :: Day -> [Seen] -> Item -> Bool
isNew day seen item
  = case find ((== itemTitle item) . seenTitle) seen of
      Nothing -> True
      Just s  -> diffDays day (lastSeen s) >= 7

updateSeen :: Day -> [Item] -> [Seen] -> [Seen]
updateSeen day items seen = map updateOne seen ++ newlySeen
  where
    updateOne s@(Seen t l)
      | any ((==t) . itemTitle) items = Seen t day
      | otherwise                     = s
    newlySeen
      = map mkSeen
      $ filter ((`notElem` (map seenTitle seen)) . itemTitle) items
    mkSeen (Item t _) = Seen t day

browseItems :: [Item] -> IO ()
browseItems items = go
  where
    go = printItems >> cmdLoop
    cmdLoop = do
      putStr "> "
      hFlush stdout
      input <- getLine
      when (input /= "q") $ do
        case (readMaybe input :: Maybe Int) of
          Just n -> when (n < length items)
                         (printItem (items !! n) >> cmdLoop)
          Nothing -> go
    printItems
      = putStr . unlines
      . map (uncurry (printf "(%02d) %s"))
      . zip [0 :: Int ..] . map itemTitle
      $ items
    printItem (Item t d)
      = putStrLn t >> putStrLn d
