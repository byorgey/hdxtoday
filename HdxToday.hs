{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- from 'sandi'
import qualified Codec.Binary.QuotedPrintable as QP
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import           Text.StringLike

import qualified Data.ByteString              as BS
import           Data.List                    (intercalate)
import           Data.Time
import           Data.Time.Calendar

import           System.Environment

data Item = Item { itemTitle :: String, itemDescription :: String }
  deriving Show

data Seen = Seen { seenTitle :: String, lastSeen :: Day }

mkItem :: [BS.ByteString] -> Item
mkItem (title:rest) =
  Item (toString title) (concat . map toString $ rest)

getItems :: FilePath -> IO ([Item], Maybe Day)
getItems fn = do
  raw <- BS.readFile fn
  case QP.decode raw of
    Left (parsed, leftovers) -> error "can't parse"
    Right parsed -> do
      let tags = parseTags parsed
          items = map mkItem
                . (map . map) fromTagText
                . filter ((>1) . length)
                . takeWhile ((~/= tt "MENU") . head)
                . map (filter (~/= tt "\160"))
                . map (filter (~/= tt "\r\n"))
                . map (filter (~== tt ""))
                . filter ((~/= tt "|") . (!!1))
                . partitions (~== to "b")
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
      print mday
      putStr . unlines . map itemTitle $ items

