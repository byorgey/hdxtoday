{-# LANGUAGE LambdaCase #-}

-- from 'sandi'
import qualified Codec.Binary.QuotedPrintable as QP
import           Text.HTML.TagSoup
import           Text.StringLike

import qualified Data.ByteString              as BS
import           Data.List                    (intercalate)

import           System.Environment

data Item = Item { itemTitle :: String, itemDescription :: String }
  deriving Show

mkItem :: StringLike str => [str] -> Item
mkItem (title:rest) =
  Item (toString title) (concat . map toString $ rest)

getItems :: FilePath -> IO [Item]
getItems fn = do
  raw <- BS.readFile fn
  case QP.decode raw of
    Left (parsed, leftovers) -> error "can't parse"
    Right parsed -> do
      let tags = parseTags parsed
          items = map mkItem
                . (map . map) fromTagText
                . filter ((>1) . length)
                . takeWhile ((~/= TagText "MENU") . head)
                . map (filter (~/= TagText "\160"))
                . map (filter (~/= TagText "\r\n"))
                . map (filter (~== TagText ""))
                . filter ((~/= TagText "|") . (!!1))
                . partitions (~== TagOpen "b" [])
                $ tags
      return items

main = do
  args <- getArgs
  case args of
    []     -> error "no file name"  -- read from stdin
    (fn:_) -> do
      items <- getItems fn
      putStr . unlines . map itemTitle $ items

