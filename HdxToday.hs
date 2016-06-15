-- from 'sandi'
import qualified Codec.Binary.QuotedPrintable as QP
import Text.HTML.TagSoup

import qualified Data.ByteString              as BS

main = do
  args <- getArgs
  case args of
    []     -> error "no file name"  -- read from stdin
    (fn:_) -> do
      raw <- BS.readFile fn

      case QP.decode raw of
        Left (parsed, leftovers) -> error "can't parse"
        Right parsed -> do
          let tags = parseTags parsed
              ps   = partitions (~== TagOpen "b" []) tags

              -- throw away any <b> tag immediately followed by | char

              -- After that, <b> tags will be either titles of sections
              --   or titles of entries

              -- After each entry title look for next <p> (throw it
              -- away) and then text inside the <p> after that.

