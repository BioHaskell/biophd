module Bio.Sequence.Phd(
  Phd(..),
  readPhd, hReadPhd,
  readPhdTags) where

import Bio.Core.Sequence
import Bio.Sequence.PhdData
import Bio.Sequence.PhdTag

import Text.ParserCombinators.Parsec hiding (label)

import qualified Data.ByteString as BBB
import qualified Data.ByteString.Lazy as BB
import qualified Data.ByteString.Lazy.Char8 as B

import Data.Ix
import Data.Int (Int64)
import Data.Char (isSpace)
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Time.Format(TimeLocale, defaultTimeLocale, parseTimeOrError)
import Data.Binary (encode)
import System.IO
-- import System.Locale

-- | Parse a .phd file, extracting the contents as a PHD
readPhd :: FilePath -> IO Phd
readPhd f = return . mkPhd =<< readFile f

readPhdTags :: FilePath -> IO (Maybe [PhdTag])
readPhdTags f = return . mkPhdTags . lines =<< readFile f

-- | Parse .phd contents from a handle
hReadPhd :: Handle -> IO Phd
hReadPhd h = return . mkPhd =<< hGetContents h

-- | The actual phd parser.

mkPhd :: String -> Phd
mkPhd inp =
  let (hd:fs)                = filter (not . null) . lines $ inp
      (magic,label)          = splitAt 15 hd
      (comment'',seqAndTags) = break (== "BEGIN_DNA") fs
      comment'               = init $ tail comment''
      comment                = map ((tail . snd) . break (== ' ')) comment'
      (seq', tags')          = break (== "END_DNA") seqAndTags
      seq                    = drop 1 seq'
      tags                   = init $ tail tags'
      fields                 = words . unlines $ comment
      sdata                  = filter ((==3).length) . map words $ seq
      err                    = error "failed to parse quality value"
      dna                    = concatMap (!! 0) sdata
      qual                   = map ((\x -> read x :: Int) . (!! 1)) sdata
      traceInd               = map ((\x -> read x :: Int) . (!! 2)) sdata
  in if magic == "BEGIN_SEQUENCE " then
       Phd (mkComment  comment)
           (mkDNABlock label dna qual traceInd)
           (mkPhdTags tags)
     else
       error "Incorrectly formatted PHD file - missing BEGIN_SEQUENCE"
     --   Todo: also check that we have a BEGIN_DNA/END_DNA region there.

mkComment :: [String] -> Comment
mkComment com = Comment { chromatFile        = head com
                        , abiThumbprint      = com!!1
                        , phredVersion       = com!!2
                        , callMethod         = com!!3
                        , qualityLevels      = read (com!!4) :: Int
                        , time               = parseTimeOrError True defaultTimeLocale "%a %b %-e %T %Y" $ com!!5
                        , traceArrayMinIndex = read (com!!6) :: Int
                        , traceArrayMaxIndex = read (com!!7) :: Int
                        , trim               = if length com > 10 then
                                                 Just $ com!!8
                                               else
                                                 Nothing
                        , chem = (!!) com (if length com > 10 then 9 else 8)
                        , dye  = (!!) com (if length com > 10 then 10 else 9)
                        }

mkDNABlock :: String -> String -> [Int] -> [Int] -> DNABlock
mkDNABlock l d q t = DNABlock { label        = SeqLabel { unSL = B.pack l}
                              , bases        = SeqData  { unSD = B.pack d }
                              , qualities    = QualData { unQD = encode q }
                              , traceIndices = t }

mkPhdTags :: [String] -> Maybe [PhdTag]
mkPhdTags phdLines = case groupByTags phdLines of
                       [] -> Nothing
                       _  -> Just (map (fromJust . mkOnePhdTag)
                                       (groupByTags phdLines))

groupByTags :: [String] -> [[String]]
groupByTags [] = []
groupByTags xs =
  let begin_indices = elemIndices "BEGIN_TAG" xs
      end_indices   = elemIndices "END_TAG" xs
      tag_spans     = zip begin_indices end_indices
      grouping x    = drop (fst (tag_spans !! x)) (take (snd (tag_spans !! x) + 1) xs)
  in  map grouping (Data.Ix.range (0, length tag_spans - 1))

parseReadPosition :: String -> (Offset, Offset)
parseReadPosition line = (head positions, (head . tail) positions)
    where positions = map (\x -> Offset {unOff = read x :: Int64}) $ dropLabel line                      

dropLabel :: String -> [String]
dropLabel = tail . words

mkOnePhdTag :: [String] -> Maybe PhdTag
mkOnePhdTag td = case length td of
                   0 -> Nothing
                   9 -> Just PhdTag { tagType = head $ dropLabel (td!!1)
                                    , source  = head $ dropLabel (td!!2)
                                    , unpaddedReadPosition = parseReadPosition (td!!3)
                                    , date    = parseTimeOrError True defaultTimeLocale "%y/%d/%m %T" $ intercalate " " $ dropLabel (td!!4)
                                    , Bio.Sequence.PhdTag.comment = if td!!6 == "BEGIN_TAG" then Nothing
                                                                    else Just (td!!6)
                                    }
                   _ -> Just PhdTag { tagType = head $ dropLabel (td!!1)
                                    , source  = head $ dropLabel (td!!2)
                                    , unpaddedReadPosition = parseReadPosition (td!!3)
                                    , date    = parseTimeOrError True defaultTimeLocale "%y/%d/%m %T" $ intercalate " " $ dropLabel (td!!4)
                                    , Bio.Sequence.PhdTag.comment = Nothing
                                    }
