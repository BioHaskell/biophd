module Bio.Sequence.PhdData where

import Data.Maybe(fromJust)
import Bio.Core.Sequence
import qualified Data.ByteString.Lazy as BB
import qualified Data.ByteString.Lazy.Char8 as B

{-- A .phd file consists of a DNA block with base and quality
    values, followed by one or more (optional) tag blocks. --}
data Phd = Phd { comment  :: Comment
               , dnaBlock :: DNABlock
               , phdTags  :: Maybe [PhdTag]
               } deriving (Show)

{-- These types are subject to change if it improves functionality,
    but for now it's simplest to just call them String, Int etc.--}
data Comment = Comment
    { chromatFile        :: FilePath
    , abiThumbprint      :: String
    , phredVersion       :: String
    , callMethod         :: String
    , qualityLevels      :: Int
    , time               :: String
    , traceArrayMinIndex :: Int
    , traceArrayMaxIndex :: Int
    , trim               :: Maybe String
    , chem               :: String
    , dye                :: String
    } deriving (Show, Eq)

{--
instance Show Comment where
    show (Comment cf abit pv cm ql ti mintai maxtai tr ch dy) =
      ("\n" ++) $ unlines
                    [ "CHROMAT_FILE: "   ++ cf
                    , "ABI_THUMBPRINT: " ++ abit
                    , "PHRED_VERSION: "  ++ pv
                    , "CALL_METHOD: "    ++ cm
                    , "QUALITY_LEVELS: " ++ show ql
                    , "TIME: "           ++ show ti
                    , "TRACE_ARRAY_MIN_INDEX: " ++ show mintai
                    , "TRACE_ARRAY_MAX_INDEX: " ++ show maxtai
                    , "TRIM: "           ++ if (tr == Nothing) then "" else fromJust tr
                    , "CHEM: "           ++ ch
                    , "DYE: "            ++ dy
                    ]
--}

data DNABlock = DNABlock
                { label        :: SeqLabel
                , bases        :: SeqData
                , qualities    :: QualData
                , traceIndices :: [Int]
                } deriving (Show, Eq)

{--
instance Show DNABlock where
  show = B.unpack . toFasta
--}

instance BioSeq DNABlock where
  seqid     db = SeqLabel $ B.takeWhile (/= ' ') $ unSL $ label db
  seqheader    = label
  seqdata      = bases
  seqlength db = Offset $ B.length $ unSD $ bases db

instance BioSeqQual DNABlock where
  seqqual = qualities

data PhdTag  = PhdTag
               { tagType :: String
               , source  :: String
               , unpaddedReadPosition :: [Offset]
               , date    :: String
               , tagComment :: String
               } deriving (Show, Eq)

-- Some default values for the data types, useful for debugging in ghci

defaultComment = Comment { chromatFile        = "seq.ab1"
                         , abiThumbprint      = "0"
                         , phredVersion       = "0.980904.e"
                         , callMethod         = "phred"
                         , qualityLevels      = 99
                         , time               = ""
                         , traceArrayMinIndex = 0
                         , traceArrayMaxIndex = 1
                         , trim               = Nothing
                         , chem               = "unknown"
                         , dye                = "unknown" }

defaultDNABlock = DNABlock { label        = SeqLabel $ B.pack "some_dna A sample sequence."
                           , bases        = SeqData  $ B.pack "aatgcatcta"
                           , qualities    = QualData $ B.pack "0000000000"
                           , traceIndices = [0,1,2,3,4,5,6,7,8,9,10] }

defaultPhdTag = PhdTag { tagType              = "polymorphism"
                       , source               = "polyphred"
                       , unpaddedReadPosition = [5, 5]
                       , date                 = "01/01/70 00:00:00"
                       , tagComment              = "" }
