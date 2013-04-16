module Bio.Sequence.PhdTag where 
import System.Locale
import Data.Time
import Data.Time.Format
import Bio.Core.Sequence (Offset, unOff)

data PhdTag = PhdTag
              { tagType              :: String
              , source               :: String
              , unpaddedReadPosition :: [Offset]
              , date                 :: LocalTime
              , comment              :: Maybe String
              } deriving (Eq)

instance Show PhdTag where
  show (PhdTag tt so urp da co) = ("\n" ++) $ unlines $ map (" " ++)
                                  [ "TYPE: "              ++ show tt
                                  , "SOURCE: "            ++ show so
                                  , "UNPADDED_READ_POS: " ++ show (map unOff urp)
                                  , "DATE: "              ++ show da
                                  , "COMMENT: "           ++ show co ]

defaultPhdTag = PhdTag { tagType              = "polymorphism"
                       , source               = "polyphred"
                       , unpaddedReadPosition = [5, 5]
                       , date                 = readTime defaultTimeLocale "%D %T" "09/22/11 15:17:47"
                       , comment              = Nothing }
