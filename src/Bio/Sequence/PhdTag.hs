module Bio.Sequence.PhdTag where 

import Data.Time.LocalTime(LocalTime)
import Bio.Core.Sequence (Offset, unOff)

data PhdTag = PhdTag
              { tagType              :: String
              , source               :: String
              , unpaddedReadPosition :: (Offset, Offset)
              , date                 :: LocalTime
              , comment              :: Maybe String
              } deriving (Eq)

instance Show PhdTag where
  show (PhdTag tt so urp da co) = ("\n" ++) $ unlines $ map (" " ++)
                                  [ "TYPE: "              ++ show tt
                                  , "SOURCE: "            ++ show so
                                  , "UNPADDED_READ_POS: " ++ (show $ unOff $ fst urp) ++ " " ++ (show $ unOff $ snd urp)
                                  , "DATE: "              ++ show da
                                  , "COMMENT: "           ++ show co ]

