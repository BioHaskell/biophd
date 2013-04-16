module Bio.Sequence.PhdTag where 

import Bio.Core.Sequence (Offset, unOff)

data PhdTag = PhdTag
              { tagType              :: String
              , source               :: String
              , unpaddedReadPosition :: [Offset]
              , date                 :: String
              , comment              :: String  
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
                       , date                 = "01/01/70 00:00:00"
                       , comment              = "" }
