module Bio.Sequence.PhdTag where 

import Bio.Core.Sequence (Offset, unOff)

data PhdTag = PhdTag
              { tagType              :: String
              , source               :: String
              , unpaddedReadPosition :: [Offset]
              , date                 :: String
              , comment              :: String  
              } deriving (Eq, Show)

instance Show PhdTag where
  show (PhdTag tt so urp da co) = ("\n" ++) $ unlines $ map (" " ++)
                                  [ "TYPE: "              ++ show tt
                                  , "SOURCE: "            ++ show so
                                  , "UNPADDED_READ_POS: " ++ show (map unOff urp)
                                  , "DATE: "              ++ show da
                                  , "COMMENT: "           ++ show co ]
