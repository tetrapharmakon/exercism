module ProteinTranslation(proteins) where

import Data.List.Split

trans :: String -> String
trans s
  | s == "AUG"                            = "Methionine"
  | s `elem` ["UUU", "UUC"]               = "Phenylalanine"
  | s `elem` ["UUA", "UUG"]               = "Leucine"
  | s `elem` ["UCU", "UCC", "UCA", "UCG"] = "Serine"
  | s `elem` ["UAU", "UAC"]               = "Tyrosine"
  | s `elem` ["UGU", "UGC"]               = "Cysteine"
  | s == "UGG"                            = "Tryptophan"
  | s `elem` ["UAA", "UAG", "UGA"]        = "STOP"
  | otherwise                             = undefined

proteins :: String -> Maybe [String]
proteins s = Just $ takeWhile (/="STOP") $ map trans (chunksOf 3 s)