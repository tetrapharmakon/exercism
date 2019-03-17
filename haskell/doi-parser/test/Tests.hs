{-# LANGUAGE RecordWildCards #-}

import           Data.Foldable     (for_)
import           Test.Hspec        (Spec, describe, it, shouldBe)
import           Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import           Doi

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "DOI code" $ for_ cases test
  where
    test Case{..} = it description $ doi input `shouldBe` expected

data Case = Case { description ::       String
                 , input       ::       String
                 , expected    :: Maybe [String]
                 }

cases :: [Case]
cases =
    [
      Case
      { description = "invalid if DOI name does not start with 10."
      , input       = "4.10000"
      , expected    = Nothing
      }
      Case
      { description = "invalid if no dots."
      , input       = "10-10000-10-1"
      , expected    = Nothing
      }
      Case
      { description = "invalid if alien characters."
      , input       = "10.100@!0.15480ç*-1"
      , expected    = Nothing
      }
      Case 
      { description = "invalid if empty after a dot."
      , input       = "10."
      , expected    = Nothing
      }
      Case 
      { description = "invalid if empty spaces separated by dots."
      , input       = "10...."
      , expected    = Nothing
      }
      Case 
      { description = "valid if DOI code has dot-separated letters"
      , input      = "10.AJPH.2009.160184"
      , expected   = Just ["10", "AJPH", "2009", "160184"]
      }
      Case 
      { description = "valid if DOI code contains /"
      , input       = "10.2105/AJPH.2009.160184"
      , expected    = Just ["10", "2105", "/", "AJPH", "2009", "160184"]
      }
      Case 
      { description = "valid if very long"
      , input       = "10.1145/27834278360546.27827834463605"
      , expected    = Just ["10", "1145", "/", "27834278360546", "27827834463605"]
      }
      Case 
      { description = "valid if lowercase letters"
      , input       = "10.1111/j.1600-0404.1986.tb04634.x"
      , expected    = Just ["10", "1111", "j", "1600-0404", "1986", "tb04634", "x"]
      }
      Case 
      { description = "valid if DOI url, but returns no url"
      , input       = "http://dx.doi.org/10.1392.x"
      , expected    = Just ["10", "1392", "x"]
      }
      Case 
      { description = "valid if DOI url, with /, but returns no url"
      , input       = "http://dx.doi.org/10.2105/AJPH"
      , expected    = Just ["10", "2105", "/", "AJPH"]
      }
    ]
