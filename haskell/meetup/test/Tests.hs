{-# LANGUAGE RecordWildCards #-}

import Data.Foldable      (for_)
import Data.Time.Calendar (fromGregorian)
import Test.Hspec         (Spec, describe, it, shouldBe)
import Test.Hspec.Runner  (configFastFail, defaultConfig, hspecWith)

import Meetup (Weekday(..), Schedule(..), meetupDay)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "meetupDay" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion   = returnedDay `shouldBe` expectedDay
        returnedDay = meetupDay week dayofweek year month
        expectedDay = fromGregorian year month dayofmonth

data Case = Case { description :: String
                 , year        :: Integer
                 , month       :: Int
                 , week        :: Schedule
                 , dayofweek   :: Weekday
                 , dayofmonth  :: Int
                 }

cases :: [Case]
cases = [ Case { description = "monteenth of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Teenth
               , dayofweek   = Mo
               , dayofmonth  = 13
               }
        , Case { description = "monteenth of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Teenth
               , dayofweek   = Mo
               , dayofmonth  = 19
               }
        , Case { description = "monteenth of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Teenth
               , dayofweek   = Mo
               , dayofmonth  = 16
               }
        , Case { description = "tuesteenth of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Teenth
               , dayofweek   = Tu
               , dayofmonth  = 19
               }
        , Case { description = "tuesteenth of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Teenth
               , dayofweek   = Tu
               , dayofmonth  = 16
               }
        , Case { description = "tuesteenth of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Teenth
               , dayofweek   = Tu
               , dayofmonth  = 13
               }
        , Case { description = "wednesteenth of January 2013"
               , year        = 2013
               , month       = 1
               , week        = Teenth
               , dayofweek   = We
               , dayofmonth  = 16
               }
        , Case { description = "wednesteenth of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Teenth
               , dayofweek   = We
               , dayofmonth  = 13
               }
        , Case { description = "wednesteenth of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Teenth
               , dayofweek   = We
               , dayofmonth  = 19
               }
        , Case { description = "thursteenth of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Teenth
               , dayofweek   = Th
               , dayofmonth  = 16
               }
        , Case { description = "thursteenth of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Teenth
               , dayofweek   = Th
               , dayofmonth  = 13
               }
        , Case { description = "thursteenth of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Teenth
               , dayofweek   = Th
               , dayofmonth  = 19
               }
        , Case { description = "friteenth of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Teenth
               , dayofweek   = Fr
               , dayofmonth  = 19
               }
        , Case { description = "friteenth of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Teenth
               , dayofweek   = Fr
               , dayofmonth  = 16
               }
        , Case { description = "friteenth of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Teenth
               , dayofweek   = Fr
               , dayofmonth  = 13
               }
        , Case { description = "saturteenth of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Teenth
               , dayofweek   = Sa
               , dayofmonth  = 16
               }
        , Case { description = "saturteenth of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Teenth
               , dayofweek   = Sa
               , dayofmonth  = 13
               }
        , Case { description = "saturteenth of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Teenth
               , dayofweek   = Sa
               , dayofmonth  = 19
               }
        , Case { description = "sunteenth of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Teenth
               , dayofweek   = Su
               , dayofmonth  = 19
               }
        , Case { description = "sunteenth of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Teenth
               , dayofweek   = Su
               , dayofmonth  = 16
               }
        , Case { description = "sunteenth of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Teenth
               , dayofweek   = Su
               , dayofmonth  = 13
               }
        , Case { description = "first Mo of March 2013"
               , year        = 2013
               , month       = 3
               , week        = First
               , dayofweek   = Mo
               , dayofmonth  = 4
               }
        , Case { description = "first Mo of April 2013"
               , year        = 2013
               , month       = 4
               , week        = First
               , dayofweek   = Mo
               , dayofmonth  = 1
               }
        , Case { description = "first Tu of May 2013"
               , year        = 2013
               , month       = 5
               , week        = First
               , dayofweek   = Tu
               , dayofmonth  = 7
               }
        , Case { description = "first Tu of June 2013"
               , year        = 2013
               , month       = 6
               , week        = First
               , dayofweek   = Tu
               , dayofmonth  = 4
               }
        , Case { description = "first We of July 2013"
               , year        = 2013
               , month       = 7
               , week        = First
               , dayofweek   = We
               , dayofmonth  = 3
               }
        , Case { description = "first We of August 2013"
               , year        = 2013
               , month       = 8
               , week        = First
               , dayofweek   = We
               , dayofmonth  = 7
               }
        , Case { description = "first Th of September 2013"
               , year        = 2013
               , month       = 9
               , week        = First
               , dayofweek   = Th
               , dayofmonth  = 5
               }
        , Case { description = "first Th of October 2013"
               , year        = 2013
               , month       = 10
               , week        = First
               , dayofweek   = Th
               , dayofmonth  = 3
               }
        , Case { description = "first Fr of November 2013"
               , year        = 2013
               , month       = 11
               , week        = First
               , dayofweek   = Fr
               , dayofmonth  = 1
               }
        , Case { description = "first Fr of December 2013"
               , year        = 2013
               , month       = 12
               , week        = First
               , dayofweek   = Fr
               , dayofmonth  = 6
               }
        , Case { description = "first Sa of January 2013"
               , year        = 2013
               , month       = 1
               , week        = First
               , dayofweek   = Sa
               , dayofmonth  = 5
               }
        , Case { description = "first Sa of February 2013"
               , year        = 2013
               , month       = 2
               , week        = First
               , dayofweek   = Sa
               , dayofmonth  = 2
               }
        , Case { description = "first Su of March 2013"
               , year        = 2013
               , month       = 3
               , week        = First
               , dayofweek   = Su
               , dayofmonth  = 3
               }
        , Case { description = "first Su of April 2013"
               , year        = 2013
               , month       = 4
               , week        = First
               , dayofweek   = Su
               , dayofmonth  = 7
               }
        , Case { description = "second Mo of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Second
               , dayofweek   = Mo
               , dayofmonth  = 11
               }
        , Case { description = "second Mo of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Second
               , dayofweek   = Mo
               , dayofmonth  = 8
               }
        , Case { description = "second Tu of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Second
               , dayofweek   = Tu
               , dayofmonth  = 14
               }
        , Case { description = "second Tu of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Second
               , dayofweek   = Tu
               , dayofmonth  = 11
               }
        , Case { description = "second We of July 2013"
               , year        = 2013
               , month       = 7
               , week        = Second
               , dayofweek   = We
               , dayofmonth  = 10
               }
        , Case { description = "second We of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Second
               , dayofweek   = We
               , dayofmonth  = 14
               }
        , Case { description = "second Th of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Second
               , dayofweek   = Th
               , dayofmonth  = 12
               }
        , Case { description = "second Th of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Second
               , dayofweek   = Th
               , dayofmonth  = 10
               }
        , Case { description = "second Fr of November 2013"
               , year        = 2013
               , month       = 11
               , week        = Second
               , dayofweek   = Fr
               , dayofmonth  = 8
               }
        , Case { description = "second Fr of December 2013"
               , year        = 2013
               , month       = 12
               , week        = Second
               , dayofweek   = Fr
               , dayofmonth  = 13
               }
        , Case { description = "second Sa of January 2013"
               , year        = 2013
               , month       = 1
               , week        = Second
               , dayofweek   = Sa
               , dayofmonth  = 12
               }
        , Case { description = "second Sa of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Second
               , dayofweek   = Sa
               , dayofmonth  = 9
               }
        , Case { description = "second Su of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Second
               , dayofweek   = Su
               , dayofmonth  = 10
               }
        , Case { description = "second Su of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Second
               , dayofweek   = Su
               , dayofmonth  = 14
               }
        , Case { description = "third Mo of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Third
               , dayofweek   = Mo
               , dayofmonth  = 18
               }
        , Case { description = "third Mo of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Third
               , dayofweek   = Mo
               , dayofmonth  = 15
               }
        , Case { description = "third Tu of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Third
               , dayofweek   = Tu
               , dayofmonth  = 21
               }
        , Case { description = "third Tu of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Third
               , dayofweek   = Tu
               , dayofmonth  = 18
               }
        , Case { description = "third We of July 2013"
               , year        = 2013
               , month       = 7
               , week        = Third
               , dayofweek   = We
               , dayofmonth  = 17
               }
        , Case { description = "third We of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Third
               , dayofweek   = We
               , dayofmonth  = 21
               }
        , Case { description = "third Th of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Third
               , dayofweek   = Th
               , dayofmonth  = 19
               }
        , Case { description = "third Th of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Third
               , dayofweek   = Th
               , dayofmonth  = 17
               }
        , Case { description = "third Fr of November 2013"
               , year        = 2013
               , month       = 11
               , week        = Third
               , dayofweek   = Fr
               , dayofmonth  = 15
               }
        , Case { description = "third Fr of December 2013"
               , year        = 2013
               , month       = 12
               , week        = Third
               , dayofweek   = Fr
               , dayofmonth  = 20
               }
        , Case { description = "third Sa of January 2013"
               , year        = 2013
               , month       = 1
               , week        = Third
               , dayofweek   = Sa
               , dayofmonth  = 19
               }
        , Case { description = "third Sa of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Third
               , dayofweek   = Sa
               , dayofmonth  = 16
               }
        , Case { description = "third Su of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Third
               , dayofweek   = Su
               , dayofmonth  = 17
               }
        , Case { description = "third Su of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Third
               , dayofweek   = Su
               , dayofmonth  = 21
               }
        , Case { description = "fourth Mo of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Fourth
               , dayofweek   = Mo
               , dayofmonth  = 25
               }
        , Case { description = "fourth Mo of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Fourth
               , dayofweek   = Mo
               , dayofmonth  = 22
               }
        , Case { description = "fourth Tu of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Fourth
               , dayofweek   = Tu
               , dayofmonth  = 28
               }
        , Case { description = "fourth Tu of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Fourth
               , dayofweek   = Tu
               , dayofmonth  = 25
               }
        , Case { description = "fourth We of July 2013"
               , year        = 2013
               , month       = 7
               , week        = Fourth
               , dayofweek   = We
               , dayofmonth  = 24
               }
        , Case { description = "fourth We of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Fourth
               , dayofweek   = We
               , dayofmonth  = 28
               }
        , Case { description = "fourth Th of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Fourth
               , dayofweek   = Th
               , dayofmonth  = 26
               }
        , Case { description = "fourth Th of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Fourth
               , dayofweek   = Th
               , dayofmonth  = 24
               }
        , Case { description = "fourth Fr of November 2013"
               , year        = 2013
               , month       = 11
               , week        = Fourth
               , dayofweek   = Fr
               , dayofmonth  = 22
               }
        , Case { description = "fourth Fr of December 2013"
               , year        = 2013
               , month       = 12
               , week        = Fourth
               , dayofweek   = Fr
               , dayofmonth  = 27
               }
        , Case { description = "fourth Sa of January 2013"
               , year        = 2013
               , month       = 1
               , week        = Fourth
               , dayofweek   = Sa
               , dayofmonth  = 26
               }
        , Case { description = "fourth Sa of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Fourth
               , dayofweek   = Sa
               , dayofmonth  = 23
               }
        , Case { description = "fourth Su of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Fourth
               , dayofweek   = Su
               , dayofmonth  = 24
               }
        , Case { description = "fourth Su of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Fourth
               , dayofweek   = Su
               , dayofmonth  = 28
               }
        , Case { description = "last Mo of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Last
               , dayofweek   = Mo
               , dayofmonth  = 25
               }
        , Case { description = "last Mo of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Last
               , dayofweek   = Mo
               , dayofmonth  = 29
               }
        , Case { description = "last Tu of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Last
               , dayofweek   = Tu
               , dayofmonth  = 28
               }
        , Case { description = "last Tu of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Last
               , dayofweek   = Tu
               , dayofmonth  = 25
               }
        , Case { description = "last We of July 2013"
               , year        = 2013
               , month       = 7
               , week        = Last
               , dayofweek   = We
               , dayofmonth  = 31
               }
        , Case { description = "last We of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Last
               , dayofweek   = We
               , dayofmonth  = 28
               }
        , Case { description = "last Th of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Last
               , dayofweek   = Th
               , dayofmonth  = 26
               }
        , Case { description = "last Th of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Last
               , dayofweek   = Th
               , dayofmonth  = 31
               }
        , Case { description = "last Fr of November 2013"
               , year        = 2013
               , month       = 11
               , week        = Last
               , dayofweek   = Fr
               , dayofmonth  = 29
               }
        , Case { description = "last Fr of December 2013"
               , year        = 2013
               , month       = 12
               , week        = Last
               , dayofweek   = Fr
               , dayofmonth  = 27
               }
        , Case { description = "last Sa of January 2013"
               , year        = 2013
               , month       = 1
               , week        = Last
               , dayofweek   = Sa
               , dayofmonth  = 26
               }
        , Case { description = "last Sa of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Last
               , dayofweek   = Sa
               , dayofmonth  = 23
               }
        , Case { description = "last Su of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Last
               , dayofweek   = Su
               , dayofmonth  = 31
               }
        , Case { description = "last Su of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Last
               , dayofweek   = Su
               , dayofmonth  = 28
               }
        , Case { description = "last We of February 2012"
               , year        = 2012
               , month       = 2
               , week        = Last
               , dayofweek   = We
               , dayofmonth  = 29
               }
        , Case { description = "last We of December 2014"
               , year        = 2014
               , month       = 12
               , week        = Last
               , dayofweek   = We
               , dayofmonth  = 31
               }
        , Case { description = "last Su of February 2015"
               , year        = 2015
               , month       = 2
               , week        = Last
               , dayofweek   = Su
               , dayofmonth  = 22
               }
        , Case { description = "first Fr of December 2012"
               , year        = 2012
               , month       = 12
               , week        = First
               , dayofweek   = Fr
               , dayofmonth  = 7
               }
        ]
