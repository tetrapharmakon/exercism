{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import LeapYear (isLeapYear)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "isLeapYear" $ for_ cases test
  where

    test Case{..} = it explanation assertion
      where
        explanation = unwords [show input, "-", description]
        assertion   = isLeapYear (fromIntegral input) `shouldBe` expected

data Case = Case { description :: String
                 , input       :: Integer
                 , expected    :: Bool
                 }

cases :: [Case]
cases = [ Case { description = "year not divisible by 4: common year"
               , input       = 2015
               , expected    = False
               }
        , Case { description = "year divisible by 4, not divisible by 100: leap year"
               , input       = 1996
               , expected    = True
               }
        , Case { description = "year divisible by 100, not divisible by 400: common year"
               , input       = 2100
               , expected    = False
               }
        , Case { description = "year divisible by 400: leap year"
               , input       = 2000
               , expected    = True
               }
        , Case { description = "year divisible by 200, not divisible by 400: common year"
               , input       = 1800
               , expected    = False
               }
        ]
test_list = [  1584
             , 1588
             , 1592
             , 1596
             , 1600
             , 1604
             , 1608
             , 1612
             , 1616
             , 1620
             , 1624
             , 1628
             , 1632
             , 1636
             , 1640
             , 1644
             , 1648
             , 1652
             , 1656
             , 1660
             , 1664
             , 1668
             , 1672
             , 1676
             , 1680
             , 1684
             , 1688
             , 1692
             , 1696
             , 1704
             , 1708
             , 1712
             , 1716
             , 1720
             , 1724
             , 1728
             , 1732
             , 1736
             , 1740
             , 1744
             , 1748
             , 1752
             , 1756
             , 1760
             , 1764
             , 1768
             , 1772
             , 1776
             , 1780
             , 1784
             , 1788
             , 1792
             , 1796
             , 1804
             , 1808
             , 1812
             , 1816
             , 1820
             , 1824
             , 1828
             , 1832
             , 1836
             , 1840
             , 1844
             , 1848
             , 1852
             , 1856
             , 1860
             , 1864
             , 1868
             , 1872
             , 1876
             , 1880
             , 1884
             , 1888
             , 1892
             , 1896
             , 1904
             , 1908
             , 1912
             , 1916
             , 1920
             , 1924
             , 1928
             , 1932
             , 1936
             , 1940
             , 1944
             , 1948
             , 1952
             , 1956
             , 1960
             , 1964
             , 1968
             , 1972
             , 1976
             , 1980
             , 1984
             , 1988
             , 1992
             , 1996
             , 2000
             , 2004
             , 2008
             , 2012
             , 2016
             , 2020
            ]