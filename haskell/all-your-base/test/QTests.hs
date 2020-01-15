import Base (fromBase, inBase)
import Test.Hspec

-- import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

main :: IO ()
main =
  hspec $
  describe "Base-conversion twice is identity" $ do
    it "n == (n `inBase` 10) `fromBase` 10" $
      property (\n -> n == (n `inBase` 10) `fromBase` 10)
    -- modifyMaxSuccess (const 101) $
