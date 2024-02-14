import Data.List
import Test.Tasty
import Test.Tasty.Falsify
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range as Range
import qualified Test.Falsify.Predicate as P
import Test.Falsify.Predicate ((.$))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [
    testProperty "sort == sort . reverse" prop_Foo
  ]

prop_Foo :: Property ()
prop_Foo = do
    xs <- gen $ Gen.list (Range.between (1, 10)) (Gen.int (Range.between (-1000, 1000)))
    assert $ P.eq .$ ("sort xs", sort xs) .$ ("reverse . sort $ xs", sort . reverse $ xs)

unitTests :: TestTree
unitTests = testGroup "Unit tests" []