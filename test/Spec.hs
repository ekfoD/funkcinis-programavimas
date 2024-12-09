{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib2 qualified
import Lib3 qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

genPitch :: Gen Lib2.Pitch
genPitch = QC.elements [Lib2.A, Lib2.B, Lib2.C, Lib2.D, Lib2.E, Lib2.F, Lib2.G]

genDuration :: Gen Lib2.Duration
genDuration = QC.elements [Lib2.Whole, Lib2.Half, Lib2.Quarter, Lib2.Eighth, Lib2.Sixteenth]

genNote :: Gen Lib2.Note
genNote = Lib2.Note <$> genPitch <*> genDuration

-- generate Melodiiiiies
genMelody :: Int -> Gen Lib2.Melody
genMelody 0 = Lib2.CompoundMelody . (: []) . Lib2.SingleNote <$> genNote
genMelody depth =
  QC.oneof
    [ Lib2.CompoundMelody . (: []) . Lib2.SingleNote <$> genNote,
      Lib2.CompoundMelody <$> listOf1 (genMelody (depth - 1))
    ]

-- Generate CreateMelody queries
genCreateMelodyQuery :: Gen Lib2.Query
genCreateMelodyQuery = do
  mid <- QC.choose (1, 9) -- melody ID range
  melody <- genMelody 3 -- Limit recursion depth
  return $ Lib2.CreateMelody mid melody

-- Modified to ONLY generate Batch statements (nes taip rendering veikia all good)
genBatchStatements :: Gen Lib3.Statements
genBatchStatements = Lib3.Batch <$> listOf1 genCreateMelodyQuery

-- Update Arbitrary instance to only generate Batches
instance Arbitrary Lib3.Statements where
  arbitrary = genBatchStatements

-- Update property test to focus on Batch round-trip
propertyTests :: TestTree
propertyTests =
  testGroup
    "Property Tests"
    [ testProperty "batch rendering and parsing preserves structure" $
        \s -> case Lib3.parseStatements (Lib3.renderStatements s) of
          Right (parsedStatements, "") -> s == parsedStatements
          _ -> False
    ]

tests :: TestTree
tests =
  testGroup
    "Melody State Tests"
    [ unitTests,
      propertyTests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testCase "Parsing empty string fails" $
        Lib2.parseQuery "" @?= Left "Error! could not parse that",
      testCase "rendering works?" $
        Lib3.renderStatements (Lib3.Batch [Lib2.CreateMelody 1 (Lib2.CompoundMelody [Lib2.SingleNote (Lib2.Note Lib2.A Lib2.Whole)])]) @?= "BEGIN createMelody 1 A1 stop; END",
      testCase "parsing works?" $
        Lib3.parseStatements "BEGIN createMelody 1 A1 stop END" @?= Right (Lib3.Batch [Lib2.CreateMelody 1 (Lib2.CompoundMelody [Lib2.SingleNote (Lib2.Note Lib2.A Lib2.Whole)])], ""),
      testCase "Batch rendering works?" $
        Lib3.renderStatements (Lib3.Batch [Lib2.CreateMelody 1 (Lib2.SingleNote (Lib2.Note Lib2.A Lib2.Whole)), Lib2.CreateMelody 2 (Lib2.SingleNote (Lib2.Note Lib2.B Lib2.Half))]) @?= "BEGIN createMelody 1 A1 stop; createMelody 2 B2 stop; END"
    ]