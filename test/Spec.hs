{-# LANGUAGE ImportQualifiedPost #-}

import Lib1 qualified
import Lib2 qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Lib1 tests"
    [ testCase "Empty Query" $
        Lib2.parseQuery "" @?= Left "Error, command doesn't match any known query.",
      testCase "Parsing case 1 - give a better name" $
        Lib2.parseChangeTempoMelody "changeTempoMelody 1 +3" @?= Right (Lib2.ChangeTempoMelody 1 (Lib2.SmallInteger Lib2.Plus 3), ""),
      testCase "Creating melody" $
        Lib2.parseCreateMelody "createMelody 1 F2F4F8(A2((G16))B4) stop"
          @?= Right
            ( Lib2.CreateMelody
                1
                ( Lib2.CompoundMelody
                    [ Lib2.SingleNote (Lib2.Note Lib2.F Lib2.Half),
                      Lib2.SingleNote (Lib2.Note Lib2.F Lib2.Quarter),
                      Lib2.SingleNote (Lib2.Note Lib2.F Lib2.Eighth),
                      Lib2.CompoundMelody
                        [ Lib2.SingleNote (Lib2.Note Lib2.A Lib2.Half),
                          Lib2.CompoundMelody [Lib2.CompoundMelody [Lib2.SingleNote (Lib2.Note Lib2.G Lib2.Sixteenth)]],
                          Lib2.SingleNote (Lib2.Note Lib2.B Lib2.Quarter)
                        ]
                    ]
                ),
              ""
            )
    ]