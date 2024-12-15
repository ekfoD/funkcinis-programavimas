{-# LANGUAGE DeriveFunctor #-}
module DSL where

import Control.Monad.Free (Free(..), liftF)


-- define domain algebra
data DomainAlgebra next
  = CreateMelody Int String next
  | ReadMelody Int next
  | MelodyList next
  | DeleteMelody Int next
  | ChangeTempoMelody Int String next
  | TransposeMelody Int String next
  | EditMelody Int String next
  | Save next
  | Load next
  deriving (Functor)

type Domain = Free DomainAlgebra

createMelody :: Int -> String -> Domain ()
createMelody id melody = liftF $ CreateMelody id melody ()

readMelody :: Int -> Domain ()
readMelody id = liftF $ ReadMelody id ()

melodyList :: Domain ()
melodyList = liftF $ MelodyList ()

deleteMelody :: Int -> Domain ()
deleteMelody id = liftF $ DeleteMelody id ()

changeTempoMelody :: Int -> String -> Domain ()
changeTempoMelody id tempo = liftF $ ChangeTempoMelody id tempo ()

transposeMelody :: Int -> String -> Domain ()
transposeMelody id pitch = liftF $ TransposeMelody id pitch ()

editMelody :: Int -> String -> Domain ()
editMelody id editString = liftF $ EditMelody id editString ()

save :: Domain ()
save = liftF $ Save ()

load :: Domain ()
load = liftF $ Load ()
