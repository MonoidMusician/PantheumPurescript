module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff (HalogenEffects)
import Halogen.Aff.Util (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

import Vocab (ui)

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui unit body
