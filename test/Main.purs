module Test.Main (main) where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.ATLCore as ATLCore
import Test.ATL as ATL

--------------------------------------------------------------------------------

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] $ ATLCore.spec *> ATL.spec

--------------------------------------------------------------------------------
