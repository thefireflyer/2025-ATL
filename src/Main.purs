module Main (main, parse, tyOf, eval, evalStatics) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import ATL.Defs

--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  log "🍝"

--------------------------------------------------------------------------------

parse :: String -> Tm
parse _ = Tm

tyOf :: Tm -> Ty
tyOf _ = Ty

evalStatics :: Tm -> Tm
evalStatics _ = Tm

eval :: Tm -> Tm
eval _ = Tm

--------------------------------------------------------------------------------