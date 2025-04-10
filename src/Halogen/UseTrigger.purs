module Halogen.UseTrigger where

import Prelude

import Data.Tuple.Nested ((/\))
import Halogen.Hooks (type (<>), class HookNewtype, HookType, UseState, useState)
import Halogen.Hooks as Hooks
import Halogen.UseTrigger.Internal (EvalTrigger, newEvalTrigger)

foreign import data UseTrigger :: HookType

type UseTrigger' = UseState EvalTrigger <> Hooks.Pure

instance HookNewtype UseTrigger UseTrigger'

type UseTriggerInterface m =
  { triggerEval :: Hooks.HookM m Unit
  }

useTrigger :: forall m. Hooks.Hook m UseTrigger (UseTriggerInterface m)
useTrigger = Hooks.wrap hook
  where
  hook = Hooks.do
    _ /\ stateId <- useState (newEvalTrigger unit)

    let
      triggerEval = Hooks.put stateId (newEvalTrigger unit)

    Hooks.pure
      { triggerEval
      }