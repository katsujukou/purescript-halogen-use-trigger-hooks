module Example.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (captures, useTickEffect)
import Halogen.Hooks as Hooks
import Halogen.UseTrigger (useTrigger)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI app {} body

app :: forall q i o m. MonadEffect m => H.Component q i o m
app = Hooks.component \_ _ -> Hooks.do
  { triggerEval } <- useTrigger

  let
    handleClick = do
      triggerEval

  captures {} useTickEffect do
    Console.log "Tick"
    pure Nothing

  Hooks.pure do
    HH.div []
      [ HH.button
          [ HE.onClick \_ -> handleClick ]
          [ HH.text "I'll trigger eval!" ]
      , HH.button
          [ HE.onClick \_ -> pure unit ]
          [ HH.text "I won't trigger eval..." ]
      ]
