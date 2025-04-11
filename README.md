# PureScript Halogen UseTrigger Hooks

A tiny hooks library allows ones to control the component evaluation.

[![purs - v0.15.15](https://img.shields.io/badge/purs-v0.15.15-blue?logo=purescript)](https://github.com/purescript/purescript/releases/tag/v0.15.15) [![CI](https://github.com/katsujukou/purescript-halogen-use-trigger-hooks/actions/workflows/ci.yml/badge.svg)](https://github.com/katsujukou/purescript-halogen-use-trigger-hooks/actions/workflows/ci.yml)

## How to Use

Install with spago

```sh
spago install halogen-use-trigger-hooks
```

You can imperatively force a re-render:

```purs
app :: forall q i o m. MonadEffect m => H.Component q i o m
app = Hooks.component \_ _ -> Hooks.do
  { triggerEval } <- useTrigger

  Hooks.pure do
    HH.div []
      [ HH.button
          [ HE.onClick \_ -> triggerEval ]
          [ HH.text "I'll trigger eval!" ]
      ]
```

Clicking the button will trigger a component evaluation (and thus re-rendering).
You can also run some effectful code on the next tick:

```purs
app :: forall q i o m. MonadEffect m => H.Component q i o m
app = Hooks.component \_ _ -> Hooks.do
  { onNextTick } <- useTrigger

  let 
    handleClick = do
      onNextTick \_ -> do
        Console.log "I clicked in the previous tick."

  Hooks.pure do
    HH.div []
      [ HH.button
          [ HE.onClick \_ -> handleClick ]
          [ HH.text "I'll trigger eval on next tick!" ]
      ]
```

Clicking the button does not trigger an immediate re-render; 
instead, `handleClick` will run on the next tick (after a component state change).