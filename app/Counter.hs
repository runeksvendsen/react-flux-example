{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Counter where

import Canvas
import Ajax

import Control.DeepSeq
import Data.Aeson (object, (.=))
import Data.Typeable
import GHC.Generics
import GHCJS.Types
import React.Flux
import React.Flux.Combinators
import React.Flux.Addons.Intl
import qualified Data.JSString as JSString

newtype Counter
    = Counter { unCounter :: Int }
    deriving (Show, Typeable)

data CounterAction
   = CounterIncrement
   | CounterDecrement
   deriving (Show, Typeable, Generic, NFData)

instance StoreData Counter where
    type StoreAction Counter = CounterAction
    transform action (Counter idx) =
        pure $ Counter $
        case action of
          CounterIncrement -> idx + 1
          CounterDecrement -> idx - 1

counterStore :: ReactStore Counter
counterStore = mkStore (Counter 1000)

dispatchCounter :: CounterAction -> [SomeStoreAction]
dispatchCounter a = [SomeStoreAction counterStore a]

counterApp :: ReactView ()
counterApp =
    defineControllerView "Counter APP" counterStore $ \counterState () ->
    div_ $
    do span_ [] $ int_ $ unCounter counterState
       button_ [ onClick $ \_ _ -> dispatchCounter CounterIncrement ] $ $(message "up-button" "Up") []
       button_ [ onClick $ \_ _ -> dispatchCounter CounterDecrement ] $ $(message "down-button" "Down") []
       br_ mempty
       view ajaxView () mempty
       view canvasView (2*pi * (fromIntegral (unCounter counterState) / 100)) mempty

