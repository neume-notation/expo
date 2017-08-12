-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Base.TimeOut (
    TimeOut,
    createTimeOut,
    destroyTimeOut,
    onTimeOut,
    fromNow
) where

import Prelude

import Control.Monad.Eff           (Eff)

import Halogen.Aff                 (HalogenEffects)

-- An opaque type for references to the TimeOut object
foreign import data TimeOut :: Type

foreign import createTimeOut :: forall eff. Eff (eff) TimeOut

foreign import destroyTimeOut :: forall eff. TimeOut -> Eff (eff) Unit

foreign import onTimeOut ::
    forall eff. TimeOut -> ( Eff (HalogenEffects eff) Unit
                             -> Eff (HalogenEffects eff) Unit
                           )

foreign import fromNow :: forall eff. TimeOut -> Int -> Eff (eff) Unit
