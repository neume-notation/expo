-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Talk.Dispatcher (
    Dispatcher,
    newDispatcher,
    onDispatch
) where

-- A thin interface to JSModules/dispatch.js

import Prelude

import Control.Monad.Eff           (Eff)
import DOM                         (DOM())

import Halogen.Aff                 (HalogenEffects)

import Base.Types                  (TalkElement, TalkSequence)

-- An opaque type for references to the Dispatcher object
foreign import data Dispatcher :: (Type -> Type) -> Type

foreign import newDispatcher :: forall eff a. TalkSequence a -> Eff (dom :: DOM | eff) (Dispatcher a)

-- `onDispatch dispatcher` registers a handler to receive notifications
-- from the Dispatcher about TalkSequence actions which need to be carried
-- out (generally because of a time being reached in the video, or because
-- of the user scrolling the text, in a Narrative).
-- The value passed to the handler, when it is called, is a single element
-- of `talkSequence` whose action needs to be performed - it seems to be
-- hard to handle multiple actions in one go in Halogen using `eventSource`.
-- `onDispatch talkSequence` can be used as the first argument to
-- `eventSource` - note that it is in Eff, so must return a function which
-- performs the actual registration.  The second argument to `eventSource`
-- is the actual handler, ie it is the thing here which has type
-- `String -> Eff (H.HalogenEffects eff) Unit`
foreign import onDispatch ::
    forall eff a. Dispatcher a
                  -> ( (TalkElement a -> Eff (HalogenEffects eff) Unit)
                       -> Eff (HalogenEffects eff) Unit
                     )
