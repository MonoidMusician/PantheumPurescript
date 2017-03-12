module ArrayState (
    ArrayState, StateArray,
    sequence,
    sequenceReversed,
    evalArrayState,
    evalReversedArrayState
) where

import Prelude (map, ($), (<<<))
import Data.Array (reverse)
import Control.Monad.State (State)
import Control.Monad.State.Trans (evalStateT)
import Data.Identity (Identity(..))
import Data.Traversable (sequence) as Traversable

type ArrayState stateT dataT = Array (State stateT dataT)
type StateArray stateT dataT = State stateT (Array dataT)

sequence :: forall stateT dataT. ArrayState stateT dataT -> StateArray stateT dataT
sequence = Traversable.sequence

sequenceReversed :: forall stateT dataT. ArrayState stateT dataT -> StateArray stateT dataT
sequenceReversed = map reverse <<< sequence <<< reverse

evalArrayState :: forall i r. ArrayState i r -> i -> Array r
evalArrayState monad init =
    case evalStateT (sequence monad) init of
        Identity res -> res

evalReversedArrayState :: forall i r. ArrayState i r -> i -> Array r
evalReversedArrayState monad init =
    reverse $ evalArrayState (reverse monad) init
