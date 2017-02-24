module Table where

import Prelude
import Data.Typelevel.Num
import DOM.Event.Event as Event
import DOM.HTML.HTMLInputElement as HInput
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import MDL as MDL
import MDL.Button as Button
import MDL.Card as Card
import MDL.Layout as Layout
import MDL.Shadow as Shadow
import MDL.Textfield as Textfield
import CSS (marginLeft, marginRight, rad, vGradient)
import CSS.Common (auto)
import Control.Bind ((=<<))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.Types (Event, MouseEvent)
import DOM.HTML.Indexed.InputType (InputType(..))
import DOM.HTML.Types (HTMLInputElement)
import Data.Maybe (Maybe(..))
import Data.Vector (Vec(..), (!!), (+>), empty, toArray, mapWithIndex)
import Halogen.HTML.CSS (style)
import Unsafe.Coerce (unsafeCoerce)

single :: forall a b. (Array (HH.HTML a b) -> HH.HTML a b) -> HH.HTML a b -> HH.HTML a b
single node child = node [ child ]
infixr 9 single as <\>

data Query a
    = ToggleState a
    | UserInput Event a

type Inflection nRows nCols cell =
    { rowLabels :: Vec nRows String
    , colLabels :: Vec nCols String
    , cells :: Vec nRows (Vec nCols cell)
    }

type Declension cell = Inflection D5 D2 cell
type Noun = Declension String

nomen :: Noun
nomen =
    { rowLabels: "nom" +> "gen" +> "dat" +> "acc" +> "abl" +> empty
    , colLabels: "sg" +> "pl" +> empty
    , cells:
        ( "nomen" +> "nomina" +> empty ) +>
        ( "nominis" +> "nominum" +> empty) +>
        ( "nomini" +> "nominibus" +> empty) +>
        ( "nomen" +> "nomina" +> empty) +>
        ( "nomine" +> "nominibus" +> empty) +>
        empty
    }

type State =
    { on :: Boolean
    , text :: String
    }

initialState :: State
initialState =
    { on: false
    , text: "Hello"
    }

button :: forall a b. (MouseEvent -> Maybe b) -> Array HH.ClassName -> String -> HH.HTML a b
button cmd classes label =
  HH.button
    [ HE.onClick cmd
    , HP.classes $
        append classes
          [ MDL.button
          , MDL.jsButton
          , MDL.jsRippleEffect
          , Button._raised
          ]
    ]
    [ HH.text label ]

input :: forall a b. (Event -> Maybe b) -> Array HH.ClassName -> String -> String -> HH.HTML a b
input cmd classes label value =
    mdiv [ MDL.textfield, MDL.jsTextfield ]
        [ HH.input
            [ HP.classes [ Textfield.input ]
            , HP.type_ InputText
            , HP.id_ "sample1"
            , HE.onInput cmd
            , HP.value value
            ]
        , HH.label
            [ HP.classes [ Textfield.label ]
            , HP.for "sample1"
            ]
            [ HH.text label ]
        ]

mdiv :: forall a b. Array HH.ClassName -> Array (HH.HTML a b) -> HH.HTML a b
mdiv classes children =
    HH.div [ HP.classes classes ] children
mdiv1 :: forall a b. HH.ClassName -> Array (HH.HTML a b) -> HH.HTML a b
mdiv1 = mdiv <<< Array.singleton

card :: forall a b. Array (HH.HTML a b) -> HH.HTML a b
card =
    HH.div
        [ HP.classes [ MDL.card, Shadow._2Dp ]
        , style do
            marginLeft auto
            marginRight auto
        ]

thtext :: forall a b. String -> HH.HTML a b
thtext = HH.th_ <<< Array.singleton <<< HH.text

tdtext :: forall a b. String -> HH.HTML a b
tdtext = HH.td_ <<< Array.singleton <<< HH.text

table :: forall cell nRows nCols a b. (Nat nRows, Nat nCols) => Inflection nRows nCols cell -> (cell -> HH.HTML a b) -> HH.HTML a b
table datable mapper = HH.table_ <\> HH.tbody_ (map HH.tr_ (Array.cons header rows))
    where
      header = Array.cons (thtext "") $ map thtext $ toArray datable.colLabels
      labels = toArray datable.rowLabels
      label i = Maybe.fromMaybe "" $ Array.index labels i
      rows =
        Array.mapWithIndex rowOf $ toArray datable.cells
      rowOf i cells =
        Array.cons (thtext $ label i) (map mapper $ toArray cells)

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (dom :: DOM | eff))
ui = H.component { render, eval, initialState: const initialState, receiver: const Nothing }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    mdiv1 Layout.container <\>
      mdiv [ MDL.layout ] <\>
        mdiv1 Layout.content <\>
            table nomen tdtext

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM | eff))
  eval (ToggleState next) = do
    H.modify (\state -> { on: not state.on, text: "Bye" })
    pure next
  eval (UserInput e next) = do
    let node = unsafeCoerce Event.target e :: HTMLInputElement
    s <- H.liftEff (HInput.value node :: Eff (dom :: DOM | eff) String)
    H.modify (\state -> { on: state.on, text: s })
    pure next
