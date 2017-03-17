module Table where

import Prelude
import UIHelpers
import Pantheum.Inflection.Table
import Pantheum.Latin.Inflection
import DOM.Event.Event as Event
import DOM.HTML.HTMLInputElement as HInput
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import MDL as MDL
import MDL.Layout as Layout
import CSS.TextAlign (leftTextAlign, textAlign)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.Types (Event, MouseEvent)
import DOM.HTML.Types (HTMLInputElement)
import Data.Maybe (Maybe(Nothing))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Typelevel.Num (class Nat, D2, D5)
import Data.Vector (Vec, empty, toArray, (+>))
import Halogen.HTML.CSS (style)
import Unsafe.Coerce (unsafeCoerce)

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

celeriter :: CompoundTable Unit Unit Degree Unit Unit String
celeriter = simpleVertical
    { rows: [Positive, Comparative, Superlative]
    , getCell: case _ of
        Positive -> "celeriter"
        Comparative -> "celerius"
        Superlative -> "celerrimē"
    }

headerproduct :: forall majT minT. Array majT -> NonEmpty Array minT -> Headers majT minT
headerproduct major minor =
    map (\label -> { label, sub: minor }) major

getCellVolo :: Mood -> Voice -> Tense -> Numerus -> Person -> String
getCellVolo Indicative Active Present Singular FirstP = "volo"
getCellVolo Indicative Active Present Singular SecondP = "vīs"
getCellVolo Indicative Active Present Singular ThirdP = "volunt"
getCellVolo Indicative Active Present Plural FirstP = "volumus"
getCellVolo Indicative Active Present Plural SecondP = "vultis"
getCellVolo Indicative Active Present Plural ThirdP = "volunt"
getCellVolo Indicative Active Imperfect Singular FirstP = "volēbam"
getCellVolo Indicative Active Imperfect Singular SecondP = "volēbās"
getCellVolo Indicative Active Imperfect Singular ThirdP = "volēbat"
getCellVolo Indicative Active Imperfect Plural FirstP = "volēbāmus"
getCellVolo Indicative Active Imperfect Plural SecondP = "volēbātis"
getCellVolo Indicative Active Imperfect Plural ThirdP = "volēbant"
getCellVolo _ _ _ _ _ = "UNK"

volo :: CompoundTable Mood Voice Tense Numerus Person String
volo = CompoundTable ([TableSection
    { section: Indicative
    , rows: headerproduct [Active] (Present :| [Imperfect])
    , cols: headerproduct [Singular, Plural] (FirstP :| [SecondP, ThirdP])
    , getCell: getCellVolo
    }])

type State =
    { on :: Boolean
    , text :: String
    }

initialState :: State
initialState =
    { on: false
    , text: "Hello"
    }

table :: forall cell nRows nCols a b. (Nat nRows, Nat nCols)
    => Inflection nRows nCols cell
    -> (cell -> HH.HTML a b)
    -> HH.HTML a b
table datable mapper = HH.table_><HH.tbody_ (map HH.tr_ (Array.cons header rows))
    where
      header = Array.cons (HH.th_/> "") $ map (HH.th[style (textAlign leftTextAlign)]/> _) $ toArray datable.colLabels
      labels = toArray datable.rowLabels
      label i = Maybe.fromMaybe "" $ Array.index labels i
      rows =
        Array.mapWithIndex rowOf $ toArray datable.cells
      rowOf i cells =
        Array.cons (HH.th_/> label i) (map mapper $ toArray cells)

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (dom :: DOM | eff))
ui = H.component { render, eval, initialState: const initialState, receiver: const Nothing }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    mdiv1 Layout.container
    ><mdiv [ MDL.layout ]
      [ mdiv1 Layout.content
        ><table nomen (input (HE.input UserInput) [] "Form of nomen" >>> (HH.td_><_))
      , mdiv1 Layout.content>< display volo
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM | eff))
  eval (ToggleState next) = do
    H.modify (\state -> { on: not state.on, text: "Bye" })
    pure next
  eval (UserInput e next) = do
    let node = unsafeCoerce Event.target e :: HTMLInputElement
    s <- H.liftEff (HInput.value node :: Eff (dom :: DOM | eff) String)
    H.modify (\state -> { on: state.on, text: s })
    pure next
