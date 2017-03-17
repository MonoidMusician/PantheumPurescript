module Table where

import Prelude
import UIHelpers
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
import Data.Foldable (maximum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (class Nat, D2, D5)
import Data.Vector (Vec, empty, toArray, (+>))
import Debug.Trace (spy)
import Global.Unsafe (unsafeStringify)
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

class Show d <= Display d where
    display :: forall a b. d -> HH.HTML a b

instance unitLabel :: Display Unit where
    display _ = HH.text ""

instance emptyLabel :: Display Void where
    display _ = HH.text ""

instance identityDisplay :: Display String where
    display = HH.text

newtype TableSection sectionT majRT minRT majCT minCT dataT =
    TableSection
        { section :: sectionT
        , rows :: Headers majRT minRT
        , cols :: Headers majCT minCT
        , getCell :: sectionT -> majRT -> minRT -> majCT -> minCT -> dataT
        }


instance showTableSection :: (
    Show sectionT, Show majRT, Show minRT, Show majCT, Show minCT, Show dataT
) => Show (TableSection sectionT majRT minRT majCT minCT dataT) where
    show (TableSection { section, rows, cols, getCell }) =
        unsafeStringify
            { section: show section
            , rows: map showHeader rows
            , cols: map showHeader cols
            }
        where
            showHeader :: forall a b. (Show a, Show b) => Header a b -> String
            showHeader { label, sub: [] } =
                show label
            showHeader { label, sub } =
                show label <> " " <> show sub

{-
instance showTableSection :: Show (TableSection Unit Degree minRT majCT minCT dataT) where
    show (TableSection { section, rows, cols, getCell }) =
        show $ map showHeader rows
        where
            showHeader { label } = show label
-}

newtype CompoundTable sectionT majRT minRT majCT minCT dataT
    = CompoundTable (Array (TableSection sectionT majRT minRT majCT minCT dataT))


instance showTable :: (
    Show sectionT, Show majRT, Show minRT, Show majCT, Show minCT, Show dataT
) => Show (CompoundTable sectionT majRT minRT majCT minCT dataT) where
    show (CompoundTable t) = show t


--instance showTable :: Show (CompoundTable Unit Degree minRT majCT minCT dataT) where show (CompoundTable t) = show t

simpleTable :: forall majRT minRT majCT minCT dataT.
    { rows :: Headers majRT minRT
    , cols :: Headers majCT minCT
    , getCell :: majRT -> minRT -> majCT -> minCT -> dataT
    } -> CompoundTable Unit majRT minRT majCT minCT dataT
simpleTable { rows, cols, getCell } =
    { section: unit
    , rows: rows
    , cols: cols
    , getCell: const getCell
    } # TableSection # Array.singleton # CompoundTable

simpleVertical :: forall majRT minRT majCT minCT dataT.
    { rows :: Array majRT
    , getCell :: majRT -> dataT
    } -> CompoundTable Unit majRT minRT majCT minCT dataT
simpleVertical { rows, getCell } =
    { section: unit
    , rows: fullRows
    , cols: []
    , getCell: \_ row _ _ _ -> getCell row
    } # TableSection # Array.singleton # CompoundTable
    where
        fullRows =
            map (\label -> { label, sub: [] }) rows

type Header majT minT =
    { label :: majT
    , sub :: Array minT
    }
type Headers majT minT = Array (Header majT minT)


computeGutterWidth :: forall sectionT majRT minRT majCT minCT dataT
     . CompoundTable sectionT majRT minRT majCT minCT dataT
    -> Int
computeGutterWidth (CompoundTable sections) =
    sections # map (\(TableSection { rows }) ->
        if Array.null rows then 0
        else if Array.all (_.sub >>> Array.null) rows
        then 1 else 2
    ) # maximum # fromMaybe 0


mcons :: forall a. Maybe a -> Array a -> Array a
mcons (Just head) tail = Array.cons head tail
mcons Nothing tail = tail

instance displayTable :: (
    Display sectionT, Display majRT, Display minRT, Display majCT, Display minCT, Display dataT
) => Display (CompoundTable sectionT majRT minRT majCT minCT dataT) where
    display ctable@(CompoundTable [TableSection section]) =
        HH.table_><HH.tbody_ $ map HH.tr_ rows
        where
            gutterWidth = computeGutterWidth ctable
            cell a
                | gutterWidth == 0 = Nothing
                | otherwise =
                    Just $ HH.th [ HP.colSpan gutterWidth ] a
            mkMajRowH contents
                | gutterWidth == 0 =
                    HH.th_ >< contents
                | otherwise =
                    HH.th [ HP.colSpan gutterWidth ] >< contents
            labelCellM = cell [ display section.section ]
            padCellM = cell $ []
            headerMajCols =
                section.cols # map (\{ label, sub } ->
                    HH.th [ HP.colSpan $ max 1 $ Array.length sub ] >< display label
                )
            header = mcons labelCellM headerMajCols
            subcol2s =
                section.cols # Array.concatMap (\{ label, sub } ->
                    map (Tuple label) sub
                )
            continue majRow minRow =
                subcol2s # map (\(Tuple majCol minCol) ->
                    HH.td_ >< display $ section.getCell section.section majRow minRow majCol minCol
                )
            subheaderM =
                if Array.any (\{ sub } -> not $ Array.null sub) section.cols
                then
                    section.cols # Array.concatMap (\{ sub } ->
                        case sub of
                            [] ->
                                [ HH.th_ [] ]
                            _ ->
                                map (\h -> HH.th_ >< display h) sub
                    ) # Just
                else Nothing
            headerrows =
                case subheaderM of
                    Nothing -> [ header ]
                    Just subheader ->
                        [ header, mcons padCellM subheader ]
            simplerows =
                Array.all (\{ sub } -> Array.null sub) section.cols
            contentrows =
                section.rows # Array.concatMap mkrow
            mkrow { label, sub } =
                if simplerows
                then
                    [ mcons padCellM [ HH.th_ [ display label ] ] ]
                else
                    let
                        subrows = map (\sublabel -> [ HH.th_ [], HH.th_ [ display sublabel ] ] <> continue label sublabel) sub
                    in [ [ mkMajRowH $ display label ] ] <> subrows
            rows = headerrows <> contentrows
    display _ = display "table"

{-
computeCellWidths :: forall majT minT. Header majT minT -> Header (majT, Int) (minT, Int)

let
    labelCell = case computeGutterWidth table of
        0 -> Nothing
        1 -> Just $ H.th_ $ display section.section
        width -> Just $ H.th [ HA.colspan width ] $ display section.section
    header =

in mcons labelCell header
-}

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

data Degree = Positive | Comparative | Superlative
instance showDegree :: Show Degree where
    show Positive = "positive"
    show Comparative = "comparative"
    show Superlative = "superlative"
instance displayDegree :: Display Degree where
    display = display <<< show

data Person = FirstP | SecondP | ThirdP
instance showPerson :: Show Person where
    show FirstP = "1st person"
    show SecondP = "2nd person"
    show ThirdP = "3rd person"
instance displayPerson :: Display Person where
    display = display <<< show

data Number2 = Singular | Plural
instance showNumber2 :: Show Number2 where
    show Singular = "singular"
    show Plural = "plural"
instance displayNumber2 :: Display Number2 where
    display = display <<< show

data Tense = Present | Imperfect | Future | Perfect | Pluperfect | FuturePerfect
instance showTense :: Show Tense where
    show Present = "present"
    show Imperfect = "imperfect"
    show Future = "future"
    show Perfect = "perfect"
    show Pluperfect = "pluperfect"
    show FuturePerfect = "future-perfect"
instance displayTense :: Display Tense where
    display = display <<< show

data Voice = Active | Passive
instance showVoice :: Show Voice where
    show Active = "active"
    show Passive = "passive"
instance displayVoice :: Display Voice where
    display = display <<< show

data Mood = Indicative | Subjunctive
instance showMood :: Show Mood where
    show Indicative = "indicative"
    show Subjunctive = "subjunctive"
instance displayMood :: Display Mood where
    display = display <<< show

celeriter :: CompoundTable Unit Degree Void Void Void String
celeriter = simpleVertical
    { rows: [Positive, Comparative, Superlative]
    , getCell: case _ of
        Positive -> "celeriter"
        Comparative -> "celerius"
        Superlative -> "celerrimē"
    }

headerproduct :: forall majT minT. Array majT -> Array minT -> Headers majT minT
headerproduct major minor =
    map (\label -> { label, sub: minor }) major

getCellVolo :: Mood -> Voice -> Tense -> Number2 -> Person -> String
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

volo :: CompoundTable Mood Voice Tense Number2 Person String
volo = CompoundTable ([TableSection
    { section: Indicative
    , rows: headerproduct [Active] [Present, Imperfect]
    , cols: headerproduct [Singular, Plural] [FirstP, SecondP, ThirdP]
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
