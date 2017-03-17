module Pantheum.Inflection.Table where

import Prelude
import UIHelpers
import Pantheum.Inflection.Table
import Data.Array as Array
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Foldable (maximum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

type Header majT minT =
    { label :: majT
    , sub :: Array minT
    }
type Headers majT minT = Array (Header majT minT)

newtype TableSection sectionT majRT minRT majCT minCT dataT =
    TableSection
        { section :: sectionT
        , rows :: Headers majRT minRT
        , cols :: Headers majCT minCT
        , getCell :: sectionT -> majRT -> minRT -> majCT -> minCT -> dataT
        }

newtype CompoundTable sectionT majRT minRT majCT minCT dataT
    = CompoundTable (Array (TableSection sectionT majRT minRT majCT minCT dataT))


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

--computeCellWidths :: forall majT minT. Header majT minT -> Header (majT, Int) (minT, Int)
