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
import Data.NonEmpty (NonEmpty(..), fromNonEmpty, (:|))
import Data.Tuple (Tuple(..))

fromNonEmptyArray :: forall a. NonEmpty Array a -> Array a
fromNonEmptyArray = fromNonEmpty Array.cons

nonEmptyArrayLength :: forall a. NonEmpty Array a -> Int
nonEmptyArrayLength (_ :| a) = Array.length a + 1

class IsUnit t where
    isunit :: t -> Boolean

instance unitIsUnit :: IsUnit Unit where
    isunit unit = true

type Header majT minT =
    { label :: majT
    , sub :: NonEmpty Array minT
    }
type Headers majT minT = NonEmpty Array (Header majT minT)

blankHeaders :: Headers Unit Unit
blankHeaders = { label: unit, sub: unit :| [] } :| []

simpleHeaders :: forall minT. NonEmpty Array minT -> Headers Unit minT
simpleHeaders headers = { label: unit, sub: headers } :| []

isSimpleHeaders :: forall majT minT. (IsUnit majT) => Headers majT minT -> Boolean
isSimpleHeaders = Array.all (\{ label } -> isunit label)

isBlankHeaders :: forall majT minT. (IsUnit majT, IsUnit minT) => Headers majT minT -> Boolean
isBlankHeaders = Array.all (case _ of
    { label, sub: s :| [] } -> isunit label && isunit s
    _ -> false
)

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

simpleVertical :: forall minRT dataT.
    { rows :: NonEmpty Array minRT
    , getCell :: minRT -> dataT
    } -> CompoundTable Unit Unit minRT Unit Unit dataT
simpleVertical { rows, getCell } =
    { section: unit
    , rows: fullRows
    , cols: blankHeaders
    , getCell: \_ _ row _ _ -> getCell row
    } # TableSection # Array.singleton # CompoundTable
    where
        fullRows =
            map (\label -> { label: unit, sub: label :| [] }) rows

computeGutterWidth :: forall sectionT majRT minRT majCT minCT dataT
     . (IsUnit majRT, IsUnit minRT)
    => CompoundTable sectionT majRT minRT majCT minCT dataT
    -> Int
computeGutterWidth (CompoundTable sections) =
    sections # map (\(TableSection { rows }) ->
        if isBlankHeaders rows then 0
        else if isSimpleHeaders rows then 1
        else 2
    ) # maximum # fromMaybe 0


mcons :: forall a. Maybe a -> Array a -> Array a
mcons (Just head) tail = Array.cons head tail
mcons Nothing tail = tail

instance displayTable :: (
    Display sectionT,
    Display majRT,
    Display minRT,
    Display majCT,
    Display minCT,
    Display dataT,
    IsUnit majRT,
    IsUnit minRT,
    IsUnit majCT,
    IsUnit minCT
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
                    HH.th [ HP.colSpan $ max 1 $ nonEmptyArrayLength sub ] >< display label
                )
            header = mcons labelCellM (fromNonEmptyArray headerMajCols)
            subcol2s =
                section.cols # fromNonEmptyArray # Array.concatMap (\{ label, sub } ->
                    map (Tuple label) (fromNonEmptyArray sub)
                )
            continue majRow minRow =
                subcol2s # map (\(Tuple majCol minCol) ->
                    HH.td_ >< display $ section.getCell section.section majRow minRow majCol minCol
                )
            subheaderM =
                if Array.any (\{ label } -> not $ isunit label) section.cols
                then
                    section.cols # fromNonEmptyArray # Array.concatMap (\{ sub } ->
                        map (\h -> HH.th_ >< display h) (fromNonEmptyArray sub)
                    ) # Just
                else Nothing
            headerrows =
                case subheaderM of
                    Nothing -> [ header ]
                    Just subheader ->
                        [ header, mcons padCellM subheader ]
            simplerows = isSimpleHeaders section.rows
            contentrows =
                section.rows # fromNonEmptyArray # Array.concatMap mkrow
            mkrow { label, sub } =
                    let
                        subrows = map (\sublabel -> [ HH.th_ [], HH.th_ [ display sublabel ] ] <> continue label sublabel) $ fromNonEmptyArray sub
                    in [ [ mkMajRowH $ display label ] ] <> subrows
            rows = headerrows <> contentrows
    display _ = display "table"

--computeCellWidths :: forall majT minT. Header majT minT -> Header (majT, Int) (minT, Int)
