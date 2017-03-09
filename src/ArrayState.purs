module ArrayState where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.List.Trans (ListT, nil)
import Control.Monad.State (State)
import Control.Monad.State.Trans (StateT, evalStateT, get, lift, put)
import Control.MonadPlus (guard)
import Data.Array (reverse, (..))
import Data.Generic (class Generic, gCompare, gEq, gShow)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), indexOf, joinWith, split, trim)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (global, ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (sequence)
import Node.ReadLine.Question (raw)
import Control.Monad.Loops (whileJust_)


newtype Line =
    Line (Array Res)

instance showLine :: Show Line where
    show (Line line) = joinWith "" do
        res <- line
        pure case res of
            Punct s -> s
            Verb { syllables } ->
                joinWith "" do
                    { value, stype } <- syllables
                    pure (value <> show stype)

data Res
    = Punct String
    | Verb Word


type Word =
    { syllables :: Array Syllable
    , gloss :: String
    }


type Syllable =
    { value :: String
    , stype :: SyllableType
    }


data SyllableType
    = Long
    | Ambiguous (Maybe Boolean)
    | Short
    | Elided

instance showSyllableType :: Show SyllableType where
    show s =
        case s of
            Short ->
                "˘"

            Long ->
                "¯"

            Elided ->
                "˙"

            Ambiguous Nothing ->
                "˟"

            Ambiguous (Just b)
                | b -> "˜"
                | otherwise -> "ˇ"

startsWith :: Pattern -> String -> Boolean
startsWith p s = case indexOf p s of
    Just 0 -> true
    _ -> false

weight :: String -> ScanBias
weight syllable =
    if startsWith (Pattern "x") syllable || startsWith (Pattern "z") syllable then
        IntoDoubleConsonant
    else if test r_cx syllable then
        IntoConsonant
    else
        IntoVowel

ugly :: forall i r. Array (State i r) -> i -> Array r
ugly monad init =
    case evalStateT (sequence monad) init of
        Identity res -> res

ugly2 :: forall i r. Array (State i r) -> i -> Array r
ugly2 monad init =
    case evalStateT (sequence (reverse monad)) init of
        Identity res -> reverse res

data ScanBias = Elidable | IntoVowel | IntoConsonant | IntoDoubleConsonant | LineEnd
derive instance genericScanBias :: Generic ScanBias

instance showScanBias :: Show ScanBias where
  show = gShow

instance eqScanBias :: Eq ScanBias where
  eq = gEq

instance ordScanBias :: Ord ScanBias where
  compare = gCompare

reform :: Syllable -> ScanBias -> SyllableType
reform syllable bias =
    case bias of
        Elidable | test r_elision syllable.value ->
            Elided
        IntoDoubleConsonant ->
            Long
        _ -> case syllable.stype of
            Ambiguous Nothing
                | bias == IntoVowel
                || bias == Elidable ->
                    Ambiguous $ Just false
                | bias == IntoConsonant ->
                    Ambiguous $ Just true
            st -> st

inner :: Syllable -> State ScanBias Syllable
inner syllable = do
    bias <- get
    put (weight syllable.value)
    let stype = reform syllable bias
    pure ({ value: syllable.value {-<> show bias-}, stype })

middle :: Array Syllable -> State ScanBias (Array Syllable)
middle syllables = map reverse $ sequence $ reverse $ map inner syllables

outer :: Res -> State ScanBias Res
outer (Verb w) = do
    bias <- get
    when (bias == IntoVowel) (put Elidable)
    syllables <- middle w.syllables
    pure $ Verb { syllables, gloss: w.gloss }
outer res = pure res

rescan :: Line -> Line
rescan (Line line) = Line $ ugly2 (map outer line) LineEnd


v :: String
v =
    "(?:(?:a[eu]|oe)(?![̄̈])|[aeiouyāēīōūȳ]̄?|[aeiouy]̄|[äëïöüÿ])"

c :: String
c =
    "(?:[qg]u(?=" <> v <> ")|[ck]h|[bcdfghjklmnprstvwxz]|\\bi(?=" <> v <> "))"

l :: String
l =
    "(?:" <> c <> "|" <> v <> ")"

r_word :: Regex
r_word =
    unsafeRegex ("((?:" <> c <> "|" <> v <> ")+)") (ignoreCase <> global)

r_syllable :: Regex
r_syllable =
    unsafeRegex (c <> "*" <> v <> "(" <> c <> "*$|(?![dbpckgt][rl])" <> c <> "((?=" <> c <> "+" <> v <> "?))" <> ")?") (ignoreCase <> global)

r_short :: Regex
r_short =
    unsafeRegex ("(\\bi?|[^aeiouy]|[qg]u)[aeiouy]$") ignoreCase

r_long :: Regex
r_long =
    unsafeRegex ("[aeiouy]̄|[āēīōūȳ]|a[eu]|oe|(x|z|" <> c <> c <> ")$") ignoreCase

r_cx :: Regex
r_cx =
    unsafeRegex ("^((?!h)" <> c <> "|[ij]" <> v <> "|[aeiouy]̄|[äëïöüÿ])") ignoreCase

r_elision :: Regex
r_elision =
    unsafeRegex (v <> "m?$") ignoreCase

r_wordbreak :: Regex
r_wordbreak =
    unsafeRegex ("(?:(?:^|" <> l <> ")" <> l <> "*(?=))") ignoreCase

foreign import _findall :: Regex -> String -> Array String

mksyllables :: String -> Array Syllable
mksyllables content =
    content # _findall r_syllable # map mksyllable
    --content # Regex.find Regex.All r_syllable # map (_.match >>> syllable)
    --content # Regex.match r_syllable # fromMaybe [] # map (_.match >>> syllable)

mksyllable :: String -> Syllable
mksyllable s =
    { value: s
    , stype:
        if test r_short s then
            Short
        else if test r_long s then
            Long
        else
            Ambiguous Nothing
    }

mkline :: String -> Line
mkline content =
    content # mkwords # Line # rescan

mklines :: String -> Array Line
mklines content =
    trim content # split (Pattern "\n") # map (trim >>> mkline)


mkwords :: String -> Array Res
mkwords content =
    content # _findall r_word # map (\s -> Verb { syllables: mksyllables s, gloss: s})
    --processtext (\s -> Verb $ { syllables: syllables s, gloss: s }) Punct content

prescanned :: Line
prescanned = Line $ mkwords "arma virumque canō, Trōjae quī prīmus ab ōrīs"

rescanned :: Line
rescanned = rescan prescanned

-- Find Pythagorean triples using an array comprehension.
triples_ :: Int -> ListT (State Int) (Array Int)
triples_ n = do
  i <- lift get
  lift $ put (i + 1)
  pure [i]
  nil
  pure [i+1]

triples' :: Int -> Array (State Int (Array Int))
triples' n = do
  z <- 1 .. n
  y <- 1 .. z
  x <- 1 .. y
  guard $ x * x + y * y == z * z
  pure do
    i <- get
    put (i + 1)
    pure [i,x,y,z]

triples :: Int -> Array (Array Int)
--triples = (flip evalStateT 0) <<< _triples
--triples = (flip evalStateT 0) <<< foldl (flip cons) [] <<< triples_
--triples = const [[2]]
triples = (\(Identity i) -> i) <<< (flip evalStateT 0) <<< sequence <<< triples'

_triples :: Int -> StateT Int Array (Array Int)
_triples n = do
  z <- lift $ 1 .. n
  y <- lift $ 1 .. z
  x <- lift $ 1 .. y
  guard $ x * x + y * y == z * z
  i <- get
  put (i + 1)
  pure [i, x, y, z]

--main :: forall e. Eff ( console :: CONSOLE, err :: EXCEPTION, readline :: READLINE | e ) Unit
--main = for_ (triples 20) logShow
{-
main = do
    logShow prescanned
    logShow rescanned
-}

main = launchAff $ do
    liftEff $ logShow prescanned
    liftEff $ logShow rescanned
    whileJust_
        (raw "> " # map case _ of
            "" -> Nothing
            line -> Just line
        )
        (mkline >>> logShow >>> liftEff)
