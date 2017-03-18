module Pantheum.Latin.Inflection where

import Data.Show (class Show, show)
import Pantheum.Inflection.Table (class IsBlank, class Table, CompoundTable(..))
import Prelude ((<<<), Unit)
import UIHelpers (class Display, display)

data Degree = Positive | Comparative | Superlative
instance showDegree :: Show Degree where
    show Positive = "positive"
    show Comparative = "comparative"
    show Superlative = "superlative"
instance displayDegree :: Display Degree where
    display = display <<< show
instance degreeIsntBlank :: IsBlank Degree where
    isblank _ = false

data Case = Nominative | Genitive | Dative | Accusative | Ablative | Vocative | Locative
instance showCase :: Show Case where
    show Nominative = "nominative"
    show Genitive = "genitive"
    show Dative = "dative"
    show Accusative = "accusative"
    show Ablative = "ablative"
    show Vocative = "vocative"
    show Locative = "locative"
instance displayCase :: Display Case where
    display = display <<< show
instance caseIsntBlank :: IsBlank Case where
    isblank _ = false

data Gender = Feminine | Masculine | Neuter
instance showGender :: Show Gender where
    show Feminine = "feminine"
    show Masculine = "masculine"
    show Neuter = "neuter"
instance displayGender :: Display Gender where
    display = display <<< show
instance genderIsntBlank :: IsBlank Gender where
    isblank _ = false

data Person = FirstP | SecondP | ThirdP
instance showPerson :: Show Person where
    show FirstP = "1st person"
    show SecondP = "2nd person"
    show ThirdP = "3rd person"
instance displayPerson :: Display Person where
    display = display <<< show
instance personIsntBlank :: IsBlank Person where
    isblank _ = false

data Numerus = Singular | Plural
instance showNumerus :: Show Numerus where
    show Singular = "singular"
    show Plural = "plural"
instance displayNumerus :: Display Numerus where
    display = display <<< show
instance numerusIsntBlank :: IsBlank Numerus where
    isblank _ = false

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
instance tenseIsntBlank :: IsBlank Tense where
    isblank _ = false

data Voice = Active | Passive
instance showVoice :: Show Voice where
    show Active = "active"
    show Passive = "passive"
instance displayVoice :: Display Voice where
    display = display <<< show
instance voiceIsntBlank :: IsBlank Voice where
    isblank _ = false

data Mood = Indicative | Subjunctive
instance showMood :: Show Mood where
    show Indicative = "indicative"
    show Subjunctive = "subjunctive"
instance displayMood :: Display Mood where
    display = display <<< show
instance moodIsntBlank :: IsBlank Mood where
    isblank _ = false

instance verbTable :: Table Mood Voice Tense Numerus Person String
type NounTable = CompoundTable Unit Unit Case Numerus Gender String
