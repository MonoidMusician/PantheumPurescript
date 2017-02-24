module MDL.Checkbox where

  import Halogen.HTML.Core (ClassName, className)

  boxOutline :: ClassName
  boxOutline = className "mdl-checkbox__box-outline"

  focusHelper :: ClassName
  focusHelper = className "mdl-checkbox__focus-helper"

  input :: ClassName
  input = className "mdl-checkbox__input"

  label :: ClassName
  label = className "mdl-checkbox__label"

  rippleContainer :: ClassName
  rippleContainer = className "mdl-checkbox__ripple-container"

  tickOutline :: ClassName
  tickOutline = className "mdl-checkbox__tick-outline"


