module MDL.Switch where

  import Halogen.HTML.Core (ClassName, className)

  focusHelper :: ClassName
  focusHelper = className "mdl-switch__focus-helper"

  input :: ClassName
  input = className "mdl-switch__input"

  label :: ClassName
  label = className "mdl-switch__label"

  rippleContainer :: ClassName
  rippleContainer = className "mdl-switch__ripple-container"

  thumb :: ClassName
  thumb = className "mdl-switch__thumb"

  track :: ClassName
  track = className "mdl-switch__track"


