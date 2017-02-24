module MDL.Textfield where

  import Halogen.HTML.Core (ClassName, className)

  _alignRight :: ClassName
  _alignRight = className "mdl-textfield--align-right"

  _expandable :: ClassName
  _expandable = className "mdl-textfield--expandable"

  _floatingLabel :: ClassName
  _floatingLabel = className "mdl-textfield--floating-label"

  _fullWidth :: ClassName
  _fullWidth = className "mdl-textfield--full-width"

  error :: ClassName
  error = className "mdl-textfield__error"

  expandableHolder :: ClassName
  expandableHolder = className "mdl-textfield__expandable-holder"

  input :: ClassName
  input = className "mdl-textfield__input"

  label :: ClassName
  label = className "mdl-textfield__label"


