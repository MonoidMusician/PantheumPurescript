module MDL.Snackbar where

  import Halogen.HTML.Core (ClassName, className)

  _active :: ClassName
  _active = className "mdl-snackbar--active"

  action :: ClassName
  action = className "mdl-snackbar__action"

  text :: ClassName
  text = className "mdl-snackbar__text"


