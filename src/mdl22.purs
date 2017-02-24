module MDL.Menu where

  import Halogen.HTML.Core (ClassName, className)

  _bottomRight :: ClassName
  _bottomRight = className "mdl-menu--bottom-right"

  _topLeft :: ClassName
  _topLeft = className "mdl-menu--top-left"

  _topRight :: ClassName
  _topRight = className "mdl-menu--top-right"

  _unaligned :: ClassName
  _unaligned = className "mdl-menu--unaligned"

  container :: ClassName
  container = className "mdl-menu__container"

  item :: ClassName
  item = className "mdl-menu__item"

  outline :: ClassName
  outline = className "mdl-menu__outline"


