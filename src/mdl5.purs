module MDL.Card where

  import Halogen.HTML.Core (ClassName, className)

  _border :: ClassName
  _border = className "mdl-card--border"

  _expand :: ClassName
  _expand = className "mdl-card--expand"

  actions :: ClassName
  actions = className "mdl-card__actions"

  media :: ClassName
  media = className "mdl-card__media"

  menu :: ClassName
  menu = className "mdl-card__menu"

  subtitleText :: ClassName
  subtitleText = className "mdl-card__subtitle-text"

  supportingText :: ClassName
  supportingText = className "mdl-card__supporting-text"

  title :: ClassName
  title = className "mdl-card__title"

  titleText :: ClassName
  titleText = className "mdl-card__title-text"


