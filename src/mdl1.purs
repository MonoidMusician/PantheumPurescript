module MDL where
  import Prelude (Unit)
  import Control.Monad.Eff (Eff())

  import DOM (DOM())
  import DOM.HTML.Types (HTMLElement())

  import Halogen.HTML.Core (ClassName, className)
  import Halogen.HTML.Properties.Indexed (IProp(), I)

  class HasMDLInitializer i where
    mdlInitializer :: HTMLElement -> i

  --initializer :: ∀ i r. HasMDLInitializer i => IProp (initializer :: I | r) i
  --initializer = Halogen.HTML.Properties.Indexed.initializer mdlInitializer

  foreign import upgradeElement :: ∀ eff. HTMLElement -> Eff (dom :: DOM | eff) Unit

  jsButton :: ClassName
  jsButton = className "mdl-js-button"

  jsCheckbox :: ClassName
  jsCheckbox = className "mdl-js-checkbox"

  jsIconToggle :: ClassName
  jsIconToggle = className "mdl-js-icon-toggle"

  jsMenu :: ClassName
  jsMenu = className "mdl-js-menu"

  jsProgress :: ClassName
  jsProgress = className "mdl-js-progress"

  jsRadio :: ClassName
  jsRadio = className "mdl-js-radio"

  jsSlider :: ClassName
  jsSlider = className "mdl-js-slider"

  jsSpinner :: ClassName
  jsSpinner = className "mdl-js-spinner"

  jsSwitch :: ClassName
  jsSwitch = className "mdl-js-switch"

  jsTabs :: ClassName
  jsTabs = className "mdl-js-tabs"

  jsTextfield :: ClassName
  jsTextfield = className "mdl-js-textfield"

  jsLayout :: ClassName
  jsLayout = className "mdl-js-layout"

  jsDataTable :: ClassName
  jsDataTable = className "mdl-js-data-table"

  jsRippleEffect :: ClassName
  jsRippleEffect = className "mdl-js-ripple-effect"

  accordion :: ClassName
  accordion = className "mdl-accordion"

  badge :: ClassName
  badge = className "mdl-badge"

  button :: ClassName
  button = className "mdl-button"

  card :: ClassName
  card = className "mdl-card"

  cell :: ClassName
  cell = className "mdl-cell"

  checkbox :: ClassName
  checkbox = className "mdl-checkbox"

  dataTable :: ClassName
  dataTable = className "mdl-data-table"

  dialog :: ClassName
  dialog = className "mdl-dialog"

  dropdownMenu :: ClassName
  dropdownMenu = className "mdl-dropdown-menu"

  grid :: ClassName
  grid = className "mdl-grid"

  iconToggle :: ClassName
  iconToggle = className "mdl-icon-toggle"

  item :: ClassName
  item = className "mdl-item"

  layout :: ClassName
  layout = className "mdl-layout"

  layoutIcon :: ClassName
  layoutIcon = className "mdl-layout-icon"

  layoutSpacer :: ClassName
  layoutSpacer = className "mdl-layout-spacer"

  layoutTitle :: ClassName
  layoutTitle = className "mdl-layout-title"

  list :: ClassName
  list = className "mdl-list"

  logo :: ClassName
  logo = className "mdl-logo"

  megaFooter :: ClassName
  megaFooter = className "mdl-mega-footer"

  menu :: ClassName
  menu = className "mdl-menu"

  miniFooter :: ClassName
  miniFooter = className "mdl-mini-footer"

  navigation :: ClassName
  navigation = className "mdl-navigation"

  progress :: ClassName
  progress = className "mdl-progress"

  radio :: ClassName
  radio = className "mdl-radio"

  ripple :: ClassName
  ripple = className "mdl-ripple"

  slider :: ClassName
  slider = className "mdl-slider"

  snackbar :: ClassName
  snackbar = className "mdl-snackbar"

  spinner :: ClassName
  spinner = className "mdl-spinner"

  switch :: ClassName
  switch = className "mdl-switch"

  tabs :: ClassName
  tabs = className "mdl-tabs"

  textfield :: ClassName
  textfield = className "mdl-textfield"

  tooltip :: ClassName
  tooltip = className "mdl-tooltip"


