module MDL.DataTable.Header where

  import Halogen.HTML.Core (ClassName, className)

  _sortedAscending :: ClassName
  _sortedAscending = className "mdl-data-table__header--sorted-ascending"

  _sortedDescending :: ClassName
  _sortedDescending = className "mdl-data-table__header--sorted-descending"


