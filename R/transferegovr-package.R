#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang %||%
## usethis namespace: end
NULL

# Silences the R CMD check note for objects loaded from sysdata.rda.
utils::globalVariables(c(
  ".tg_schema", ".tg_module_labels", ".tg_module_aliases", ".tg_schema_built_at"
))
