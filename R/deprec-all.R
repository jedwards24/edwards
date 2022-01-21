# Temporary function since renaming.
#' @noRd
count_string <- function(...){
  warning("`count_string() is depreciated. Use count_pattern() instead (renamed).", call. = FALSE)
  count_pattern(...)
}
