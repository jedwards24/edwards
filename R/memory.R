#' Get sizes, in Mb, of all objects in global environment
#'
#' Returns table of sizes of all objects in the global environment in Mb (1024^2 B), in decreasing order of size.
#'
#' @return A tibble with two columns: object name and size in Mb.
#' @export
object_size_all <- function(){
  names <- ls(envir = .GlobalEnv)
  mb <- vapply(names, function(x) utils::object.size(get(x, envir = .GlobalEnv)) / 1024^2, numeric(1))
  mb <- sort(mb, decreasing = TRUE)
  tibble::tibble(object = names(mb), Mb = mb)
}
