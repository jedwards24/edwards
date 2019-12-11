############
# object_size_all: Returns table of sizes of all object in Mb, in order.
############
#'
#' Get sizes of all objects in global environment
#'
#' Returns table of sizes of all objects in the global environment in Mb, in decreasing order of size.
#'
#' @return A tibble with two columns: object name and size in Mb.
#' @export
object_size_all <- function(){
  names <- ls(envir = .GlobalEnv)
  mb <- vapply(names, function(x) object.size(get(x, envir = .GlobalEnv)) / 10^6, numeric(1))
  mb <- sort(mb, decreasing = TRUE)
  tibble::tibble(object = names(mb), Mb = mb)
}
