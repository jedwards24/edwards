############
# object_size_all: returns table of sizes of all object in Mb, in order.
############
#'
#' object_size_all: returns table of sizes of all object in Mb, in order.
#'
#' @return A tibble with two columns: object name and size in Mb.
#' @export
object_size_all2 <- function(){
  names <- ls(envir = .GlobalEnv)
  mb <- vapply(names, function(x) object.size(get(x, envir = .GlobalEnv)) / 10^6, numeric(1))
  mb <- sort(mb, decreasing = TRUE)
  tibble::tibble(object = names(mb), Mb = mb)
}
