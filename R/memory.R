############
# object_size_all: returns table of sizes of all object in Mb, in order
############
#'
#' object_size_all: returns table of sizes of all object in Mb, in order
#'
#' @return A tibble with two columns: object name and size in Mb.
#' @export
object_size_all <- function(){
  Mb <- ls(envir=.GlobalEnv) %>% sapply(. %>% get %>% object.size %>% '/'(10^6))
  return(tibble(object=names(Mb), Mb=Mb) %>% arrange(desc(Mb)))
}
