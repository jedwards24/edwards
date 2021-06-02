# Place to keep older, unused code that might be useful

# Before I added compare_vecs
compare_vars <- function(df, var1, var2) {
  vec1 <- df[, var1, drop=T]
  vec2 <- df[, var2, drop=T]
  if(is.factor(vec1)) vec1 <- levels(vec1)[vec1]
  if(is.factor(vec2)) vec2 <- levels(vec2)[vec2]
  both_num <- is.numeric(vec1) && is.numeric(vec2)
  both_date <- (lubridate::is.Date(vec1) && lubridate::is.Date(vec2)) ||
    (lubridate::is.POSIXt(vec1) && lubridate::is.POSIXt(vec2))
  name1 <- if (is.numeric(var1)) names(df)[var1] else var1
  name2 <- if (is.numeric(var1)) names(df)[var2] else var2
  if(!(both_num || both_date)){
    diff_all <- vec1 == vec2
    diff <- diff_all[!is.na(diff_all)]
    tibble::tibble(comparison = c(paste(name1, "=", name2), paste(name1, "!=", name2), "Missing"),
                   count = c(sum(diff), sum(!diff), length(diff_all) - length(diff)),
                   prop = count / length(diff_all)
    )
  }else{
    diff_all <- vec1 - vec2
    diff <- diff_all[!is.na(diff_all)]
    cat(name1, "has mean", mean(vec1, na.rm = T), "and", name2, "has mean", mean(vec2, na.rm = T), "\n")

    cat("Mean difference (", name1, " - ", name2, ") is ", mean(diff_all, na.rm = T), "\n", sep = "")
    tibble::tibble(comparison = c(paste(name1, ">", name2), paste(name1, "<", name2), paste(name1, "==", name2), "Missing"),
                   count = c(sum(diff > 0), sum(diff < 0), sum(diff == 0), length(diff_all) - length(diff)),
                   prop = count / length(diff_all)
    )
  }
}
