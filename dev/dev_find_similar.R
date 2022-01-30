
find_similar_single2 <- function(df){
  classes <- purrr::map_chr(df, class) %>%
    unname()
  nm <- names(df)
  niter <- length(nm) - 1
  res_list <- vector("list", niter)
  for (i in 1 : niter){
    x <- dplyr::pull(df, i)
    inds_y <- which(classes[i] == classes) %>% .[. > i]
    res_list[[i]] <- purrr::map_dfr(inds_y, ~similar_col_single(x, df[[.]], names(df)[.])) %>%
      dplyr::mutate(var1 = nm[i]) %>%
      dplyr::mutate(class = classes[i])
  }
    dplyr::bind_rows(res_list) %>%
      dplyr::select(var1, var2, class, everything()) %>%
      dplyr::mutate(diff = nrow(df) - match - both_na - na_1 - na_2) %>%
      dplyr::mutate(prop_match_nz = (match - match_zero) / (nrow(df) - match_zero - both_na - na_1 - na_2))
}

similar_col_single <- function(x, y, name_y) {
  tibble::tibble(var2 = name_y,
                 match = sum(x == y, na.rm = T),
                 match_zero = sum(x == 0 & y == 0, na.rm = T),
                 both_na = sum(is.na(x) & is.na(y)),
                 na_1 = sum(is.na(x) & !is.na(y)),
                 na_2 = sum(!is.na(x) & is.na(y)))
}

find_similar_single3 <- function(df){
  classes <- purrr::map_chr(df, class) %>%
    unname()
  nm <- names(df)
  niter <- length(nm) - 1
  res_list <- vector("list", niter)
  for (i in 1 : niter){
    x <- dplyr::pull(df, i)
    inds_y <- which(classes[i] == classes) %>% .[. > i]
    if (length(inds_y) == 0) next()
    res_mat <- purrr::map(inds_y, ~similar_col_single3(x, df[[.]])) %>%
      do.call(rbind, .)
    colnames(res_mat) <- c("match", "match_zero", "both_na", "na_1", "na_2")
    res_list[[i]] <- as_tibble(res_mat) %>%
      dplyr::mutate(var1 = nm[i]) %>%
      dplyr::mutate(var2 = nm[inds_y]) %>%
      dplyr::mutate(class = classes[i])
  }
  dplyr::bind_rows(res_list) %>%
    dplyr::select(var1, var2, class, everything()) %>%
    dplyr::mutate(diff = nrow(df) - match - both_na - na_1 - na_2) %>%
    dplyr::mutate(prop_match_nz = (match - match_zero) / (nrow(df) - match_zero - both_na - na_1 - na_2))
}

similar_col_single3 <- function(x, y) {
  c(sum(x == y, na.rm = T),
    sum(x == 0 & y == 0, na.rm = T),
    sum(is.na(x) & is.na(y)),
    sum(is.na(x) & !is.na(y)),
    sum(!is.na(x) & is.na(y)))
}

find_similar_single4 <- function(df){
  classes <- purrr::map_chr(df, class)
  niter <- ncol(df) - 1
  res_list <- vector("list", niter)
  for (i in 1 : niter){
    inds_y <- which(classes[i] == classes) %>% .[. > i]
    if (length(inds_y) == 0) next()
    x <- dplyr::pull(df, i)
    df_y <- dplyr::select(df, all_of(inds_y))
    res_list[[i]] <- compare_cols_to_vector(x, df_y, names(df)[i])
  }
  dplyr::bind_rows(res_list) %>%
    dplyr::mutate_all(unname) %>%
    dplyr::mutate(diff = nrow(df) - match - both_na - na_1 - na_2) %>%
    dplyr::mutate(prop_match_nz = (match - match_zero) / (nrow(df) - match_zero - both_na - na_1 - na_2))
}

compare_cols_to_vector <- function(x, df_y, name_x) {
  match <- purrr::map_int(df_y, ~sum(. == x, na.rm = TRUE))
  match_zero <- purrr::map_int(df_y, ~sum(x == 0 & . == 0, na.rm = TRUE))
  both_na <- purrr::map_int(df_y, ~sum(is.na(x) & is.na(.)))
  na_1 <- purrr::map_int(df_y, ~sum(is.na(x) & !is.na(.)))
  na_2 <- purrr::map_int(df_y, ~sum(!is.na(x) & is.na(.)))
  tibble::tibble(var1 = name_x,
                 var2 = names(df_y),
                 class = class(x),
                 match,
                 match_zero,
                 both_na,
                 na_1,
                 na_2)
}
