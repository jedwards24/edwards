#########################################################################################
# match_parent: Match model level names to parent variable names
#########################################################################################
#'
#' Match model level names to parent variable names
#'
#' Helper for \code{glmnet_to_table()}. Each level starts with a parent plus maybe more text. Returns
#' vector of parents corresponding to \code{level_names}. Parent is matching parent of greatest length.
#' If no match is found or level is \code{NA} then the parent is the level name.
#'
#' @param level_names Character vector to find parents for.
#' @param parent_names Character vector of candidate parents.
#'
match_parent <- function(level_names, parent_names) {
  nn <- length(level_names)
  parent <- character(nn)
  for (i in 1 : nn){
    matches <- str_detect(level_names[i], paste0("^", parent_names)) %>% parent_names[.]
    if(length(matches) == 0 | any(is.na(matches))){
      parent[i] <- level_names[i]
    }else{
      parent[i] <- matches[which.max(str_length(matches))] #choose longest to avoid substring matching
    }
  }
  parent
}

#########################################################################################
# glmnet_to_table: Summarise coefficients from glmnet in a table
#########################################################################################
#'
#' Summarise coefficients from glmnet in a table
#'
#' Returns a tibble of coefficients for glmnet model \code{fit} with parameter \code{s}. Only
#' coefficients with absolute value greater than \code{min_coef} are included. If \code{var_names} is
#' supplied then a column of "parents" for each coefficient will be added. The parent is the data column
#' from which the variable or factor level is taken (first match in the case of interactions).
#'
#' @param fit A fitted glmnet model.
#' @param var_names (optional) A character vector of column names for data used in \code{fit}.
#' @param s The regularisation parameter determines which model is used from \code{fit} (as used in glmnet).
#' @param min_coef Coefficients with smaller absolute value than this are excluded from the table.
#'
#' @export
glmnet_to_table <- function(fit, var_names = NULL, s="lambda.1se", min_coef=1E-10) {
  ce <- coef(fit, s=s)
  coef_mat <- as.matrix(ce)
  level_names <- rownames(ce)
  tbl <- tibble(name = rownames(coef_mat), coef = coef_mat[, 1])
  if (!is.null(var_names)){
    tbl <- mutate(tbl, parent = match_parent(level_names, var_names)) %>%
      mutate(is_parent = (name == parent))
  }
  filter(tbl, abs(coef) >= min_coef) %>%
    arrange(desc(coef))
}

# This handles models with interactions, splitting the interacting variables to get the parent of each.
# Maybe keep the "name" columns.
# Might be neater ways of doing this with the new dplyr::across().
#########################################################################################
# glmnet_to_table: Summarise coefficients from glmnet in a table
#########################################################################################
#'
#' Summarise coefficients from glmnet in a table
#'
#' Experimental variant of \code{glmnet_to_table()} which splits by interaction when finding parents.
#'
#' @inherit glmnet_to_table
glmnet_to_table2 <- function(fit, var_names = NULL, s="lambda.1se", min_coef=1E-10) {
  ce <- coef(fit, s=s)
  coef_mat <- as.matrix(ce)
  level_names <- rownames(ce)
  tbl <- tibble(name = rownames(coef_mat), coef = coef_mat[, 1])
  if (!is.null(var_names)){
    n_vars <- max(str_count(tbl$name, ":")) + 1
    tbl <- separate(tbl, name, nms, sep = ":", remove = F, fill = "right")
    for (i in 1 : n_vars){
      nm <- paste0("parent", i)
      new_nm <- paste0("name", i)
      tbl <- mutate(tbl, !!sym(nm) :=  match_parent(!!sym(new_nm), names(dt)))
    }
    tbl <- mutate(tbl, is_parent = (name == parent1)) %>%
      select(name, coef, contains("parent"))
  }
  filter(tbl, abs(coef) >= min_coef) %>%
    arrange(desc(coef))
}
