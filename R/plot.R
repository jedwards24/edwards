#########################################################################################
# plot_nas: Plots missing values per variable.
#########################################################################################
#'
#' Plot missing values per variable
#'
#' Based on \code{DataExplorer::plot_missing} but simpler.
#'
#' @param df A data frame.
#' @param show_all Logical. Default FALSE doesn't show variables with no NAs.
#' @param sort Logical. Sort variables by number of NAs? Defaults to \code{TRUE}.
#'
#' @export
plot_nas <- function(df, show_all = F, sort = T) {
  tb <- count_nas2(df, all = show_all, sort = sort)
  g <- ggplot(tb, aes(x = reorder(variable, nas), y = nas)) +
    geom_col(alpha = 0.5, fill = "blue") +
    coord_flip() +
    geom_label(label = paste0(round(100 *
                                      tb$prop, 2), "%")) +
    xlab("Features") +
    ylab("Missing Rows")
  print(g)
  invisible(df)
}

#########################################################################################
# plot_missing2: Plots missing values per variable.
#########################################################################################
#'
#' Plot missing values per variable
#'
#' Adaptation of \code{DataExplorer::plot_missing} but with option to not show variables with no NAs.
#' Also has different default lables for each group.
#'
#' For details and main arguments see \code{DataExplorer::plot_missing}.
#'
#' @param data Input data.
#' @param group Missing profile band taking a list of group name and group upper bounds.
#' @param geom_label_args A list of other arguments to \code{geom_label()}.
#' @param title Plot title.
#' @param ggtheme \code{ggplot2} themes.
#' @param theme_config A list of configurations to be passed to \code{theme()}.
#' @param show_all Logical. If \code{FALSE} (default) then do not show variables with no NAs.
#'
#' @export
plot_missing2 <- function (data, group = list(Low = 0.05, OK = 0.4, High = 1),
                           geom_label_args = list(), title = NULL, ggtheme = theme_gray(),
                           theme_config = list(legend.position = c("bottom")), show_all = F) {
  pct_missing <- Band <- NULL
  missing_value <- DataExplorer::profile_missing(data)
  if(!show_all){missing_value <- dplyr::filter(missing_value, num_missing > 0)}
  missing_value <- data.table::data.table(missing_value)
  group <- group[sort.list(unlist(group))]
  invisible(lapply(seq_along(group), function(i) {
    if (i == 1) {
      missing_value[pct_missing <= group[[i]], `:=`(Band,
                                                    names(group)[i])]
    } else {
      missing_value[pct_missing > group[[i - 1]] & pct_missing <=
                      group[[i]], `:=`(Band, names(group)[i])]
    }
  }))
  output <- ggplot(missing_value, aes_string(x = "feature",
                                             y = "num_missing", fill = "Band")) + geom_bar(stat = "identity") +
    scale_fill_discrete("Band") + coord_flip() + xlab("Features") +
    ylab("Missing Rows")
  geom_label_args_list <- list(mapping = aes(label = paste0(round(100 *
                                                                    pct_missing, 2), "%")))
  output <- output + do.call("geom_label", c(geom_label_args_list,
                                             geom_label_args))
  class(output) <- c("single", class(output))
  DataExplorer::plotDataExplorer(plot_obj = output, title = title, ggtheme = ggtheme,
                   theme_config = theme_config)
}
