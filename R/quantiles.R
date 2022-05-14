#' Calculate arbitrary quantiles for particular column(s) by group
#'
#' @description
#' Calculate arbitrary quantiles for particular column(s) by group
#' and create a summary [data.frame] which holds a column for each
#' unique combination of values of columns (`cols`) and quantile
#' values (`probs`).
#'
#' @details
#' Create a new [data.frame] which summarizes the one passed as `df`.
#' The summary [data.frame] will have a column for each grouping column
#' (`grp_cols`) and a set of columns for each column name (`cols`), where
#' the set of columns is one for each value of `probs`. New columns will
#' be compose of the name in `cols`, an underscore, and the value in `probs`.
#'
#' @param df A [data.frame]
#' @param grp_cols A character vector of the column names to use for grouping
#' the data
#' @param cols A character vector representing column names on which to perform
#' the calculations
#' @param probs A vector of quantile probabilities to pass to [stats::quantile]
#' @param include_mean If [TRUE], include the mean in the output as the column
#' ending in `*_avg`
#'
#' @return A [data.frame] with a new column for each value in the `probs`
#' vector and a column for the mean in `include_mean` is `TRUE`
#' @importFrom rlang syms sym
#' @importFrom dplyr select ungroup summarize group_by bind_cols
#' @importFrom dplyr vars group_map summarize_at mutate
#' @importFrom purrr map map2 set_names partial
#' @export
summarize_quants <- function(df = NULL,
                             grp_cols = NULL,
                             cols = NULL,
                             probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                             include_mean = TRUE){

  verify_argument(df, "data.frame")
  verify_argument(cols, "character", chk_is_in = names(df))
  verify_argument(grp_cols, "character", chk_is_in = names(df))
  map(cols, ~{
    stopifnot(class(df[[.x]]) == "numeric" | class(df[[.x]]) == "integer")
  })
  verify_argument(probs, "numeric")
  if(any(probs <= 0)){
    stop("`probs` must all be positive values",
         call. = FALSE)
  }
  verify_argument(include_mean, "logical", 1)

  # Perform [stats::quantile] calculations and add a column for each
  # quantile value in `probs`
  #
  # @inheritParams summarize_quants
  # @return A [data.frame] which has a new column for each value of
  calc_helper <- function(df,
                          cols,
                          probs,
                          include_mean){
    nms <- map(cols, ~{
      paste0(.x, "_", probs)
    })
    out <- map2(cols, nms, ~{
      col_sym <- sym(.x)
      tmp <- summarize_at(df,
                          vars(!!col_sym),
                          map(probs,
                              ~partial(quantile,
                                       probs = .x,
                                       na.rm = TRUE))) %>%
        set_names(.y)
      if(include_mean){
        avg <- sym(paste0(.x, "_avg"))
        tmp <- tmp %>%
          mutate(!!avg := mean(df[[.x]]))
      }
      tmp
    }) %>%
      bind_cols
    out
  }

  grp_cols_sym <- syms(grp_cols)
  # Create a data frame of the grouped columns only, which are bound back
  # on after the quantile columns are calculated
  grp_cols_df <- df %>%
    group_by(!!!grp_cols_sym) %>%
    summarize(tmp = 1) %>%
    ungroup() %>%
    select(-tmp)

  df %>%
    group_by(!!!grp_cols_sym) %>%
    group_map(~calc_helper(.x, cols, probs, include_mean)) %>%
    map_df(~{.x}) %>%
    bind_cols(grp_cols_df) %>%
    select(!!!grp_cols_sym, everything())
}
