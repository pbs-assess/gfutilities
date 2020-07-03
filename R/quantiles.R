#' Calculate arbitrary quantiles for a particular column by a grouping
#'
#' @param df A [data.frame]
#' @param grp_cols A character vector of the column names to use for grouping the data
#' @param col A character string representing a column name on which to perform the
#' calculations. Must be in `df` or an error will be thrown
#' @param probs A vector of quantile probabilities to pass to [stats::quantile()]
#' @param include_mean If TRUE, include the mean in the output as the column `avg`
#' @param prepend_colname If TRUE, the name of the column used in the quantile calculation
#' will be prepended to each quantile value column name
#'
#' @return A [data.frame] with a new column for each value in the `probs` vector
#' @importFrom purrr set_names partial map
#' @importFrom dplyr vars mutate summarize_at group_by group_map select everything
#' @importFrom rlang sym syms
#' @importFrom stats quantile
#' @export
#' @examples
#' library(rlang)
#' library(dplyr)
#' library(purrr)
#' df <- storms %>% filter(year %in% 2000:2005)
#' probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
#' group_quantiles(df, grp_col = c("year", "month", "hour"), col = "wind", probs = probs)
group_quantiles <- function(df = NULL,
                            grp_cols = NULL,
                            col = NULL,
                            probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                            include_mean = TRUE,
                            prepend_colname = TRUE){

  verify_argument(df, "data.frame")
  verify_argument(col, "character", chk_is_in = names(df))
  verify_argument(grp_cols, "character", chk_is_in = names(df))
  stopifnot(class(df[[col]]) == "numeric" | class(df[[col]]) == "integer")
  verify_argument(probs, "numeric")
  if(any(probs <= 0)){
    stop("`probs` must all be positive values",
         call. = FALSE)
  }
  verify_argument(include_mean, "logical", 1)

  calc_helper <- function(df, col, probs, include_mean){
    nms <- probs
    if(prepend_colname){
      nms <- paste0(col, "_", probs)
    }
    col_sym <- sym(col)
    # There is some explanation of summarize_at/map/partial here:
    # https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/
    out <- summarize_at(df,
                        vars(!!col_sym),
                        map(probs,
                            ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
                          set_names(nms))
    if(include_mean){
      avg <- sym(ifelse(prepend_colname, paste0(col, "_avg"), "avg"))
      out <- out %>%
        mutate(!!avg := mean(df[[col]]))
    }
    out
  }

  grp_cols_sym <- syms(grp_cols)
  # Create a data frame of the grouped columns only, which are bound back on after the
  # quantile columns are calculated
  grp_cols_df <- df %>%
    group_by(!!!grp_cols_sym) %>%
    summarize(tmp = 1) %>%
    ungroup() %>%
    select(-tmp)
  df %>%
    group_by(!!!grp_cols_sym) %>%
    group_map(~calc_helper(.x, col, probs, include_mean)) %>%
    map_df(~{.x}) %>%
    bind_cols(grp_cols_df) %>%
    select(!!!grp_cols_sym, everything())
}
