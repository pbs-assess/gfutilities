#' Calculate arbitrary quantiles for a particular column by a grouping
#'
#' @param df A [data.frame]
#' @param grp_cols A character vector of the column names to use for grouping the data
#' @param cols A character vector representing column names on which to perform the
#' calculations. They must all be in `df` or an error will be thrown
#' @param probs A vector of quantile probabilities to pass to [stats::quantile()]
#' @param include_mean If TRUE, include the mean in the output as the column `avg`
#'
#' @return A [data.frame] with a new column for each value in the `probs` vector
#' @importFrom purrr set_names partial map map2
#' @importFrom dplyr vars mutate summarize summarize_at group_by
#' @importFrom dplyr group_map select everything ungroup bind_cols
#' @importFrom rlang sym syms
#' @importFrom stats quantile
#' @export
#' @examples
#' library(rlang)
#' library(dplyr)
#' library(purrr)
#' df <- storms %>% filter(year %in% 2000:2005)
#' probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
#' q <- group_quantiles(df, grp_cols = c("year", "month", "hour"),
#'  cols = c("wind", "pressure"), probs = probs)
#' print(head(q, 5))
group_quantiles <- function(df = NULL,
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

  calc_helper <- function(df, cols, probs, include_mean){
    nms <- map(cols, ~{
      paste0(.x, "_", probs)
    })
    # There is some explanation of summarize_at/map/partial here:
    # https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/
    out <- map2(cols, nms, ~{
      col_sym <- sym(.x)
      tmp <- summarize_at(df,
                          vars(!!col_sym),
                          map(probs,
                              ~partial(quantile, probs = .x, na.rm = TRUE))) %>%
        set_names(.y)
      if(include_mean){
        avg <- sym(paste0(.x, "_avg"))
        tmp <- tmp %>%
          mutate(!!avg := mean(df[[.x]]))
      }
      tmp
    }) %>% bind_cols
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
    group_map(~calc_helper(.x, cols, probs, include_mean)) %>%
    map_df(~{.x}) %>%
    bind_cols(grp_cols_df) %>%
    select(!!!grp_cols_sym, everything())
}
