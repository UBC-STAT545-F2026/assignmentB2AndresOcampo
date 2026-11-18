#' Count missing values for all columns by group
#'
#' @description Given a data frame `data` and a column `group_col`,
#' `count_all_missing_by_group()` creates a new data frame with one row
#' per level of `group_col`. The first column contains the levels of
#' `group_col`, and the remaining columns contain the number of missing
#' values for every other column in `data`.
#'
#' @details This function is a wrapper around the `group_by()` and
#' `summarize()` functions in the `dplyr` package.
#'
#' @param data Data in data.frame or tibble format.
#' @param group_col Column in `data` to group by.
#' @param .groups (OPTIONAL) Controls whether the output data frame is
#'   grouped. By default, drops all levels of grouping (`"drop"`). Specify
#'   `"keep"` to keep the output data frame grouped by `group_col`.
#'   See `dplyr::summarize()` documentation for a full list of options.
#'
#' @return A data frame or tibble with the levels of `group_col`, and the
#'   number of NAs within each level of `group_col` for all other columns
#'   in `data`.
#'
#' @export
#'
#' @examples
#' count_all_missing_by_group(airquality, Month)
#' airquality |> count_all_missing_by_group(Month)
#' count_all_missing_by_group(airquality, Month, .groups = "keep")
count_all_missing_by_group <- function(data, group_col, .groups = "drop") {

  if (!is.null(.groups)) {
    if (!(.groups %in% c("drop_last", "drop", "keep", "rowwise"))) {
      stop('.groups must be one of NULL, "drop_last", "drop", "keep", or "rowwise".')
    }
  }

  data |>
    dplyr::group_by({{ group_col }}) |>
    dplyr::summarize(
      dplyr::across(dplyr::everything(), ~ sum(is.na(.x))),
      .groups = .groups
    )
}
