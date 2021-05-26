#' @importFrom magrittr %>%



#' @export
write_school <- function(
  val_filter, table_schools, col_filter, cols_include, names_pretty, password
  ) {

  table_school <-
    table_schools %>%
    dplyr::filter({{ col_filter }} == val_filter)

  path_school <- unique(table_school$path)

  table_school %>%
    dplyr::select({{ cols_include }}) %>%
    purrr::set_names(names_pretty) %>%
    readr::write_excel_csv(path_school, na = "")

  if (!rlang::is_null(password)) {

    shell(
      glue::glue(
        '
      7z.exe a "{path_school}.zip" "{path_school}" -p{password}
      '
      )
    )

    file.remove(path_school)

  }

}



#' @export
write_by_school <- function(
  table_schools, col_filter, cols_include, names_pretty, password = NULL
  ) {

  schools <- table_schools %>% dplyr::pull({{ col_filter }}) %>% unique()

  purrr::walk(
    schools, ~ write_school(
      .,
      table_schools = table_schools,
      col_filter = {{ col_filter }},
      cols_include = {{ cols_include }},
      names_pretty = names_pretty,
      password = password
    )
  )

}
