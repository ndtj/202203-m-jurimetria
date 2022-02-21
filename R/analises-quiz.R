# aula 02: Câmaras --------------------------------------------------------

dados <- readxl::read_excel("~/Downloads/dados.xlsx") |>
  dplyr::select(where(~!is.logical(.x))) |>
  janitor::clean_names() |>
  purrr::set_names(
    c("id", "start_time", "completion_time", "email",
      "name", "coleta", "prop", "oque")
  )



