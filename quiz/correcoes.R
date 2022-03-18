# funcoes gerais ----------------------------------------------------------

read_quiz <- function(f) {
  id_names <- c("id", "dt_ini", "dt_fim", "email", "nome")
  feedback <- c("fb_positivo", "fb_melhorar", "fb_especifico")
  rx_fb_cols <- "Feedback - |Points - |Total points|Quiz feedback|Grade post"
  base <- f |>
    readxl::read_excel() |>
    dplyr::select(-tidyselect::matches(rx_fb_cols),)
  n_questoes <- ncol(base) - 8
  questoes <- paste0("q", 1:n_questoes)
  purrr::set_names(base, c(id_names, questoes, feedback))
}

corrigir_coluna <- function(questao, coluna, respostas) {
  resp <- respostas[[coluna]]
  n <- length(resp)
  rx <- paste(resp, collapse = "|")
  stringr::str_count(questao, rx) / n
}

corrigir_todas <- function(da, respostas) {
  dplyr::mutate(da, dplyr::across(
    dplyr::starts_with("q"),
    ~corrigir_coluna(.x, dplyr::cur_column(), respostas)
  ))
}


# base geral --------------------------------------------------------------

# pegar base de todos os alunos para dar left join
todos_alunos <- "https://docs.google.com/spreadsheets/d/1gHKw5hKvlu-G3TCMPavTxVnHQzMkaeYx5CAVVVFCtik/edit#gid=0" |>
  googlesheets4::read_sheet() |>
  dplyr::select(turma:nome) |>
  dplyr::filter(!ra %in% "RA00303943")


# quiz 01 -----------------------------------------------------------------

respostas_quiz01 <- list(
  q1 = c(
    "O estudo clássico é dedudivo, enquanto o estudo jurimétrico é indutivo"
  ),
  q2 = c(
    "Conhecer a teoria e estudos anteriores sobre o tema",
    "Elaborar boas perguntas de pesquisa",
    "Estudar como os tribunais organizam e disponibilizam os dados"
  ),
  q3 = c(
    "Porque toda pesquisa jurimétrica passa por ele, a partir do momento que sabemos onde os dados estão"
  )
)

quiz01 <- read_quiz("quiz/Quiz 01_(1-58).xlsx") |>
  # questoes adiadas
  dplyr::select(-q4, -q5) |>
  corrigir_todas(respostas_quiz01) |>
  dplyr::mutate(ra = toupper(stringr::str_extract(email, "[^@]+")))

quiz01 |>
  tidyr::drop_na(fb_positivo) |>
  with(unique(fb_positivo)) |>
  sample()

quiz01 |>
  tidyr::drop_na(fb_melhorar) |>
  with(unique(fb_melhorar)) |>
  sample()

quiz01 |>
  tidyr::drop_na(fb_especifico) |>
  with(unique(fb_especifico)) |>
  sample()

quiz01_sem_feedback <- quiz01 |>
  dplyr::select(ra, tidyselect::starts_with("q"))

# quiz 01 - análises ------------------------------------------------------

# alguém que respondeu não está na linha de presença?
quiz01_sem_feedback |>
  dplyr::anti_join(todos_alunos, "ra")

# quem não respondeu
todos_alunos |>
  dplyr::anti_join(quiz01_sem_feedback, "ra")

# notas
quiz01_sem_feedback |>
  dplyr::summarise(dplyr::across(q1:q3, mean))


