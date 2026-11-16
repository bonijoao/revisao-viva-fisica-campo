test_that("exportar_asreview grava as colunas esperadas", {
  r <- registros_vazio()[rep(NA_integer_, 2), ]
  r$openalex_id <- c("W1", "W2"); r$titulo <- c("A", "B")
  r$resumo <- c("res", NA); r$doi <- c("10.1/x", NA); r$url_oa <- c(NA, "u")
  f <- tempfile(fileext = ".csv")
  expect_equal(exportar_asreview(r, f), f)
  l <- utils::read.csv(f, stringsAsFactors = FALSE, na.strings = character())
  expect_equal(names(l), c("openalex_id", "title", "abstract", "doi", "url"))
  expect_equal(l$title, c("A", "B"))
  expect_equal(l$abstract[2], "")
})

test_that("test_asreview_rotulos", {
  ex <- tibble::tibble(openalex_id = c("W1", "W2", "W3"), included = c(1, 0, NA))
  d <- importar_asreview(ex, c("W1", "W2", "W3"), "2026-12", "2026-12-05", "r")
  expect_equal(d$decisao, c("incluir", "excluir", "nao_triado_parada"))
  expect_equal(names(d), names(decisoes_vazio()))
  expect_true(all(d$via == "asreview" & d$etapa == "titulo_resumo"))
})

test_that("rotulo em outras colunas e texto", {
  ex <- tibble::tibble(openalex_id = c("W1", "W2"), label_included = c("1", ""))
  d <- importar_asreview(ex, c("W1", "W2"), "2026-12", "2026-12-05", "r")
  expect_equal(d$decisao, c("incluir", "nao_triado_parada"))
})

test_that("test_asreview_id_ausente", {
  ex <- tibble::tibble(openalex_id = "W1", included = 1)
  d <- importar_asreview(ex, c("W1", "W9"), "2026-12", "2026-12-05", "r")
  expect_equal(d$openalex_id, c("W1", "W9"))
  expect_equal(d$decisao, c("incluir", "nao_triado_parada"))
})

test_that("test_asreview_sem_coluna_rotulo", {
  ex <- tibble::tibble(openalex_id = "W1", titulo_x = "a")
  expect_error(importar_asreview(ex, "W1", "2026-12", "2026-12-05", "r"), "titulo_x")
})

test_that("sem coluna openalex_id da erro listando colunas", {
  ex <- tibble::tibble(id_novo = "W1", included = 1)
  expect_error(importar_asreview(ex, "W1", "2026-12", "2026-12-05", "r"), "id_novo")
})

test_that("ids_enviados duplicados sao deduplicados", {
  ex <- tibble::tibble(openalex_id = "W1", included = 1)
  d <- importar_asreview(ex, c("W1", "W1"), "2026-12", "2026-12-05", "r")
  expect_equal(nrow(d), 1)
})
