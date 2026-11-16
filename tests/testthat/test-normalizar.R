fixture_oa <- function() readRDS(file.path("..", "fixtures", "openalex_amostra.rds"))

test_that("test_doi_norm", {
  expect_equal(normalizar_doi(c("https://doi.org/10.1/AB", "doi:10.1/ab", "")),
               c("10.1/ab", "10.1/ab", NA_character_))
})

test_that("test_titulo_norm_acentos", {
  expect_equal(normalizar_titulo("Física na Educação do Campo: um ESTUDO!"),
               "fisica na educacao do campo um estudo")
})

test_that("test_normalizar_fixture", {
  bruto <- fixture_oa()
  r <- normalizar_obras(bruto, "2026-12")
  expect_equal(names(r), names(registros_vazio()))
  expect_equal(nrow(r), nrow(bruto))
  expect_false(any(grepl("^http", r$openalex_id)))
  expect_true(all(grepl("^W", r$openalex_id)))
  expect_false(any(r$tem_resumo[is.na(r$resumo)]))
  expect_true(all(r$status == "para_triagem"))
  expect_true(all(r$rodada_identificacao == "2026-12"))
  expect_type(r$ano, "integer")
  expect_false(any(grepl("^http", r$doi), na.rm = TRUE))
  expect_true(any(nzchar(r$autores)))
  expect_true(any(grepl(";", r$autores)))
})
