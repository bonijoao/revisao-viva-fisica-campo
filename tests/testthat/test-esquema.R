cols_registros <- c(
  "openalex_id", "openalex_ids_alternativos", "doi", "titulo", "titulo_norm",
  "resumo", "tem_resumo", "resumo_atualizado_em", "ano", "tipo", "idioma",
  "fonte", "autores", "instituicoes", "palavras_chave", "url_oa",
  "rodada_identificacao", "status", "regra_id", "duplicata_de"
)
cols_decisoes <- c("openalex_id", "etapa", "decisao", "motivo", "via", "rodada", "data", "revisor")
cols_rodadas <- c(
  "rodada", "data", "modo", "hash_busca", "hash_regras", "n_identificados",
  "n_duplicatas", "n_excluidos_regra", "n_para_triagem", "n_triados",
  "n_nao_triados_parada", "n_incluidos", "issue"
)

test_that("vazios tem as colunas na ordem do esquema", {
  expect_identical(names(registros_vazio()), cols_registros)
  expect_identical(names(decisoes_vazio()), cols_decisoes)
  expect_identical(names(rodadas_vazio()), cols_rodadas)
  expect_equal(nrow(registros_vazio()), 0L)
  expect_type(registros_vazio()$tem_resumo, "logical")
  expect_type(registros_vazio()$ano, "integer")
  expect_type(rodadas_vazio()$issue, "integer")
})

test_that("salvar e ler estado e uma ida e volta identica", {
  dir <- withr::local_tempdir()
  reg <- registros_vazio()
  reg[1, ] <- list(
    "W1", "W2;W3", "10.1/abc", "Ação \"quântica\" de campos", "acao quantica de campos",
    NA_character_, FALSE, NA_character_, 2020L, "article", "en",
    "Rev. Fis.", "A;B", "UFLA;USP", "campo;gauge", "http://x.org",
    "r1", "incluir", NA_character_, NA_character_
  )
  dec <- decisoes_vazio()
  dec[1, ] <- list("W1", "titulo_resumo", "incluir", NA_character_, "humano", "r1", "2026-11-16", "jp")
  rod <- rodadas_vazio()
  rod[1, ] <- list("r1", "2026-11-16", "completo", "h1", "h2", 10L, 1L, 2L, 7L, 7L, 0L, 3L, NA_integer_)
  estado <- list(registros = reg, decisoes = dec, rodadas = rod)

  salvar_estado(estado, dir)
  volta <- ler_estado(dir)
  expect_identical(volta, estado)
})

test_that("ler_estado em diretorio vazio devolve tabelas vazias", {
  dir <- withr::local_tempdir()
  est <- ler_estado(dir)
  expect_identical(est$registros, registros_vazio())
  expect_identical(est$decisoes, decisoes_vazio())
  expect_identical(est$rodadas, rodadas_vazio())
})

test_that("salvar_estado grava UTF-8, LF e NA como vazio", {
  dir <- withr::local_tempdir()
  salvar_estado(list(registros = registros_vazio(), decisoes = decisoes_vazio(), rodadas = rodadas_vazio()), dir)
  linhas <- readLines(file.path(dir, "registros.csv"), encoding = "UTF-8")
  expect_identical(strsplit(linhas[1], ",")[[1]], cols_registros)
  bytes <- readBin(file.path(dir, "registros.csv"), "raw", 10000)
  expect_false(any(bytes == as.raw(13)))
})
