reg <- function(id, doi = NA, titulo = "Titulo", ano = 2020L, resumo = "r",
                alt = NA, status = "para_triagem", rodada = "2026-11") {
  r <- registros_vazio()[1, ]
  r[] <- NA
  r$openalex_id <- id
  r$openalex_ids_alternativos <- alt
  r$doi <- normalizar_doi(doi)
  r$titulo <- titulo
  r$titulo_norm <- normalizar_titulo(titulo)
  r$resumo <- resumo
  r$tem_resumo <- !is.na(resumo) & nzchar(resumo)
  r$ano <- as.integer(ano)
  r$rodada_identificacao <- rodada
  r$status <- status
  r
}

test_that("test_dedup_id_existente", {
  ex <- reg("W1")
  r <- deduplicar(reg("W1"), ex)
  expect_equal(nrow(r$novos), 0)
  expect_equal(nrow(r$atualizados), 0)
})

test_that("test_dedup_id_alternativo", {
  ex <- reg("W1", alt = "W9;W8")
  r <- deduplicar(reg("W9", titulo = "Outro", doi = "10.9/x"), ex)
  expect_equal(nrow(r$novos), 0)
})

test_that("test_dedup_doi_id_novo", {
  ex <- reg("W1", doi = "10.1/ab", titulo = "A")
  r <- deduplicar(reg("W2", doi = "https://doi.org/10.1/AB", titulo = "B", ano = 1999L), ex)
  expect_equal(nrow(r$atualizados), 1)
  expect_equal(r$atualizados$openalex_id, "W1")
  expect_true("W2" %in% strsplit(r$atualizados$openalex_ids_alternativos, ";")[[1]])
  expect_equal(nrow(r$novos), 1)
  expect_equal(r$novos$status, "duplicata")
  expect_equal(r$novos$duplicata_de, "W1")
  expect_equal(r$novos$openalex_id, "W2")
})

test_that("test_dedup_titulo_ano", {
  ex <- reg("W1", titulo = "Física na Escola")
  r <- deduplicar(reg("W2", titulo = "FISICA NA ESCOLA!", ano = 2020L), ex)
  expect_equal(r$novos$status, "duplicata")
  expect_equal(r$atualizados$openalex_ids_alternativos, "W2")
  r2 <- deduplicar(reg("W3", titulo = "FISICA NA ESCOLA!", ano = 2021L), ex)
  expect_equal(r2$novos$status, "para_triagem")
  expect_equal(nrow(r2$atualizados), 0)
})

test_that("test_dedup_dentro_do_lote", {
  lote <- rbind(reg("W1", doi = "10.1/a"), reg("W2", doi = "10.1/a", titulo = "Z"))
  r <- deduplicar(lote, registros_vazio())
  expect_equal(r$novos$status, c("para_triagem", "duplicata"))
  expect_equal(r$novos$duplicata_de, c(NA, "W1"))
  expect_equal(nrow(r$atualizados), 0)
})

test_that("test_resumo_tardio", {
  ex <- reg("W1", resumo = NA)
  r <- deduplicar(reg("W1", resumo = "agora tem"), ex)
  expect_equal(nrow(r$novos), 0)
  expect_equal(nrow(r$atualizados), 1)
  expect_equal(r$atualizados$resumo, "agora tem")
  expect_true(r$atualizados$tem_resumo)
  expect_equal(r$atualizados$resumo_atualizado_em, "2026-11")
})

test_that("alternativo e resumo tardio no mesmo existente geram uma linha", {
  ex <- reg("W1", doi = "10.1/a", resumo = NA)
  lote <- rbind(reg("W1", doi = "10.1/a", resumo = "novo", rodada = "2026-12"),
                reg("W2", doi = "10.1/a", resumo = "x", rodada = "2026-12"))
  r <- deduplicar(lote, ex)
  expect_equal(nrow(r$atualizados), 1)
  expect_equal(r$atualizados$resumo, "novo")
  expect_equal(r$atualizados$openalex_ids_alternativos, "W2")
  expect_equal(r$atualizados$resumo_atualizado_em, "2026-12")
  expect_equal(r$novos$openalex_id, "W2")
})

test_that("existentes vazio nao quebra", {
  r <- deduplicar(reg("W1"), registros_vazio())
  expect_equal(r$novos$status, "para_triagem")
  expect_equal(nrow(r$atualizados), 0)
})
