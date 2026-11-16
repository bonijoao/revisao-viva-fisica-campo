linha_dec <- function(id, decisao, data = "2026-12-01", rodada = "r1", via = "issue",
                      etapa = "titulo_resumo") {
  tibble::tibble(openalex_id = id, etapa = etapa, decisao = decisao, motivo = "",
                 via = via, rodada = rodada, data = data, revisor = "x")
}

test_that("registrar substitui a mesma rodada", {
  d <- registrar_decisoes(decisoes_vazio(), linha_dec("W1", "incluir"))
  d <- registrar_decisoes(d, linha_dec("W1", "excluir"))
  expect_equal(nrow(d), 1)
  expect_equal(d$decisao, "excluir")
  expect_equal(names(d), names(decisoes_vazio()))
})

test_that("registrar mantem outras rodadas", {
  d <- registrar_decisoes(decisoes_vazio(), linha_dec("W1", "incluir", rodada = "r1"))
  d <- registrar_decisoes(d, linha_dec("W1", "excluir", rodada = "r2"))
  expect_equal(nrow(d), 2)
})

test_that("vigente usa a decisao mais recente", {
  d <- dplyr::bind_rows(linha_dec("W1", "excluir", "2026-12-01", rodada = "r1"),
                        linha_dec("W1", "incluir", "2027-01-01", rodada = "r2"))
  v <- decisao_vigente(d)
  expect_equal(nrow(v), 1)
  expect_equal(v$decisao, "incluir")
  expect_equal(names(v), c("openalex_id", "decisao", "via", "rodada", "data"))
})

test_that("vigente: empate fica com a ultima, filtra etapa, vazio ok", {
  d <- dplyr::bind_rows(linha_dec("W1", "excluir"), linha_dec("W1", "incluir"),
                        linha_dec("W2", "incluir", etapa = "texto_completo"))
  v <- decisao_vigente(d)
  expect_equal(v$openalex_id, "W1")
  expect_equal(v$decisao, "incluir")
  e <- decisao_vigente(decisoes_vazio())
  expect_equal(nrow(e), 0)
  expect_equal(names(e), c("openalex_id", "decisao", "via", "rodada", "data"))
})

test_that("registrar rejeita data NA, vazia ou fora do formato", {
  expect_error(registrar_decisoes(decisoes_vazio(), linha_dec("W9", "incluir", data = NA_character_)), "W9")
  expect_error(registrar_decisoes(decisoes_vazio(), linha_dec("W8", "incluir", data = "")), "W8")
  expect_error(registrar_decisoes(decisoes_vazio(),
                                  linha_dec("W7", "incluir", data = "2026-12-01T12:00:00-03:00")), "W7")
  expect_no_error(registrar_decisoes(decisoes_vazio(),
                                     linha_dec("W6", "incluir", data = "2026-12-01T12:00:00Z")))
})

test_that("vigente: data simples vs datetime UTC no mesmo dia", {
  d <- dplyr::bind_rows(linha_dec("W1", "incluir", "2026-12-01T10:00:00Z", rodada = "r2"),
                        linha_dec("W1", "excluir", "2026-12-01", rodada = "r1"))
  expect_equal(decisao_vigente(d)$decisao, "incluir")
})

test_that("vigente rejeita data NA e ignora etapa NA", {
  d <- dplyr::bind_rows(linha_dec("W1", "incluir"), linha_dec("W1", "excluir", data = NA_character_))
  expect_error(decisao_vigente(d), "W1")
  d2 <- dplyr::bind_rows(linha_dec("W1", "incluir"), linha_dec("W2", "incluir", etapa = NA_character_))
  v <- decisao_vigente(d2)
  expect_equal(v$openalex_id, "W1")
  expect_false(anyNA(v$openalex_id))
})
