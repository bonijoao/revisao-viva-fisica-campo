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
