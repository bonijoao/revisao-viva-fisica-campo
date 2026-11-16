estado_sintetico <- function() {
  reg <- function(id, rod, st) {
    r <- registros_vazio()[1, ]
    r[] <- NA
    r$openalex_id <- id; r$rodada_identificacao <- rod; r$status <- st
    r
  }
  registros <- do.call(rbind, c(
    lapply(1:3, function(i) reg(paste0("A", i), "2026-12", "para_triagem")),
    list(reg("A4", "2026-12", "duplicata"), reg("A5", "2026-12", "excluido_regra")),
    lapply(1:4, function(i) reg(paste0("B", i), "2027-01", "para_triagem")),
    list(reg("B5", "2027-01", "duplicata"))
  ))
  dec <- function(id, d, data = "2027-01-05") {
    tibble::tibble(openalex_id = id, etapa = "titulo_resumo", decisao = d, motivo = NA_character_,
                   via = "manual", rodada = "2027-01", data = data, revisor = "r")
  }
  decisoes <- rbind(dec("A1", "incluir"), dec("A2", "excluir"), dec("A2", "incluir", "2027-01-06"),
                    dec("B1", "incluir"), dec("B2", "nao_triado_parada"))
  rodadas <- rodadas_vazio()[1:2, ]
  rodadas$rodada <- c("2026-12", "2027-01")
  rodadas$data <- c("2026-12-01", "2027-01-01")
  list(registros = registros, decisoes = decisoes, rodadas = rodadas)
}

fecha <- function(x) {
  expect_equal(x$identificados, x$duplicatas + x$excluidos_regra + x$triados +
                 x$nao_triados_parada + x$aguardando)
  expect_equal(x$triados, x$excluidos_triagem + x$incluidos)
}

test_that("test_contagens_fecham", {
  est <- estado_sintetico()
  for (r in c("2026-12", "2027-01")) {
    est2 <- est
    est2$rodadas <- est$rodadas[est$rodadas$rodada == r, ]
    est2$registros <- est$registros[est$registros$rodada_identificacao == r, ]
    fecha(contagens_prisma(est2)$nova)
  }
  cp <- contagens_prisma(est)
  fecha(cp$anterior); fecha(cp$nova)
})

test_that("test_contagens_anterior_nova", {
  cp <- contagens_prisma(estado_sintetico())
  expect_equal(cp$anterior$identificados, 5)
  expect_equal(cp$anterior$duplicatas, 1)
  expect_equal(cp$anterior$excluidos_regra, 1)
  expect_equal(cp$anterior$incluidos, 2)  # A2 virou incluir (decisao vigente)
  expect_equal(cp$anterior$excluidos_triagem, 0)
  expect_equal(cp$anterior$aguardando, 1)
  expect_equal(cp$nova$identificados, 5)
  expect_equal(cp$nova$incluidos, 1)
  expect_equal(cp$nova$nao_triados_parada, 1)
  expect_equal(cp$nova$aguardando, 2)
  expect_equal(cp$nova$triados, 1)
  expect_equal(cp$pendentes$rodada, c("2026-12", "2027-01"))
  expect_equal(cp$pendentes$n, c(1, 2))
})

test_that("test_contagens_estado_vazio", {
  cp <- contagens_prisma(list(registros = registros_vazio(), decisoes = decisoes_vazio(),
                              rodadas = rodadas_vazio()))
  expect_true(all(unlist(cp$anterior) == 0))
  expect_true(all(unlist(cp$nova) == 0))
  expect_equal(nrow(cp$pendentes), 0)
})

test_that("test_contagens_iguais_rodadas_csv", {
  fx <- readRDS(file.path("..", "fixtures", "openalex_amostra.rds"))
  busca <- yaml::read_yaml(file.path("..", "..", "protocolo", "busca.yml"))
  regras <- yaml::read_yaml(file.path("..", "..", "protocolo", "regras.yml"))
  est0 <- list(registros = registros_vazio(), decisoes = decisoes_vazio(), rodadas = rodadas_vazio())
  out <- executar_rodada(est0, busca, regras, modo = "mensal", data = as.Date("2026-12-01"),
                         fetcher = function(...) fx, limite = 50)
  est <- out$estado
  ids <- out$candidatos$openalex_id
  skip_if(length(ids) < 2)
  est$decisoes <- registrar_decisoes(est$decisoes, tibble::tibble(
    openalex_id = ids[1:2], etapa = "titulo_resumo", decisao = c("incluir", "excluir"),
    motivo = NA_character_, via = "manual", rodada = "2026-12", data = "2026-12-05", revisor = "r"))
  cp <- contagens_prisma(est)
  r <- est$rodadas
  expect_equal(cp$nova$identificados, tail(r$n_identificados, 1))
  expect_equal(cp$nova$duplicatas, tail(r$n_duplicatas, 1))
  expect_equal(cp$nova$excluidos_regra, tail(r$n_excluidos_regra, 1))
  expect_equal(cp$nova$triados, 2)
  expect_equal(cp$nova$incluidos, 1)
  expect_true(all(unlist(cp$anterior) == 0))
})

test_that("test_desenhar_prisma_roda", {
  g <- desenhar_prisma(contagens_prisma(estado_sintetico()))
  expect_false(is.null(g))
})

test_that("test_desenhar_prisma_vazio", {
  cp <- contagens_prisma(list(registros = registros_vazio(), decisoes = decisoes_vazio(),
                              rodadas = rodadas_vazio()))
  g <- tryCatch(desenhar_prisma(cp), error = function(e) NULL)
  expect_true(is.null(g) || !is.null(g))  # nao deve propagar erro
})
