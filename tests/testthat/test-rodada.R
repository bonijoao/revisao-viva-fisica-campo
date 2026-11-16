raiz_repo <- normalizePath(file.path("..", ".."))
busca_yml <- yaml::read_yaml(file.path(raiz_repo, "protocolo", "busca.yml"))
regras_yml <- yaml::read_yaml(file.path(raiz_repo, "protocolo", "regras.yml"))
fixture_rodada <- readRDS(file.path("..", "fixtures", "openalex_amostra.rds"))
fetcher_falso <- function(...) fixture_rodada
fetcher_falha <- function(...) stop("OpenAlex fora do ar")

estado_vazio <- function() {
  list(registros = registros_vazio(), decisoes = decisoes_vazio(), rodadas = rodadas_vazio())
}

rodar <- function(estado = estado_vazio(), modo = "mensal", data = as.Date("2026-12-01"),
                  regras = regras_yml, fetcher = fetcher_falso, limite = 50) {
  executar_rodada(estado, busca_yml, regras, modo = modo, data = data,
                  fetcher = fetcher, limite = limite)
}

test_that("test_rodada_primeira_vez", {
  out <- rodar()
  r <- out$estado$rodadas
  expect_equal(nrow(r), 1)
  expect_equal(r$rodada, "2026-12")
  expect_equal(r$modo, "mensal")
  expect_equal(r$n_identificados, nrow(fixture_rodada))
  expect_equal(r$n_identificados, r$n_duplicatas + r$n_excluidos_regra + r$n_para_triagem)
  expect_equal(r$n_para_triagem, nrow(out$candidatos))
  expect_equal(c(r$n_triados, r$n_nao_triados_parada, r$n_incluidos), c(0L, 0L, 0L))
  expect_true(is.na(r$issue))
  expect_equal(r$hash_busca, digest::digest(busca_yml, "sha1"))
  expect_equal(r$hash_regras, digest::digest(regras_yml, "sha1"))
  expect_equal(names(r), names(rodadas_vazio()))
  expect_equal(names(out$estado$registros), names(registros_vazio()))
  expect_true(all(out$estado$registros$rodada_identificacao == "2026-12"))
  expect_true(all(out$candidatos$status == "para_triagem"))
  expect_equal(out$destino, if (nrow(out$candidatos) == 0) "nenhum" else "issue")
})

test_that("test_rodada_segunda_vez_sem_novos", {
  um <- rodar()
  dois <- rodar(um$estado, data = as.Date("2027-01-01"))
  r <- dois$estado$rodadas
  expect_equal(nrow(r), 2)
  expect_equal(r$rodada[2], "2027-01")
  expect_equal(r$n_identificados[2], 0L)
  expect_equal(nrow(dois$candidatos), 0)
  expect_equal(dois$destino, "nenhum")
  expect_equal(nrow(dois$estado$registros), nrow(um$estado$registros))
})

test_that("test_rodada_linha_de_base_vai_para_asreview", {
  out <- rodar(modo = "linha_de_base")
  expect_equal(out$estado$rodadas$rodada, "linha-de-base")
  expect_gt(nrow(out$candidatos), 0)
  expect_equal(out$destino, "asreview")
})

test_that("test_rodada_acima_limite", {
  out <- rodar(limite = 1)
  expect_gte(nrow(out$candidatos), 2)
  expect_equal(out$destino, "asreview")
})

copiar_repo <- function() {
  tmp <- tempfile("repo-")
  dir.create(tmp)
  for (d in c("R", "scripts", "protocolo", "dados")) {
    file.copy(file.path(raiz_repo, d), tmp, recursive = TRUE)
  }
  tmp
}

test_that("test_rodada_erro_fetcher_nao_altera_estado", {
  est <- estado_vazio()
  expect_error(rodar(est, fetcher = fetcher_falha), "fora do ar")

  tmp <- copiar_repo()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  # Estado nao vazio, para a comparacao ser significativa.
  salvar_estado(rodar()$estado, file.path(tmp, "dados"))
  csvs <- file.path(tmp, "dados", c("registros.csv", "decisoes.csv", "rodadas.csv"))
  antes <- lapply(csvs, function(f) readBin(f, "raw", file.info(f)$size))
  res <- callr::rscript(
    file.path("scripts", "buscar.R"), cmdargs = "mensal", wd = tmp,
    env = c(callr::rcmd_safe_env(), REVISAO_FETCHER_FALHA = "1", GITHUB_OUTPUT = ""),
    fail_on_status = FALSE, show = FALSE
  )
  expect_true(res$status != 0)
  depois <- lapply(csvs, function(f) readBin(f, "raw", file.info(f)$size))
  expect_identical(depois, antes)
  expect_false(dir.exists(file.path(tmp, "triagem")))
})

test_that("test_rodada_repetida_erro", {
  um <- rodar()
  expect_error(rodar(um$estado), "já existe")
})

test_that("test_rodada_decidido_nao_reavaliado", {
  um <- rodar()
  alvo <- um$candidatos$openalex_id[1]
  est <- um$estado
  est$decisoes <- registrar_decisoes(est$decisoes, tibble::tibble(
    openalex_id = alvo, etapa = "titulo_resumo", decisao = "incluir",
    motivo = NA_character_, via = "issue", rodada = "2026-12",
    data = "2026-12-05T10:00:00Z", revisor = "teste"
  ))
  regras_novas <- list(versao = 2, regras = list(
    list(id = "R9", descricao = "exclui tudo", tipo = "ano_menor_que", valor = 3000)
  ))
  dois <- rodar(est, data = as.Date("2027-01-01"), regras = regras_novas)
  reg <- dois$estado$registros
  expect_equal(reg$status[reg$openalex_id == alvo], "para_triagem")
  vig <- decisao_vigente(dois$estado$decisoes)
  expect_equal(vig$decisao[vig$openalex_id == alvo], "incluir")
  # Os pendentes sem decisao humana sao reavaliados pela regra nova.
  outros <- setdiff(um$candidatos$openalex_id, alvo)
  expect_true(all(reg$status[reg$openalex_id %in% outros] == "excluido_regra"))
  # Mas a contagem da rodada nova so conta linhas da rodada nova.
  expect_equal(dois$estado$rodadas$n_excluidos_regra[2], 0L)
})
