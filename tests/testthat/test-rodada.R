raiz_repo <- normalizePath(file.path("..", ".."))
busca_yml <- yaml::read_yaml(file.path(raiz_repo, "protocolo", "busca.yml"))
regras_yml <- yaml::read_yaml(file.path(raiz_repo, "protocolo", "regras.yml"))
fixture_rodada <- readRDS(file.path("..", "fixtures", "openalex_amostra.rds"))
fetcher_falso <- function(...) fixture_rodada
fetcher_falha <- function(...) stop("OpenAlex fora do ar")

estado_vazio <- function() {
  list(registros = registros_vazio(), decisoes = decisoes_vazio(), rodadas = rodadas_vazio())
}

rodar <- function(estado = estado_com_base(), modo = "mensal", data = as.Date("2026-12-01"),
                  regras = regras_yml, fetcher = fetcher_falso, limite = 50) {
  executar_rodada(estado, busca_yml, regras, modo = modo, data = data,
                  fetcher = fetcher, limite = limite)
}

test_that("test_rodada_primeira_vez", {
  out <- rodar()
  r <- out$estado$rodadas
  expect_equal(nrow(r), 2)
  r <- r[2, ]
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
  expect_equal(nrow(r), 3)
  expect_equal(r$rodada[3], "2027-01")
  expect_equal(r$n_identificados[3], 0L)
  expect_equal(nrow(dois$candidatos), 0)
  expect_equal(dois$destino, "nenhum")
  expect_equal(nrow(dois$estado$registros), nrow(um$estado$registros))
})

test_that("test_rodada_linha_de_base_vai_para_asreview", {
  out <- rodar(estado_vazio(), modo = "linha_de_base")
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
  est <- estado_com_base()
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
  # Pendentes antigos sem decisao humana ja estao em triagem: nao sao reavaliados.
  outros <- setdiff(um$candidatos$openalex_id, alvo)
  expect_true(all(reg$status[reg$openalex_id %in% outros] == "para_triagem"))
  expect_true(all(is.na(reg$regra_id[reg$openalex_id %in% outros])))
  expect_equal(dois$estado$rodadas$n_excluidos_regra[3], 0L)
})

test_that("test_rodada_regras_so_nas_linhas_novas", {
  um <- rodar(fetcher = function(...) fixture_rodada[1:3, ])
  regras_novas <- list(versao = 2, regras = list(
    list(id = "R9", descricao = "exclui tudo", tipo = "ano_menor_que", valor = 3000)
  ))
  dois <- rodar(um$estado, data = as.Date("2027-01-01"), regras = regras_novas,
                fetcher = function(...) fixture_rodada[4:5, ])
  reg <- dois$estado$registros
  expect_true(all(reg$status[reg$rodada_identificacao == "2026-12"] == "para_triagem"))
  expect_true(all(reg$status[reg$rodada_identificacao == "2027-01"] == "excluido_regra"))
  expect_equal(dois$estado$rodadas$n_excluidos_regra[3], 2L)
})

test_that("test_rodada_contagens_mistas", {
  um <- rodar(fetcher = function(...) fixture_rodada[1:3, ])
  f2 <- fixture_rodada[c(1, 1, 4, 4, 5), ]
  f2$id[2] <- "https://openalex.org/WDUPEX"    # DOI de um registro existente
  f2$id[4] <- "https://openalex.org/WDUPLOTE"  # DOI de outra obra do mesmo lote
  f2$id[5] <- "https://openalex.org/WOFF"      # fora do tema (R2)
  f2$title[5] <- "Cooking pasta at home"
  f2$display_name[5] <- "Cooking pasta at home"
  if ("abstract" %in% names(f2)) f2$abstract[5] <- "Recipes."
  if ("keywords" %in% names(f2)) f2$keywords[5] <- list(NULL)
  f2$doi[5] <- "https://doi.org/10.9999/off"
  dois <- rodar(um$estado, data = as.Date("2027-01-01"), fetcher = function(...) f2)
  r <- dois$estado$rodadas[3, ]
  expect_equal(r$n_identificados, 4L)
  expect_equal(r$n_duplicatas, 2L)
  expect_equal(r$n_excluidos_regra, 1L)
  expect_equal(r$n_para_triagem, 1L)
  reg <- dois$estado$registros
  expect_equal(reg$regra_id[reg$openalex_id == "WOFF"], "R2")
  tab <- table(factor(reg$status[reg$rodada_identificacao == "2027-01"],
                      levels = c("duplicata", "excluido_regra", "para_triagem")))
  expect_equal(as.integer(tab), c(r$n_duplicatas, r$n_excluidos_regra, r$n_para_triagem))
  expect_equal(sum(tab), r$n_identificados)
  expect_equal(dois$candidatos$openalex_id, reg$openalex_id[reg$rodada_identificacao == "2027-01" &
                                                             reg$status == "para_triagem"])
})

test_that("test_redigir_segredos", {
  withr::local_envvar(OPENALEX_API_KEY = "chave-secreta-123", OPENALEX_EMAIL = "fulano@exemplo.org")
  msg <- paste0("HTTP 500 em https://api.openalex.org/works?filter=x&api_key=abc123&mailto=a%40b.org&page=2",
                " (chave chave-secreta-123, email fulano@exemplo.org)")
  out <- redigir_segredos(msg)
  expect_false(grepl("abc123", out, fixed = TRUE))
  expect_false(grepl("a%40b.org", out, fixed = TRUE))
  expect_false(grepl("chave-secreta-123", out, fixed = TRUE))
  expect_false(grepl("fulano@exemplo.org", out, fixed = TRUE))
  expect_true(grepl("api_key=***&mailto=***&page=2", out, fixed = TRUE))
  withr::local_envvar(OPENALEX_API_KEY = "", OPENALEX_EMAIL = "")
  expect_equal(redigir_segredos("sem segredos"), "sem segredos")
})

test_that("test_rodada_mensal_sem_linha_de_base_erro", {
  expect_error(rodar(estado_vazio(), modo = "mensal"), "linha de base ainda não foi executada")
})

test_that("test_rodada_linha_de_base_com_estado_nao_vazio_erro", {
  expect_error(rodar(estado_com_base(), modo = "linha_de_base"), "linha de base")
  um <- rodar()
  expect_error(rodar(um$estado, modo = "linha_de_base", data = as.Date("2027-02-01")), "linha de base")
})

test_that("test_script_mensal_sem_linha_de_base_sai_0_sem_escrever", {
  tmp <- copiar_repo()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  unlink(list.files(file.path(tmp, "dados"), full.names = TRUE))
  salvar_estado(estado_vazio(), file.path(tmp, "dados"))
  saida <- tempfile()
  csvs <- file.path(tmp, "dados", c("registros.csv", "decisoes.csv", "rodadas.csv"))
  antes <- lapply(csvs, function(f) readBin(f, "raw", file.info(f)$size))
  res <- callr::rscript(
    file.path("scripts", "buscar.R"), cmdargs = "mensal", wd = tmp,
    env = c(callr::rcmd_safe_env(), REVISAO_FETCHER_FALHA = "1", GITHUB_OUTPUT = saida),
    fail_on_status = FALSE, show = FALSE
  )
  expect_equal(res$status, 0L)
  expect_match(res$stderr, "linha de base")
  expect_true("destino=nenhum" %in% readLines(saida))
  depois <- lapply(csvs, function(f) readBin(f, "raw", file.info(f)$size))
  expect_identical(depois, antes)
  expect_false(dir.exists(file.path(tmp, "triagem")))
})

test_that("test_script_linha_de_base_com_estado_nao_vazio_sai_1", {
  tmp <- copiar_repo()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  unlink(list.files(file.path(tmp, "dados"), full.names = TRUE))
  salvar_estado(estado_com_base(), file.path(tmp, "dados"))
  res <- callr::rscript(
    file.path("scripts", "buscar.R"), cmdargs = "linha_de_base", wd = tmp,
    env = c(callr::rcmd_safe_env(), REVISAO_FETCHER_FALHA = "1", GITHUB_OUTPUT = ""),
    fail_on_status = FALSE, show = FALSE
  )
  expect_true(res$status != 0)
})

test_that("test_preparar_material_issue_grande_cai_para_asreview", {
  cand <- registros_vazio()[rep(NA_integer_, 40), ]
  cand$openalex_id <- paste0("W", 1:40)
  cand$titulo <- strrep("x", 3000)
  expect_error(montar_corpo_issue(cand, "2027-01"), "caracteres")
  dir <- withr::local_tempdir()
  m <- preparar_material("issue", cand, "2027-01", dir_triagem = dir)
  expect_equal(m$destino, "asreview")
  expect_true(file.exists(m$arquivo))
  expect_false(file.exists(file.path(dir, "issue-2027-01.md")))
  # Issue normal continua sendo issue.
  m2 <- preparar_material("issue", cand[1:2, ], "2027-01", dir_triagem = dir)
  expect_equal(m2$destino, "issue")
  expect_true(file.exists(m2$arquivo))
})
