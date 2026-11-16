# Renderiza o site Quarto em copia temporaria, com estado vazio e com estado cheio.

fixture_site <- readRDS(file.path("..", "fixtures", "openalex_amostra.rds"))

copiar_projeto <- function(destino) {
  raiz <- normalizePath(file.path("..", ".."))
  for (d in c("site", "R", "protocolo", "dados")) {
    file.copy(file.path(raiz, d), destino, recursive = TRUE)
  }
  unlink(file.path(destino, "site", c("_site", ".quarto")), recursive = TRUE)
  invisible(destino)
}

renderizar_site <- function(destino) {
  out <- system2("quarto", c("render", shQuote(file.path(destino, "site"))),
                 stdout = TRUE, stderr = TRUE)
  list(status = attr(out, "status") %||% 0L, saida = out)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

estado_vazio_site <- function() {
  list(registros = registros_vazio(), decisoes = decisoes_vazio(), rodadas = rodadas_vazio())
}

test_that("test_site_renderiza com estado vazio", {
  skip_if(Sys.which("quarto") == "")
  tmp <- withr::local_tempdir()
  copiar_projeto(tmp)
  unlink(list.files(file.path(tmp, "dados"), full.names = TRUE))
  salvar_estado(list(registros = registros_vazio(), decisoes = decisoes_vazio(),
                     rodadas = rodadas_vazio()), file.path(tmp, "dados"))
  r <- renderizar_site(tmp)
  expect_equal(r$status, 0L, info = paste(r$saida, collapse = "\n"))
  expect_true(file.exists(file.path(tmp, "site", "_site", "index.html")))
  expect_true(file.exists(file.path(tmp, "site", "_site", "prisma.html")))
  corpus <- paste(readLines(file.path(tmp, "site", "_site", "corpus.html"), encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_match(corpus, "Ainda não há registros incluídos.", fixed = TRUE)
})

test_that("test_site_renderiza com estado cheio", {
  skip_if(Sys.which("quarto") == "")
  busca <- yaml::read_yaml(file.path("..", "..", "protocolo", "busca.yml"))
  regras <- yaml::read_yaml(file.path("..", "..", "protocolo", "regras.yml"))
  out <- executar_rodada(estado_vazio_site(), busca, regras, modo = "mensal",
                         data = as.Date("2026-12-01"),
                         fetcher = function(...) fixture_site, limite = 50)
  est <- out$estado
  cand <- est$registros[est$registros$status == "para_triagem", ]
  expect_gte(nrow(cand), 2)
  ids <- cand$openalex_id
  novas <- tibble::tibble(
    openalex_id = ids, etapa = "titulo_resumo",
    decisao = c(rep("incluir", min(3, length(ids))), rep("excluir", max(0, length(ids) - 3))),
    motivo = NA_character_, via = "humano", rodada = "2026-12",
    data = "2026-12-02", revisor = "teste")
  est$decisoes <- registrar_decisoes(est$decisoes, novas)
  est$registros$resumo_atualizado_em[match(ids[1], est$registros$openalex_id)] <- "2027-01-05"
  tmp <- withr::local_tempdir()
  copiar_projeto(tmp)
  salvar_estado(est, file.path(tmp, "dados"))
  r <- renderizar_site(tmp)
  expect_equal(r$status, 0L, info = paste(r$saida, collapse = "\n"))
  for (p in c("index", "prisma", "corpus", "caracterizacao", "protocolo", "metodo")) {
    expect_true(file.exists(file.path(tmp, "site", "_site", paste0(p, ".html"))), info = p)
  }
  corpus <- paste(readLines(file.path(tmp, "site", "_site", "corpus.html"), encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_false(grepl("Ainda não há registros incluídos.", corpus, fixed = TRUE))
  # copia visual para inspecao manual
  amostra <- Sys.getenv("SITE_AMOSTRA_DIR")
  if (nzchar(amostra)) {
    unlink(amostra, recursive = TRUE)
    file.copy(file.path(tmp, "site", "_site"), dirname(amostra), recursive = TRUE)
    file.rename(file.path(dirname(amostra), "_site"), amostra)
  }
})

