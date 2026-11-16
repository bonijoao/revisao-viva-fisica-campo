# Orquestracao de uma rodada: busca -> normaliza -> deduplica -> regras -> rodadas.

nome_rodada <- function(modo, data) {
  if (identical(modo, "linha_de_base")) "linha-de-base" else format(as.Date(data), "%Y-%m")
}

# `regras` pode ser o YAML inteiro (com $regras) ou so a lista de regras;
# o hash e sempre do objeto recebido.
.lista_regras <- function(regras) if (!is.null(regras$regras)) regras$regras else regras

executar_rodada <- function(estado, busca, regras, modo, data,
                            fetcher = openalexR::oa_fetch, limite = 50) {
  data <- as.Date(data)
  rodada <- nome_rodada(modo, data)
  if (rodada %in% estado$rodadas$rodada) {
    stop("A rodada ", rodada, " já existe em rodadas.csv.", call. = FALSE)
  }

  bruto <- buscar_openalex(busca, ate = data, fetcher = fetcher)
  novos <- if (is.null(bruto) || NROW(bruto) == 0) registros_vazio() else normalizar_obras(bruto, rodada)
  dd <- deduplicar(novos, estado$registros)

  registros <- estado$registros
  if (nrow(dd$atualizados) > 0) {
    pos <- match(dd$atualizados$openalex_id, registros$openalex_id)
    registros[pos, ] <- dd$atualizados[names(registros)]
  }
  registros <- tibble::as_tibble(rbind(registros, dd$novos[names(registros_vazio())]))

  decididos <- decisao_vigente(estado$decisoes)$openalex_id
  registros <- aplicar_regras(registros, .lista_regras(regras), decididos = decididos)

  # Linhas acrescentadas nesta rodada (deduplicar so devolve IDs nunca vistos).
  desta <- registros$openalex_id %in% dd$novos$openalex_id &
    registros$rodada_identificacao %in% rodada
  st <- registros$status[desta]
  candidatos <- registros[desta & registros$status %in% "para_triagem", ]

  linha <- rodadas_vazio()[NA_integer_, ]
  linha$rodada <- rodada
  linha$data <- as.character(data)
  linha$modo <- modo
  linha$hash_busca <- digest::digest(busca, "sha1")
  linha$hash_regras <- digest::digest(regras, "sha1")
  linha$n_identificados <- as.integer(sum(desta))
  linha$n_duplicatas <- as.integer(sum(st == "duplicata"))
  linha$n_excluidos_regra <- as.integer(sum(st == "excluido_regra"))
  linha$n_para_triagem <- as.integer(sum(st == "para_triagem"))
  linha$n_triados <- 0L
  linha$n_nao_triados_parada <- 0L
  linha$n_incluidos <- 0L
  linha$issue <- NA_integer_

  destino <- if (nrow(candidatos) == 0) {
    "nenhum"
  } else if (identical(modo, "linha_de_base") || nrow(candidatos) > limite) {
    "asreview"
  } else {
    "issue"
  }

  estado$registros <- registros
  estado$rodadas <- tibble::as_tibble(rbind(estado$rodadas, linha))
  list(estado = estado, candidatos = candidatos, destino = destino)
}

# Atualiza campos de uma rodada existente em `rodadas`.
atualizar_rodada <- function(rodadas, rodada, ...) {
  i <- which(rodadas$rodada == rodada)
  if (length(i) != 1) stop("Rodada ", rodada, " não encontrada em rodadas.csv.", call. = FALSE)
  campos <- list(...)
  for (nome in names(campos)) rodadas[[nome]][i] <- as.integer(campos[[nome]])
  rodadas
}

# Escreve `key=value` em $GITHUB_OUTPUT, ou na saida padrao se nao houver.
escrever_saida_gha <- function(...) {
  pares <- list(...)
  linhas <- paste0(names(pares), "=", vapply(pares, function(v) {
    v <- as.character(v)
    if (length(v) == 0 || is.na(v)) "" else v
  }, character(1)))
  destino <- Sys.getenv("GITHUB_OUTPUT")
  if (nzchar(destino)) {
    cat(linhas, file = destino, sep = "\n", append = TRUE)
  } else {
    cat(linhas, sep = "\n")
  }
  invisible(linhas)
}

agora_utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

escrever_utf8 <- function(texto, caminho) {
  dir.create(dirname(caminho), showWarnings = FALSE, recursive = TRUE)
  con <- file(caminho, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(paste(texto, collapse = "\n"))), con)
  invisible(caminho)
}
