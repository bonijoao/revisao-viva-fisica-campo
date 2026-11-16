# Issue de triagem: renderiza candidatos como checklist e le as marcacoes.

LIMITE_CORPO_ISSUE <- 65000L
LIMITE_RESUMO_ISSUE <- 1500L

.tem_valor <- function(x) !is.na(x) & nzchar(trimws(x))

.limpar_linha <- function(x) {
  x <- gsub("[\r\n\t]+", " ", x)
  trimws(gsub(" {2,}", " ", x))
}

.limpar_titulo <- function(x) {
  x <- .limpar_linha(x)
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("*", "\\*", x, fixed = TRUE)
  x <- gsub("[", "\\[", x, fixed = TRUE)
  x <- gsub("]", "\\]", x, fixed = TRUE)
  gsub("<!--", "<!-", x, fixed = TRUE)
}

.item_issue <- function(cand, i, limite_resumo) {
  v <- function(col) if (col %in% names(cand)) cand[[col]][i] else NA_character_
  id <- v("openalex_id")
  titulo <- if (.tem_valor(v("titulo"))) .limpar_titulo(v("titulo")) else "(sem título)"
  ano <- if ("ano" %in% names(cand) && !is.na(cand$ano[i])) paste0(" (", cand$ano[i], ")") else ""
  fonte <- if (.tem_valor(v("fonte"))) paste0(" — ", gsub("<!--", "<!-", .limpar_linha(v("fonte")), fixed = TRUE)) else ""
  links <- paste0("[OpenAlex](https://openalex.org/", id, ")")
  if (.tem_valor(v("doi"))) links <- c(links, paste0("[DOI](https://doi.org/", gsub("<!--", "<!-", trimws(v("doi")), fixed = TRUE), ")"))
  if (.tem_valor(v("url_oa"))) links <- c(links, paste0("[Texto](", gsub("\\s", "", v("url_oa")), ")"))
  res <- v("resumo")
  if (.tem_valor(res)) {
    res <- .limpar_linha(res)
    res <- gsub("<!--", "<!-", res, fixed = TRUE)
    if (nchar(res) > limite_resumo) res <- paste0(substr(res, 1, limite_resumo), "…")
  } else {
    res <- "*(sem resumo no OpenAlex)*"
  }
  paste0(
    "- [ ] **", titulo, "**", ano, fonte, "\n",
    "  ", paste(links, collapse = " · "), "\n",
    "  <details><summary>Resumo</summary>", res, "</details>\n",
    "  <!-- id:", id, " -->\n"
  )
}

montar_corpo_issue <- function(candidatos, rodada) {
  n <- nrow(candidatos)
  cabecalho <- paste0(
    "## Triagem ", rodada, " — ", n, " registros\n\n",
    "Marque os que devem ser INCLUÍDOS. Ao terminar, feche a issue. ",
    "Desmarcados = excluídos.\n\n"
  )
  montar <- function(lim) {
    itens <- vapply(seq_len(n), function(i) .item_issue(candidatos, i, lim), character(1))
    paste0(cabecalho, paste(itens, collapse = "\n"))
  }
  corpo <- montar(LIMITE_RESUMO_ISSUE)
  # Se estourar o limite do GitHub, encurta os resumos progressivamente.
  for (lim in c(1000L, 700L, 500L, 300L, 150L, 0L)) {
    if (nchar(corpo) <= LIMITE_CORPO_ISSUE) break
    corpo <- montar(lim)
  }
  if (nchar(corpo) > LIMITE_CORPO_ISSUE) {
    stop("O corpo da issue tem ", nchar(corpo), " caracteres (limite ",
         LIMITE_CORPO_ISSUE, "); use a via ASReview para este lote.", call. = FALSE)
  }
  corpo
}

ler_decisoes_issue <- function(corpo, rodada, data, revisor) {
  linhas <- strsplit(paste(corpo, collapse = "\n"), "\n", fixed = TRUE)[[1]]
  linhas <- sub("\r$", "", linhas)
  inicio <- grep("^\\s*- \\[[ xX]\\]", linhas)
  if (length(inicio) == 0) return(decisoes_vazio())
  fim <- c(inicio[-1] - 1L, length(linhas))
  ids <- character(); marc <- character()
  for (k in seq_along(inicio)) {
    bloco <- paste(linhas[inicio[k]:fim[k]], collapse = "\n")
    padrao <- "<!--\\s*id:\\s*([^\\s>]+?)\\s*-->"
    m <- regmatches(bloco, gregexpr(padrao, bloco, perl = TRUE))[[1]]
    if (length(m) < 1) next
    # Vale o ultimo marcador do bloco (o real, gerado depois do titulo/fonte).
    ids <- c(ids, sub(padrao, "\\1", m[length(m)], perl = TRUE))
    marc <- c(marc, sub("^\\s*- \\[(.)\\].*$", "\\1",linhas[inicio[k]]))
  }
  if (length(ids) == 0) return(decisoes_vazio())
  # Ids repetidos: qualquer ocorrencia marcada vale incluir.
  marcado <- marc %in% c("x", "X")
  ids_u <- unique(ids)
  incluir <- vapply(ids_u, function(i) any(marcado[ids == i]), logical(1), USE.NAMES = FALSE)
  ids <- ids_u
  tibble::tibble(
    openalex_id = ids, etapa = "titulo_resumo",
    decisao = ifelse(incluir, "incluir", "excluir"),
    motivo = NA_character_, via = "issue", rodada = rodada,
    data = data, revisor = revisor
  )
}
