# Exportacao/importacao do ASReview LAB via CSV.

COLUNAS_ROTULO_ASREVIEW <- c("included", "label_included", "asreview_label")

exportar_asreview <- function(candidatos, caminho) {
  col <- function(nome) {
    x <- if (nome %in% names(candidatos)) as.character(candidatos[[nome]]) else rep(NA_character_, nrow(candidatos))
    x[is.na(x)] <- ""
    x
  }
  saida <- data.frame(
    openalex_id = col("openalex_id"), title = col("titulo"),
    abstract = col("resumo"), doi = col("doi"), url = col("url_oa"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(saida, caminho, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(caminho)
}

importar_asreview <- function(exportado, ids_enviados, rodada, data, revisor) {
  achada <- intersect(COLUNAS_ROTULO_ASREVIEW, names(exportado))
  if (length(achada) == 0) {
    stop("Nenhuma coluna de rótulo do ASReview encontrada (esperado: ",
         paste(COLUNAS_ROTULO_ASREVIEW, collapse = ", "), "). Colunas do arquivo: ",
         paste(names(exportado), collapse = ", "), call. = FALSE)
  }
  if (!"openalex_id" %in% names(exportado)) {
    stop("O arquivo do ASReview não tem a coluna openalex_id. Colunas do arquivo: ",
         paste(names(exportado), collapse = ", "), call. = FALSE)
  }
  ids_enviados <- unique(as.character(ids_enviados))
  rotulo <- trimws(as.character(exportado[[achada[1]]]))
  rotulo[rotulo %in% c("", "NA")] <- NA_character_
  num <- suppressWarnings(as.numeric(rotulo))
  dec_arq <- ifelse(is.na(num), "nao_triado_parada",
                    ifelse(num == 1, "incluir", ifelse(num == 0, "excluir", "nao_triado_parada")))
  ids_arq <- as.character(exportado$openalex_id)
  ausentes <- setdiff(ids_enviados, ids_arq)
  if (length(ausentes) > 0) {
    stop(length(ausentes), " registro(s) enviado(s) ao ASReview não estão no arquivo importado (ex.: ",
         paste(utils::head(ausentes, 3), collapse = ", "),
         "). Exporte TODOS os registros do projeto como CSV, não só os relevantes.", call. = FALSE)
  }
  dec <- dec_arq[match(ids_enviados, ids_arq)]
  dec[is.na(dec)] <- "nao_triado_parada"  # id presente, mas sem rotulo
  tibble::tibble(
    openalex_id = as.character(ids_enviados), etapa = "titulo_resumo",
    decisao = dec, motivo = NA_character_, via = "asreview",
    rodada = rodada, data = data, revisor = revisor
  )
}
