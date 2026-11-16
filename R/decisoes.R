# Decisoes humanas de triagem (decisoes.csv e a fonte unica da verdade).
#
# Formato obrigatorio de `data` na escrita: data ISO "YYYY-MM-DD" ou datetime
# UTC "YYYY-MM-DDTHH:MM:SSZ". Nada de offsets, NA ou vazio. Nesse formato a
# ordem lexicografica coincide com a cronologica (data simples ordena antes de
# qualquer datetime do mesmo dia).
FORMATO_DATA_DECISAO <- "^\\d{4}-\\d{2}-\\d{2}(T\\d{2}:\\d{2}:\\d{2}Z)?$"

# Substitui, em `decisoes`, as linhas com a mesma chave
# (openalex_id, etapa, rodada, via) de `novas` e acrescenta `novas`.
registrar_decisoes <- function(decisoes, novas) {
  ruim <- is.na(novas$data) | !grepl(FORMATO_DATA_DECISAO, novas$data)
  if (any(ruim)) {
    stop("`data` invalida (use YYYY-MM-DD ou YYYY-MM-DDTHH:MM:SSZ) para: ",
         paste(unique(novas$openalex_id[ruim]), collapse = ", "), call. = FALSE)
  }
  chave <- function(d) paste(d$openalex_id, d$etapa, d$rodada, d$via, sep = "\r")
  base <- decisoes[!(chave(decisoes) %in% chave(novas)), , drop = FALSE]
  out <- rbind(base[names(decisoes_vazio())], novas[names(decisoes_vazio())])
  tibble::as_tibble(out)
}

# Decisao vigente por registro: maior `data`; empate, a ultima na tabela.
decisao_vigente <- function(decisoes, etapa = "titulo_resumo") {
  cols <- c("openalex_id", "decisao", "via", "rodada", "data")
  d <- decisoes[which(decisoes$etapa == etapa), , drop = FALSE]
  if (nrow(d) == 0) return(decisoes_vazio()[cols])
  if (anyNA(d$data)) {
    stop("decisoes com `data` NA para: ",
         paste(unique(d$openalex_id[is.na(d$data)]), collapse = ", "), call. = FALSE)
  }
  d$.pos <- seq_len(nrow(d))
  d <- d[order(d$openalex_id, d$data, d$.pos), , drop = FALSE]
  d <- d[!duplicated(d$openalex_id, fromLast = TRUE), , drop = FALSE]
  tibble::as_tibble(d[cols])
}
