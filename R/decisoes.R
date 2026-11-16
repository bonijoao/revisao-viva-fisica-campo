# Decisoes humanas de triagem (decisoes.csv e a fonte unica da verdade).

# Substitui, em `decisoes`, as linhas com a mesma chave
# (openalex_id, etapa, rodada, via) de `novas` e acrescenta `novas`.
registrar_decisoes <- function(decisoes, novas) {
  chave <- function(d) paste(d$openalex_id, d$etapa, d$rodada, d$via, sep = "\r")
  base <- decisoes[!(chave(decisoes) %in% chave(novas)), , drop = FALSE]
  out <- rbind(base[names(decisoes_vazio())], novas[names(decisoes_vazio())])
  tibble::as_tibble(out)
}

# Decisao vigente por registro: maior `data`; empate, a ultima na tabela.
decisao_vigente <- function(decisoes, etapa = "titulo_resumo") {
  cols <- c("openalex_id", "decisao", "via", "rodada", "data")
  d <- decisoes[decisoes$etapa == etapa, , drop = FALSE]
  if (nrow(d) == 0) return(decisoes_vazio()[cols])
  d$.pos <- seq_len(nrow(d))
  d <- d[order(d$openalex_id, d$data, d$.pos), , drop = FALSE]
  d <- d[!duplicated(d$openalex_id, fromLast = TRUE), , drop = FALSE]
  tibble::as_tibble(d[cols])
}
