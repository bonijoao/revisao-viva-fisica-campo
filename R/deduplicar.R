# Deduplicacao de obras novas contra registros existentes.
# Retorna list(novos, atualizados). Ver contrato em task-3-brief.md (emendado):
# obra de ID novo que casa com existente (DOI ou titulo_norm+ano) entra em `novos`
# como status "duplicata" e o existente vai a `atualizados` com o ID alternativo.

.chave_ta <- function(titulo_norm, ano) {
  ifelse(is.na(titulo_norm) | !nzchar(titulo_norm) | is.na(ano), NA_character_,
         paste(titulo_norm, ano, sep = "|"))
}

.ids_alt <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) character() else unlist(strsplit(x, ";", fixed = TRUE))
}

deduplicar <- function(novos, existentes) {
  vazio <- list(novos = registros_vazio(), atualizados = registros_vazio())
  if (nrow(novos) == 0) return(vazio)

  # Mapa id (principal ou alternativo) -> indice do existente
  ids_ex <- existentes$openalex_id
  mapa_id <- stats::setNames(seq_len(nrow(existentes)), ids_ex)
  if (nrow(existentes) > 0) {
    for (i in seq_len(nrow(existentes))) {
      alt <- .ids_alt(existentes$openalex_ids_alternativos[i])
      if (length(alt)) mapa_id <- c(mapa_id, stats::setNames(rep(i, length(alt)), alt))
    }
  }

  idx_mod <- integer()
  tocar <- function(i) {
    if (!(i %in% idx_mod)) idx_mod <<- c(idx_mod, i)
  }
  ex <- existentes  # copia de trabalho

  # Resumo tardio e descarte de IDs ja vistos
  novos <- novos[!duplicated(novos$openalex_id), ]
  visto <- novos$openalex_id %in% names(mapa_id)
  for (j in which(visto)) {
    i <- unname(mapa_id[novos$openalex_id[j]])
    if (isFALSE(ex$tem_resumo[i]) || is.na(ex$tem_resumo[i])) {
      if (isTRUE(novos$tem_resumo[j])) {
        ex$resumo[i] <- novos$resumo[j]
        ex$tem_resumo[i] <- TRUE
        ex$resumo_atualizado_em[i] <- novos$rodada_identificacao[j]
        tocar(i)
      }
    }
  }
  novos <- novos[!visto, ]

  # Indices de casamento entre os existentes nao-duplicata
  canon <- which(is.na(ex$status) | ex$status != "duplicata")
  doi_ex <- stats::setNames(canon[!is.na(ex$doi[canon])], ex$doi[canon][!is.na(ex$doi[canon])])
  k_ex <- .chave_ta(ex$titulo_norm, ex$ano)
  ka <- canon[!is.na(k_ex[canon])]
  ta_ex <- stats::setNames(ka, k_ex[ka])

  doi_lote <- character()  # doi -> id do primeiro no lote
  ta_lote <- character()
  k_nov <- .chave_ta(novos$titulo_norm, novos$ano)

  for (j in seq_len(nrow(novos))) {
    d <- novos$doi[j]; k <- k_nov[j]
    i <- NA_integer_
    if (!is.na(d) && d %in% names(doi_ex)) i <- unname(doi_ex[d])
    else if (!is.na(k) && k %in% names(ta_ex)) i <- unname(ta_ex[k])
    if (!is.na(i)) {
      novos$status[j] <- "duplicata"
      novos$duplicata_de[j] <- ex$openalex_id[i]
      ex$openalex_ids_alternativos[i] <-
        .unir(c(.ids_alt(ex$openalex_ids_alternativos[i]), novos$openalex_id[j]))
      tocar(i)
      next
    }
    primeiro <- NA_character_
    if (!is.na(d) && d %in% names(doi_lote)) primeiro <- doi_lote[[d]]
    else if (!is.na(k) && k %in% names(ta_lote)) primeiro <- ta_lote[[k]]
    if (!is.na(primeiro)) {
      novos$status[j] <- "duplicata"
      novos$duplicata_de[j] <- primeiro
    } else {
      if (!is.na(d)) doi_lote[[d]] <- novos$openalex_id[j]
      if (!is.na(k)) ta_lote[[k]] <- novos$openalex_id[j]
    }
  }

  list(novos = novos, atualizados = ex[idx_mod, ])
}
