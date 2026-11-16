# Regras de exclusao automatica: so excluem, nunca incluem.

# Devolve funcao(registros) -> logical (TRUE = a regra bate).
.compilar_regra <- function(regra) {
  id <- regra$id
  tipo <- regra$tipo
  if (is.null(tipo)) stop("Regra ", id, ": campo 'tipo' ausente.", call. = FALSE)
  switch(tipo,
    tipo_em = {
      valores <- as.character(unlist(regra$valores))
      function(r) !is.na(r$tipo) & r$tipo %in% valores
    },
    nenhum_termo = {
      campos <- as.character(unlist(regra$campos))
      termos <- normalizar_titulo(as.character(unlist(regra$termos)))
      if (length(termos) == 0 || anyNA(termos) || any(!nzchar(termos))) {
        stop("Regra ", id, ": termo vazio apos normalizacao.", call. = FALSE)
      }
      function(r) {
        partes <- lapply(campos, function(c) {
          v <- as.character(r[[c]])
          v[is.na(v)] <- ""
          v
        })
        texto <- normalizar_titulo(do.call(paste, c(partes, sep = " ")))
        texto[is.na(texto)] <- ""
        achou <- Reduce(`|`, lapply(termos, function(t) grepl(t, texto, fixed = TRUE)))
        !achou
      }
    },
    ano_menor_que = {
      valor <- regra$valor
      function(r) !is.na(r$ano) & r$ano < valor
    },
    stop("Regra ", id, ": tipo de regra desconhecido '", tipo, "'.", call. = FALSE)
  )
}

aplicar_regras <- function(registros, regras, decididos = character()) {
  testes <- lapply(regras, .compilar_regra)
  elegivel <- registros$status == "para_triagem" & !(registros$openalex_id %in% decididos)
  elegivel[is.na(elegivel)] <- FALSE
  pendente <- elegivel
  for (i in seq_along(regras)) {
    bate <- pendente & testes[[i]](registros)
    bate[is.na(bate)] <- FALSE
    registros$status[bate] <- "excluido_regra"
    registros$regra_id[bate] <- regras[[i]]$id
    pendente <- pendente & !bate
  }
  registros
}
