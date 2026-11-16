# Normalizacao de resultados brutos do openalexR para o esquema de `registros`.

normalizar_doi <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- sub("^https?://(dx[.])?doi[.]org/", "", x)
  x <- sub("^doi:[[:space:]]*", "", x)
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x
}

normalizar_titulo <- function(x) {
  x <- as.character(x)
  x <- tolower(stringi::stri_trans_general(x, "Latin-ASCII"))
  x <- gsub("[[:punct:]]", "", x)
  x <- trimws(gsub("[[:space:]]+", " ", x))
  x
}

.sem_prefixo_id <- function(x) sub("^https?://openalex[.]org/", "", as.character(x))

.unir <- function(v) {
  v <- unique(v[!is.na(v) & nzchar(v)])
  if (length(v) == 0) NA_character_ else paste(v, collapse = ";")
}

# authorships: tibble por obra; display_name = autor, affiliations = lista de
# data.frames (coluna display_name = instituicao). Uma obra sem autores => NA.
.autores_obra <- function(a) {
  if (!is.data.frame(a) || nrow(a) == 0) return(NA_character_)
  .unir(a$display_name)
}

.instituicoes_obra <- function(a) {
  if (!is.data.frame(a) || nrow(a) == 0 || is.null(a$affiliations)) return(NA_character_)
  .unir(unlist(lapply(a$affiliations, function(f) {
    if (is.data.frame(f) && nrow(f) > 0) f$display_name else NULL
  })))
}

# keywords: data.frame com display_name.
.palavras_obra <- function(k) {
  if (!is.data.frame(k) || nrow(k) == 0) return(NA_character_)
  .unir(k$display_name)
}

.col <- function(bruto, nome, n = nrow(bruto)) {
  if (nome %in% names(bruto)) bruto[[nome]] else rep(NA, n)
}

normalizar_obras <- function(bruto, rodada) {
  n <- nrow(bruto)
  if (n == 0) return(registros_vazio())
  resumo <- as.character(.col(bruto, "abstract"))
  resumo[!is.na(resumo) & !nzchar(resumo)] <- NA_character_
  titulo <- as.character(.col(bruto, "title"))
  out <- tibble::tibble(
    openalex_id = .sem_prefixo_id(bruto$id),
    openalex_ids_alternativos = NA_character_,
    doi = normalizar_doi(.col(bruto, "doi")),
    titulo = titulo,
    titulo_norm = normalizar_titulo(titulo),
    resumo = resumo,
    tem_resumo = !is.na(resumo) & nzchar(resumo),
    resumo_atualizado_em = NA_character_,
    ano = as.integer(.col(bruto, "publication_year")),
    tipo = as.character(.col(bruto, "type")),
    idioma = as.character(.col(bruto, "language")),
    fonte = as.character(.col(bruto, "source_display_name")),
    autores = vapply(.col(bruto, "authorships"), .autores_obra, character(1)),
    instituicoes = vapply(.col(bruto, "authorships"), .instituicoes_obra, character(1)),
    palavras_chave = vapply(.col(bruto, "keywords"), .palavras_obra, character(1)),
    url_oa = as.character(.col(bruto, "oa_url")),
    rodada_identificacao = as.character(rodada),
    status = "para_triagem",
    regra_id = NA_character_,
    duplicata_de = NA_character_
  )
  out[names(registros_vazio())]
}
