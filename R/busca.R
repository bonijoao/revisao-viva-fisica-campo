# Busca no OpenAlex a partir do protocolo (protocolo/busca.yml).

#' Monta a consulta booleana: "(t1 OR t2) AND (u1 OR u2)".
#' Termos com espaco vao entre aspas duplas.
montar_consulta <- function(busca) {
  blocos <- vapply(busca$blocos, function(termos) {
    termos <- as.character(unlist(termos))
    termos <- ifelse(grepl("[[:space:]]", termos), paste0('"', termos, '"'), termos)
    paste0("(", paste(termos, collapse = " OR "), ")")
  }, character(1))
  paste(unname(blocos), collapse = " AND ")
}

#' Busca works no OpenAlex. `fetcher` e injetavel para testes; erros propagam.
buscar_openalex <- function(busca, ate, fetcher = openalexR::oa_fetch, ...) {
  options(openalexR.mailto = Sys.getenv("OPENALEX_EMAIL"))
  chave <- Sys.getenv("OPENALEX_API_KEY")
  if (nzchar(chave)) options(openalexR.apikey = chave)
  fetcher(
    entity = "works",
    title_and_abstract.search = montar_consulta(busca),
    from_publication_date = busca$desde,
    to_publication_date = as.character(ate),
    verbose = FALSE,
    ...
  )
}
