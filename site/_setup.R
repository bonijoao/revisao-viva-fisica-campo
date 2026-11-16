# Preparação compartilhada por todas as páginas do site.
for (f in sort(list.files("../R", pattern = "[.]R$", full.names = TRUE))) {
  source(f, encoding = "UTF-8")
}
library(ggplot2)

estado <- ler_estado("../dados")
contagens <- contagens_prisma(estado)

vigentes <- decisao_vigente(estado$decisoes, "titulo_resumo")
incluidos <- estado$registros[estado$registros$openalex_id %in%
                                vigentes$openalex_id[vigentes$decisao == "incluir"], , drop = FALSE]
incluidos <- incluidos[order(-incluidos$ano, incluidos$titulo), , drop = FALSE]

MSG_VAZIO <- "Ainda não há registros incluídos."

# Conta valores de um campo multivalorado (separado por ";") e devolve os `n` mais frequentes.
contar_multi <- function(x, n = 15) {
  v <- trimws(unlist(strsplit(x[!is.na(x)], ";", fixed = TRUE)))
  v <- v[nzchar(v)]
  if (length(v) == 0) return(data.frame(valor = character(), n = integer()))
  t <- utils::head(sort(table(v), decreasing = TRUE), n)
  data.frame(valor = names(t), n = as.integer(t), stringsAsFactors = FALSE)
}

barras_horizontais <- function(df, titulo) {
  if (nrow(df) == 0) return(invisible(cat("Sem dados.\n\n")))
  df$valor <- factor(df$valor, levels = rev(df$valor))
  print(ggplot(df, aes(valor, n)) + geom_col(fill = "#2c7fb8") + coord_flip() +
          labs(x = NULL, y = "Registros incluídos", title = titulo) + theme_minimal())
}
