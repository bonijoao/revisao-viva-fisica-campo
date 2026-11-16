# Leitura e escrita do estado (tres CSVs em `dir`). UTF-8, LF, NA como "".

ARQUIVOS_ESTADO <- c(
  registros = "registros.csv", decisoes = "decisoes.csv", rodadas = "rodadas.csv"
)

ler_tabela <- function(caminho, esquema, vazio) {
  if (!file.exists(caminho)) return(vazio())
  df <- readr::read_csv(
    caminho,
    col_types = paste(esquema, collapse = ""),
    na = "", locale = readr::locale(encoding = "UTF-8"),
    progress = FALSE
  )
  df <- df[, names(esquema)]
  attr(df, "spec") <- NULL
  attr(df, "problems") <- NULL
  df
}

ler_estado <- function(dir = "dados") {
  list(
    registros = ler_tabela(file.path(dir, ARQUIVOS_ESTADO[["registros"]]), ESQUEMA_REGISTROS, registros_vazio),
    decisoes  = ler_tabela(file.path(dir, ARQUIVOS_ESTADO[["decisoes"]]), ESQUEMA_DECISOES, decisoes_vazio),
    rodadas   = ler_tabela(file.path(dir, ARQUIVOS_ESTADO[["rodadas"]]), ESQUEMA_RODADAS, rodadas_vazio)
  )
}

salvar_estado <- function(estado, dir = "dados") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  for (nome in names(ARQUIVOS_ESTADO)) {
    readr::write_csv(estado[[nome]], file.path(dir, ARQUIVOS_ESTADO[[nome]]), na = "", eol = "\n")
  }
  invisible(NULL)
}
