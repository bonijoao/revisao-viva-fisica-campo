# Uso (da raiz do repositorio):
#   Rscript scripts/importar_asreview.R <arquivo_exportado> [revisor]
#
# A rodada vem do nome do arquivo (<rodada>.csv); os IDs enviados vem de
# triagem/asreview/<rodada>.csv (o que foi exportado). Registra as decisoes,
# atualiza n_triados, n_nao_triados_parada e n_incluidos da rodada e move o
# arquivo importado para triagem/asreview/importados/.
# Revisor: 2o argumento, ou env REVISOR, ou "asreview".
for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f, encoding = "UTF-8")

principal <- function(args) {
  if (!length(args) %in% 1:2) {
    stop("Uso: Rscript scripts/importar_asreview.R <arquivo_exportado> [revisor]", call. = FALSE)
  }
  arquivo <- args[1]
  if (!file.exists(arquivo)) stop("Arquivo não encontrado: ", arquivo, call. = FALSE)
  revisor <- if (length(args) == 2) args[2] else Sys.getenv("REVISOR", "asreview")
  rodada <- sub("[.]csv$", "", basename(arquivo), ignore.case = TRUE)
  enviado <- file.path("triagem", "asreview", paste0(rodada, ".csv"))
  if (!file.exists(enviado)) stop("Exportação original não encontrada: ", enviado, call. = FALSE)
  if (normalizePath(arquivo) == normalizePath(enviado)) {
    stop("O arquivo importado não pode ser o próprio ", enviado,
         "; coloque o exportado do ASReview em outra pasta.", call. = FALSE)
  }

  ler <- function(p) readr::read_csv(p, col_types = readr::cols(.default = "c"),
                                     na = character(), progress = FALSE,
                                     locale = readr::locale(encoding = "UTF-8"))
  ids_enviados <- ler(enviado)$openalex_id
  exportado <- ler(arquivo)

  estado <- ler_estado("dados")
  if (!rodada %in% estado$rodadas$rodada) {
    stop("Rodada ", rodada, " não encontrada em rodadas.csv.", call. = FALSE)
  }
  novas <- importar_asreview(exportado, ids_enviados, rodada, agora_utc(), revisor)
  estado$decisoes <- registrar_decisoes(estado$decisoes, novas)
  n_parada <- sum(novas$decisao == "nao_triado_parada")
  n_inc <- sum(novas$decisao == "incluir")
  estado$rodadas <- atualizar_rodada(estado$rodadas, rodada,
                                     n_triados = nrow(novas) - n_parada,
                                     n_nao_triados_parada = n_parada,
                                     n_incluidos = n_inc)

  destino <- file.path("triagem", "asreview", "importados", basename(arquivo))
  dir.create(dirname(destino), showWarnings = FALSE, recursive = TRUE)
  if (!file.copy(arquivo, destino, overwrite = TRUE)) stop("Falha ao copiar para ", destino, call. = FALSE)
  salvar_estado(estado, "dados")
  file.remove(arquivo)

  cat(n_inc, " incluídos, ", sum(novas$decisao == "excluir"), " excluídos, ",
      n_parada, " não triados (parada)\n", sep = "")
}

tryCatch(principal(commandArgs(trailingOnly = TRUE)), error = function(e) {
  message("Erro: ", conditionMessage(e))
  quit(status = 1, save = "no")
})
