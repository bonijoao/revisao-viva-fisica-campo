# Uso (da raiz do repositorio):
#   ISSUE_TITLE="Triagem 2026-12" Rscript scripts/registrar_issue.R <arquivo_corpo> <numero_issue> <revisor>
#
# Le o corpo da issue fechada, registra as decisoes em dados/decisoes.csv e
# atualiza issue, n_triados e n_incluidos da rodada em dados/rodadas.csv.
# Imprime o resumo para o comentario (e resumo= em $GITHUB_OUTPUT, se houver).
for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f, encoding = "UTF-8")

principal <- function(args) {
  if (length(args) != 3) {
    stop("Uso: Rscript scripts/registrar_issue.R <arquivo_corpo> <numero_issue> <revisor>", call. = FALSE)
  }
  titulo <- Sys.getenv("ISSUE_TITLE")
  m <- regmatches(titulo, regexec("Triagem (\\S+)", titulo))[[1]]
  if (length(m) < 2) stop("ISSUE_TITLE sem 'Triagem <rodada>': '", titulo, "'.", call. = FALSE)
  rodada <- m[2]
  numero <- suppressWarnings(as.integer(args[2]))
  if (is.na(numero)) stop("Número de issue inválido: ", args[2], call. = FALSE)

  estado <- ler_estado("dados")
  if (!rodada %in% estado$rodadas$rodada) {
    stop("Rodada ", rodada, " não encontrada em rodadas.csv.", call. = FALSE)
  }
  corpo <- readLines(args[1], encoding = "UTF-8", warn = FALSE)
  novas <- ler_decisoes_issue(corpo, rodada, agora_utc(), args[3])
  desconhecidos <- setdiff(novas$openalex_id, estado$registros$openalex_id)
  if (length(desconhecidos)) {
    message("Ignorados (IDs fora de registros.csv): ", paste(desconhecidos, collapse = ", "))
    novas <- novas[!novas$openalex_id %in% desconhecidos, ]
  }

  reg <- estado$registros
  candidatos <- reg$openalex_id[reg$rodada_identificacao %in% rodada & reg$status %in% "para_triagem"]
  n_inc <- sum(novas$decisao == "incluir")
  n_exc <- sum(novas$decisao == "excluir")
  n_sem <- length(setdiff(candidatos, novas$openalex_id))

  estado$decisoes <- registrar_decisoes(estado$decisoes, novas)
  estado$rodadas <- atualizar_rodada(estado$rodadas, rodada,
                                     issue = numero, n_triados = nrow(novas), n_incluidos = n_inc)
  salvar_estado(estado, "dados")

  resumo <- paste0(n_inc, " incluídos, ", n_exc, " excluídos, ", n_sem, " sem marcador")
  cat(resumo, "\n", sep = "")
  if (nzchar(Sys.getenv("GITHUB_OUTPUT"))) escrever_saida_gha(rodada = rodada, resumo = resumo)
}

tryCatch(principal(commandArgs(trailingOnly = TRUE)), error = function(e) {
  message("Erro: ", redigir_segredos(conditionMessage(e)))
  quit(status = 1, save = "no")
})
