# Uso (da raiz do repositorio): Rscript scripts/buscar.R <mensal|linha_de_base>
#
# Executa uma rodada de busca, grava o material de triagem e o estado em dados/.
# Escreve em $GITHUB_OUTPUT (ou na saida padrao): rodada=, destino=, n=, arquivo=.
# O estado so e salvo depois que todo o resto deu certo.
#
# Gancho de teste: com REVISAO_FETCHER_FALHA=1 o fetcher do OpenAlex e
# substituido por um que sempre falha (usado em tests/testthat/test-rodada.R).
for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f, encoding = "UTF-8")

principal <- function(args) {
  if (length(args) != 1 || !args[1] %in% c("mensal", "linha_de_base")) {
    stop("Uso: Rscript scripts/buscar.R <mensal|linha_de_base>", call. = FALSE)
  }
  modo <- args[1]
  fetcher <- if (identical(Sys.getenv("REVISAO_FETCHER_FALHA"), "1")) {
    function(...) stop("Falha simulada do fetcher (REVISAO_FETCHER_FALHA=1).", call. = FALSE)
  } else {
    openalexR::oa_fetch
  }

  estado <- ler_estado("dados")
  busca <- yaml::read_yaml(file.path("protocolo", "busca.yml"))
  regras <- yaml::read_yaml(file.path("protocolo", "regras.yml"))
  hoje <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = "UTC"))

  out <- executar_rodada(estado, busca, regras, modo = modo, data = hoje, fetcher = fetcher)
  rodada <- out$estado$rodadas$rodada[nrow(out$estado$rodadas)]

  arquivo <- ""
  if (out$destino == "issue") {
    corpo <- montar_corpo_issue(out$candidatos, rodada)
    arquivo <- file.path("triagem", paste0("issue-", rodada, ".md"))
    escrever_utf8(corpo, arquivo)
  } else if (out$destino == "asreview") {
    arquivo <- file.path("triagem", "asreview", paste0(rodada, ".csv"))
    dir.create(dirname(arquivo), showWarnings = FALSE, recursive = TRUE)
    exportar_asreview(out$candidatos, arquivo)
  }

  salvar_estado(out$estado, "dados")
  escrever_saida_gha(rodada = rodada, destino = out$destino,
                     n = nrow(out$candidatos), arquivo = arquivo)
}

tryCatch(principal(commandArgs(trailingOnly = TRUE)), error = function(e) {
  message("Erro: ", redigir_segredos(conditionMessage(e)))
  quit(status = 1, save = "no")
})
