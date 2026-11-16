for (f in sort(list.files(file.path("..", "..", "R"), pattern = "[.]R$", full.names = TRUE))) {
  source(f, encoding = "UTF-8")
}

# Estado com a linha de base ja executada (so a linha em rodadas, sem registros).
estado_com_base <- function() {
  e <- list(registros = registros_vazio(), decisoes = decisoes_vazio(), rodadas = rodadas_vazio())
  l <- rodadas_vazio()[NA_integer_, ]
  l$rodada <- "linha-de-base"; l$data <- "2026-11-01"; l$modo <- "linha_de_base"
  l[c("n_identificados", "n_duplicatas", "n_excluidos_regra", "n_para_triagem",
      "n_triados", "n_nao_triados_parada", "n_incluidos")] <- 0L
  e$rodadas <- tibble::as_tibble(rbind(e$rodadas, l))
  e
}

