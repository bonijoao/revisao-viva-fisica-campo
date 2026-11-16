# Contagens PRISMA 2020 (recalculadas de registros + decisoes) e diagrama.

CONTAGENS_CAMPOS <- c("identificados", "duplicatas", "excluidos_regra", "triados",
                      "excluidos_triagem", "nao_triados_parada", "incluidos", "aguardando")

contagens_zeradas <- function() {
  as.list(stats::setNames(rep(0L, length(CONTAGENS_CAMPOS)), CONTAGENS_CAMPOS))
}

# Contagens de uma rodada, agrupadas pela rodada_identificacao do registro.
contagens_rodada <- function(registros, vigentes, rodada) {
  r <- registros[which(registros$rodada_identificacao == rodada), , drop = FALSE]
  pt <- r[which(r$status == "para_triagem"), , drop = FALSE]
  dec <- vigentes$decisao[match(pt$openalex_id, vigentes$openalex_id)]
  n <- function(x) sum(x, na.rm = TRUE)
  inc <- n(dec == "incluir")
  exc <- n(dec == "excluir")
  list(
    identificados = nrow(r),
    duplicatas = n(r$status == "duplicata"),
    excluidos_regra = n(r$status == "excluido_regra"),
    triados = inc + exc,
    excluidos_triagem = exc,
    nao_triados_parada = n(dec == "nao_triado_parada"),
    incluidos = inc,
    aguardando = sum(is.na(dec))
  )
}

somar_contagens <- function(lista) {
  if (length(lista) == 0) return(contagens_zeradas())
  as.list(stats::setNames(
    vapply(CONTAGENS_CAMPOS, function(k) sum(vapply(lista, function(x) x[[k]], numeric(1))),
           numeric(1)),
    CONTAGENS_CAMPOS))
}

contagens_prisma <- function(estado) {
  rod <- estado$rodadas
  rod <- rod[order(rod$data, seq_len(nrow(rod))), , drop = FALSE]
  vig <- decisao_vigente(estado$decisoes, "titulo_resumo")
  por_rodada <- lapply(stats::setNames(rod$rodada, rod$rodada),
                       function(r) contagens_rodada(estado$registros, vig, r))
  k <- length(por_rodada)
  nova <- if (k > 0) por_rodada[[k]] else contagens_zeradas()
  anterior <- somar_contagens(if (k > 1) por_rodada[-k] else list())
  pend <- tibble::tibble(rodada = as.character(names(por_rodada)),
                         n = unname(vapply(por_rodada, function(x) as.numeric(x$aguardando), numeric(1))))
  pend <- pend[pend$n > 0, , drop = FALSE]
  list(anterior = anterior, nova = nova, pendentes = pend)
}

# Diagrama PRISMA 2020 (template de revisao atualizada). Retorna o widget de
# PRISMA2020::PRISMA_flowdiagram. Rotulos em portugues.
desenhar_prisma <- function(contagens) {
  a <- contagens$anterior
  n <- contagens$nova
  nao_feito <- "N\u00e3o realizado nesta fase"
  f <- function(x) as.character(x)
  d <- PRISMA2020::PRISMA_data(utils::read.csv(
    system.file("extdata", "PRISMA.csv", package = "PRISMA2020")))

  d$previous_studies <- f(a$incluidos)
  d$previous_reports <- f(a$incluidos)
  d$previous_text <- "Estudos anteriores"
  d$previous_studies_text <- "Estudos inclu\u00eddos em rodadas anteriores"
  d$previous_reports_text <- "Estudos inclu\u00eddos em rodadas anteriores"
  d$newstud_text <- "Identifica\u00e7\u00e3o de novos estudos via bases de dados"
  d$database_results <- f(n$identificados)
  d$database_results_text <- "Registros identificados (OpenAlex)"
  d$register_results <- f(0)
  d$duplicates <- f(n$duplicatas)
  d$duplicates_text <- "Registros duplicados"
  d$excluded_automatic <- f(n$excluidos_regra)
  d$excluded_automatic_text <- "Exclu\u00eddos por regras autom\u00e1ticas"
  d$excluded_other <- f(0)
  d$records_screened <- f(n$triados + n$nao_triados_parada)
  d$records_screened_text <- "Registros triados (t\u00edtulo e resumo)"
  d$records_excluded <- f(n$excluidos_triagem + n$nao_triados_parada)
  d$records_excluded_text <- sprintf(
    "Registros exclu\u00eddos (%d por decis\u00e3o; %d n\u00e3o triados por crit\u00e9rio de parada)",
    as.integer(n$excluidos_triagem), as.integer(n$nao_triados_parada))
  for (campo in c("dbr_sought_reports", "dbr_notretrieved_reports", "dbr_assessed")) {
    d[[campo]] <- nao_feito
  }
  d$dbr_sought_reports_text <- "Textos completos buscados"
  d$dbr_notretrieved_reports_text <- "Textos completos n\u00e3o recuperados"
  d$dbr_assessed_text <- "Textos completos avaliados"
  d$dbr_excluded <- data.frame(reason = nao_feito, n = NA_character_, stringsAsFactors = FALSE)
  d$dbr_excluded_text <- "Textos completos exclu\u00eddos"
  d$new_studies <- f(n$incluidos)
  d$new_reports <- f(n$incluidos)
  d$new_studies_text <- "Inclu\u00eddos na triagem por t\u00edtulo e resumo"
  d$new_reports_text <- "Inclu\u00eddos na triagem por t\u00edtulo e resumo"
  d$total_studies <- f(a$incluidos + n$incluidos)
  d$total_reports <- f(a$incluidos + n$incluidos)
  d$total_studies_text <- "Total inclu\u00eddo na triagem por t\u00edtulo e resumo"
  d$total_reports_text <- "Total inclu\u00eddo na triagem por t\u00edtulo e resumo"
  d$identification_text <- "Identifica\u00e7\u00e3o"
  d$screening_text <- "Triagem"
  d$included_text <- "Inclu\u00eddos"

  suppressWarnings(PRISMA2020::PRISMA_flowdiagram(
    d, previous = TRUE, other = FALSE, interactive = FALSE,
    detail_databases = FALSE, detail_registers = FALSE, side_boxes = TRUE))
}
