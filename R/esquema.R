# Esquema dos dados: tipos por coluna, na ordem canonica.
# Tipos: "c" = character, "l" = logical, "i" = integer.

ESQUEMA_REGISTROS <- c(
  openalex_id = "c", openalex_ids_alternativos = "c", doi = "c", titulo = "c",
  titulo_norm = "c", resumo = "c", tem_resumo = "l", resumo_atualizado_em = "c",
  ano = "i", tipo = "c", idioma = "c", fonte = "c", autores = "c",
  instituicoes = "c", palavras_chave = "c", url_oa = "c",
  rodada_identificacao = "c", status = "c", regra_id = "c", duplicata_de = "c"
)

ESQUEMA_DECISOES <- c(
  openalex_id = "c", etapa = "c", decisao = "c", motivo = "c",
  via = "c", rodada = "c", data = "c", revisor = "c"
)

ESQUEMA_RODADAS <- c(
  rodada = "c", data = "c", modo = "c", hash_busca = "c", hash_regras = "c",
  n_identificados = "i", n_duplicatas = "i", n_excluidos_regra = "i",
  n_para_triagem = "i", n_triados = "i", n_nao_triados_parada = "i",
  n_incluidos = "i", issue = "i"
)

tabela_vazia <- function(esquema) {
  vazio <- list(c = character(), l = logical(), i = integer())
  tibble::as_tibble(lapply(esquema, function(t) vazio[[t]]))
}

registros_vazio <- function() tabela_vazia(ESQUEMA_REGISTROS)
decisoes_vazio <- function() tabela_vazia(ESQUEMA_DECISOES)
rodadas_vazio <- function() tabela_vazia(ESQUEMA_RODADAS)
