# Gera tests/fixtures/openalex_amostra.rds (chamada real, 25 works). Rodar da raiz.
# Chave opcional: OPENALEX_API_KEY (lida so do ambiente).
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f, encoding = "UTF-8")
busca <- yaml::read_yaml("protocolo/busca.yml")
res <- buscar_openalex(
  busca, Sys.Date(),
  fetcher = function(...) openalexR::oa_fetch(..., options = list(per_page = 25, pages = 1))
)
saveRDS(res, "tests/fixtures/openalex_amostra.rds")
