test_that("montar_consulta monta blocos OR unidos por AND", {
  b <- list(blocos = list(a = c("x y", "z"), b = "w"))
  expect_equal(montar_consulta(b), '("x y" OR z) AND (w)')
})

test_that("buscar_openalex usa o fetcher com os argumentos do protocolo", {
  busca <- yaml::read_yaml(file.path("..", "..", "protocolo", "busca.yml"))
  args <- NULL
  fake <- function(...) { args <<- list(...); tibble::tibble() }
  buscar_openalex(busca, as.Date("2026-01-31"), fetcher = fake)
  expect_equal(args$entity, "works")
  expect_equal(args$from_publication_date, "2000-01-01")
  expect_equal(args$to_publication_date, "2026-01-31")
  expect_equal(args$title_and_abstract.search, montar_consulta(busca))
})

test_that("buscar_openalex propaga erro do fetcher", {
  busca <- list(desde = "2000-01-01", blocos = list(a = "x"))
  expect_error(buscar_openalex(busca, Sys.Date(), fetcher = function(...) stop("boom")), "boom")
})
