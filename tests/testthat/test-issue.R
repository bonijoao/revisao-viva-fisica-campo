cand <- function(n = 2, resumo = "Um resumo.", titulo = "Titulo") {
  r <- registros_vazio()[rep(NA_integer_, n), ]
  r$openalex_id <- paste0("W", seq_len(n))
  r$doi <- paste0("10.1/ab", seq_len(n))
  r$titulo <- paste(titulo, seq_len(n))
  r$resumo <- rep(resumo, n)
  r$ano <- rep(2024L, n)
  r$fonte <- rep("Revista X", n)
  r$url_oa <- paste0("https://exemplo.org/", seq_len(n))
  r
}

test_that("test_issue_formato", {
  corpo <- montar_corpo_issue(cand(2), "2026-12")
  expect_match(corpo, "## Triagem 2026-12 — 2 registros", fixed = TRUE)
  expect_equal(lengths(regmatches(corpo, gregexpr("- [ ] **", corpo, fixed = TRUE))), 2)
  expect_match(corpo, "<!-- id:W1 -->", fixed = TRUE)
  expect_match(corpo, "<!-- id:W2 -->", fixed = TRUE)
  expect_match(corpo, "https://openalex.org/W1", fixed = TRUE)
  expect_match(corpo, "https://doi.org/10.1/ab1", fixed = TRUE)
})

test_that("test_issue_sem_resumo", {
  c1 <- cand(1, resumo = NA_character_)
  c1$doi <- NA_character_
  c1$url_oa <- NA_character_
  corpo <- montar_corpo_issue(c1, "2026-12")
  expect_match(corpo, "(sem resumo no OpenAlex)", fixed = TRUE)
  expect_false(grepl("\\bNA\\b", corpo))
  expect_false(grepl("doi.org", corpo, fixed = TRUE))
})

test_that("test_issue_limite_tamanho", {
  c50 <- cand(50, resumo = strrep("a", 5000), titulo = strrep("t", 295))
  corpo <- montar_corpo_issue(c50, "2026-12")
  expect_lte(nchar(corpo), 65000)
  expect_equal(nrow(ler_decisoes_issue(corpo, "2026-12", "2026-12-01", "r")), 50)
})

test_that("resumo longo e truncado em 1500 com reticencias", {
  corpo <- montar_corpo_issue(cand(1, resumo = strrep("a", 5000)), "2026-12")
  expect_match(corpo, paste0(strrep("a", 1500), "…"), fixed = TRUE)
  expect_false(grepl(strrep("a", 1501), corpo, fixed = TRUE))
})

test_that("titulo com caracteres especiais nao quebra a linha", {
  c1 <- cand(1, titulo = "A ] b * c\nd")
  corpo <- montar_corpo_issue(c1, "2026-12")
  d <- ler_decisoes_issue(sub("- [ ]", "- [x]", corpo, fixed = TRUE), "2026-12", "2026-12-01", "r")
  expect_equal(d$decisao, "incluir")
  linha <- grep("- [ ] **", strsplit(corpo, "\n")[[1]], fixed = TRUE, value = TRUE)
  expect_length(linha, 1)
  expect_match(linha, "(2024)", fixed = TRUE)
})

test_that("test_ida_e_volta_issue", {
  corpo <- montar_corpo_issue(cand(3), "2026-12")
  corpo <- sub("- [ ]", "- [x]", corpo, fixed = TRUE)
  d <- ler_decisoes_issue(corpo, "2026-12", "2026-12-05", "bonijoao")
  expect_equal(names(d), names(decisoes_vazio()))
  expect_equal(d$openalex_id, c("W1", "W2", "W3"))
  expect_equal(d$decisao, c("incluir", "excluir", "excluir"))
  expect_true(all(d$etapa == "titulo_resumo" & d$via == "issue"))
  expect_true(all(d$rodada == "2026-12" & d$data == "2026-12-05" & d$revisor == "bonijoao"))
})

test_that("test_ler_issue_tolerante", {
  corpo <- montar_corpo_issue(cand(3), "2026-12")
  corpo <- sub("- [ ]", "- [X]", corpo, fixed = TRUE)
  corpo <- sub("<!-- id:W2 -->", "", corpo, fixed = TRUE)
  d <- ler_decisoes_issue(corpo, "2026-12", "2026-12-05", "r")
  expect_equal(d$openalex_id, c("W1", "W3"))
  expect_equal(d$decisao, c("incluir", "excluir"))
  expect_equal(nrow(d), 2)
})
