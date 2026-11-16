regras <- yaml::read_yaml(file.path(dirname(dirname(getwd())), "protocolo", "regras.yml"))$regras

reg <- function(id = "W1", titulo = "Fisica no campo", resumo = "x", tipo = "article",
                ano = 2020L, status = "para_triagem", palavras_chave = NA_character_) {
  r <- registros_vazio()[1, ]
  r$openalex_id <- id; r$titulo <- titulo; r$resumo <- resumo; r$tipo <- tipo
  r$ano <- ano; r$status <- status; r$palavras_chave <- palavras_chave
  r
}

test_that("test_regra_tipo", {
  out <- aplicar_regras(reg(tipo = "erratum"), regras)
  expect_equal(out$status, "excluido_regra")
  expect_equal(out$regra_id, "R1")
})

test_that("test_regras_sem_acento", {
  out <- aplicar_regras(reg(titulo = "FÍSICA na Escola do Campo"), regras)
  expect_equal(out$status, "para_triagem")
  expect_true(is.na(out$regra_id))
})

test_that("test_regras_resumo_na", {
  a <- aplicar_regras(reg(titulo = "Ensino de física em assentamentos", resumo = NA), regras)
  expect_equal(a$status, "para_triagem")
  b <- aplicar_regras(reg(titulo = "Gestão escolar", resumo = NA), regras)
  expect_equal(b$status, "excluido_regra")
  expect_equal(b$regra_id, "R2")
})

test_that("test_regras_nao_retroativas", {
  r <- reg(titulo = "Gestão escolar")
  out <- aplicar_regras(r, regras, decididos = "W1")
  expect_equal(out$status, "para_triagem")
})

test_that("test_regras_ignoram_duplicatas", {
  r <- reg(titulo = "Gestão escolar", status = "duplicata")
  out <- aplicar_regras(r, regras)
  expect_equal(out$status, "duplicata")
  expect_true(is.na(out$regra_id))
})

test_that("test_primeira_regra_vence", {
  out <- aplicar_regras(reg(tipo = "editorial", titulo = "Gestão escolar"), regras)
  expect_equal(out$regra_id, "R1")
})

test_that("regra desconhecida e termo vazio geram erro", {
  expect_error(aplicar_regras(reg(), list(list(id = "RX", tipo = "foo"))), "RX")
  expect_error(aplicar_regras(reg(), list(list(id = "RY", tipo = "nenhum_termo",
    campos = "titulo", termos = "!!"))), "RY")
})
