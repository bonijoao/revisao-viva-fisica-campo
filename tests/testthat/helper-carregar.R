for (f in sort(list.files(file.path("..", "..", "R"), pattern = "[.]R$", full.names = TRUE))) {
  source(f, encoding = "UTF-8")
}
