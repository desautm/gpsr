#' @export
creation_problemes <- function(N,
                         database,
                         n_sat = 4,
                         arrondi = FALSE,
                         output = c("pdf", "word", "html")){

  if (n_sat < 0) stop("Le nombre de satellites doit etre un entier positif.")
  if (!is.logical(arrondi)) stop("arrondi doit etre TRUE ou FALSE")

  options(digits = 15)

  output <- match.arg(output, c("pdf", "word", "html"))

  all_sat <- lapply(1:N, function(x) matrix(0, n_sat, 5))
  sols <- character(length = N)
  for (i in (1:N)){
    numero <- sample(1:nrow(database), 1)
    all_sat[[i]] <- creation_gps(database, numero, num_satellites = n_sat, arrondi = arrondi)
    sols[i] <- database[numero, ]$nom
  }

  knitr::spin(system.file("misc","donnees-etudiants.R", package = "gpsr"),
              knit = TRUE,
              report = FALSE,
              format = c("Rmd"))
  fConn <- file(paste0(getwd(),"/donnees-etudiants.md"))
  Lines <- readLines(fConn)
  text_begin <- paste0(c("---\n",
                         "title: Données pour les étudiants\n",
                         "output:\n",
                         "  html_document:\n",
                         "    toc: yes\n",
                         "    toc_float: yes\n",
                         "  pdf_document:\n",
                         "    toc: yes\n",
                         "---\n"), collapse = "")
  writeLines(paste0(c(text_begin, Lines)),con = fConn)
  close(fConn)
  knitr::spin(system.file("misc","solutions-enseignants.R", package = "gpsr"),
              knit = TRUE,
              report = FALSE,
              format = c("Rmd"))

  #where_donnees <- paste0(.libPaths()[1],"/gpsr/rmarkdown/templates/docs-enseignants/donnees-etudiants.Rmd")
  where_donnees <- system.file("misc","donnees-etudiants.Rmd", package = "gpsr")
  #where_solutions <- paste0(.libPaths()[1],"/gpsr/rmarkdown/templates/docs-enseignants/solutions-enseignants.Rmd")
  where_solutions <- system.file("misc","solutions-enseignants.Rmd", package = "gpsr")

  # if (output == "pdf"){
  #   rmarkdown::render(
  #     where_donnees,
  #     output_format = "pdf_document",
  #     output_dir = getwd()
  #   )
  #   rmarkdown::render(
  #     where_solutions,
  #     output_format = "pdf_document",
  #     output_dir = getwd()
  #   )
  #   where_donnees_pdf <- paste0(getwd(),"/donnees-etudiants.pdf")
  #   where_solutions_pdf <- paste0(getwd(),"/solutions-enseignants.pdf")
  #   system(paste0('open "',where_donnees_pdf,'"'))
  #   system(paste0('open "',where_solutions_pdf,'"'))
  # }
  # else if (output == "word"){
  #   rmarkdown::render(
  #     where_donnees,
  #     output_format = "word_document",
  #     output_dir = getwd()
  #   )
  #   rmarkdown::render(
  #     where_solutions,
  #     output_format = "word_document",
  #     output_dir = getwd()
  #   )
  #   where_donnees_word <- paste0(getwd(),"/donnees-etudiants.docx")
  #   where_solutions_word <- paste0(getwd(),"/solutions-enseignants.docx")
  #   system(paste0('open "',where_donnees_word,'"'))
  #   system(paste0('open "',where_solutions_word,'"'))
  # }
  # else if (output == "html"){
  #   rmarkdown::render(
  #     where_donnees,
  #     output_format = "html_document",
  #     output_dir = getwd()
  #   )
  #   rmarkdown::render(
  #     where_solutions,
  #     output_format = "html_document",
  #     output_dir = getwd()
  #   )
  #   where_donnees_html <- paste0(getwd(),"/donnees-etudiants.html")
  #   where_solutions_html <- paste0(getwd(),"/solutions-enseignants.html")
  #   system(paste0('open "',where_donnees_html,'"'))
  #   system(paste0('open "',where_solutions_html,'"'))
  # }

}
