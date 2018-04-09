#' Permet de creer deux documents Rmd pour les etudiants et les enseignants
#'
#' @param N Un nombre entier indiquant le nombre total de problemes a creer.
#' @param database Une tibble contenant les endroits voulus.
#' @param n_sat Le nombre de satellites a creer. La valeur par defaut est 4.
#' @param arrondi Si \code{TRUE}, nous trouvons des positions de satellites entieres. Si \code{FALSE}, nous trouvons des positions
#'   decimales plus proches de la realite.
#' @return Deux documents Rmd
#' @importFrom knitr spin
#' @export
creation_problemes <- function(N,
                         database,
                         n_sat = 4,
                         arrondi = FALSE){

  if (n_sat < 0) stop("Le nombre de satellites doit etre un entier positif.")
  if (!is.logical(arrondi)) stop("arrondi doit etre TRUE ou FALSE")

  options(digits = 15)

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
                         "title: Donnees pour les etudiants\n",
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
  fConn <- file(paste0(getwd(),"/solutions-enseignants.md"))
  Lines <- readLines(fConn)
  text_begin <- paste0(c("---\n",
                         "title: Solutions pour les enseignants\n",
                         "output:\n",
                         "  html_document:\n",
                         "    toc: yes\n",
                         "    toc_float: yes\n",
                         "  pdf_document:\n",
                         "    toc: yes\n",
                         "---\n"), collapse = "")
  writeLines(paste0(c(text_begin, Lines)),con = fConn)
  close(fConn)

  file.rename(paste0(getwd(),"/donnees-etudiants.md"), paste0(getwd(),"/donnees-etudiants.Rmd"))
  file.rename(paste0(getwd(),"/solutions-enseignants.md"), paste0(getwd(),"/solutions-enseignants.Rmd"))

}
