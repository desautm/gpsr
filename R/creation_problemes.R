#' Permet de creer deux documents Rmd pour les etudiants et les enseignants
#'
#' @param N Un nombre entier indiquant le nombre total de problemes a creer.
#' @param database Une tibble contenant les endroits voulus.
#' @param n_sat Le nombre de satellites a creer. La valeur par defaut est 4.
#' @param arrondi Si \code{TRUE}, nous trouvons des positions de satellites entieres. Si \code{FALSE}, nous trouvons des positions
#'   decimales plus proches de la realite.
#' @return Deux documents Rmd
#' @importFrom ezknitr ezknitr
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

  dir <- tcltk::tk_choose.dir(default = "", caption = "Choisissez un repertoire...")

  ezknitr::ezspin(
              file = system.file("misc","donnees-etudiants.R", package = "gpsr"),
              out_dir = dir,
              params = list('N'  = N, 'all_sat' = all_sat),
              keep_html = FALSE,
              keep_rmd = FALSE,
              keep_md = TRUE
              )

  fConn <- file(paste0(dir,"/donnees-etudiants.md"))
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

  ezknitr::ezspin(
    file = system.file("misc","solutions-enseignants.R", package = "gpsr"),
    out_dir = dir,
    params = list('N'  = N, 'all_sat' = all_sat, 'sols' = sols),
    keep_html = FALSE,
    keep_rmd = FALSE,
    keep_md = TRUE
  )

  fConn <- file(paste0(dir,"/solutions-enseignants.md"))
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

  testf1 <- file.rename(paste0(dir,"/donnees-etudiants.md"),paste0(dir,"/donnees-etudiants.Rmd"))
  if (!testf1) message("Le fichier donnees-etudiants a eu un probleme de changement de nom.")
  system(paste0('open "',dir,"/donnees-etudiants.Rmd",'"'))

  testf2 <- file.rename(paste0(dir,"/solutions-enseignants.md"),paste0(dir,"/solutions-enseignants.Rmd"))
  if (!testf2) message("Le fichier solutions-enseignants a eu un probleme de changement de nom.")
  system(paste0('open "',dir,"/solutions-enseignants.Rmd",'"'))

}
