#' # Solutions pour les enseignants
#'
#+ solutions, results = "asis", echo=FALSE
for (i in (1:N)){
  cat(paste0("Exercice ",i,". "))
  cat(sols[i])
  cat("\n\n")
}
