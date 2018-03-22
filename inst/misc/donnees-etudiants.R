#+ donnees-etudiants, results='asis', echo=FALSE
for (i in (1:N)){
  cat(c("###  Exercice ", i, "."))
  print(knitr::kable(all_sat[[i]], align = "c", digits = c(8, 8, 8, 8, 6)))
  cat("\n")
  if ((i %% 5) == 0) cat("\\newpage\n")
}
