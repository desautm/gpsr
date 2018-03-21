#' @export
creation_devoir <- function(N,
                            data,
                            num_satellites = 4,
                            arrondi = FALSE){

  donnees <- lapply(1:N, function(x) matrix(0, num_satellites, 5))
  sols <- character(length = N)
  for (i in (1:N)){
    numero <- sample(1:nrow(data), 1)
    donnees[[i]] <- creation_gps(data, numero, num_satellites = num_satellites)
    sols[i] <- data[numero, ]$nom
  }

  newList <- list("satellites" = donnees, "positions" = sols)

  return(newList)

}
