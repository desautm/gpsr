#' Une tibble contenant des donnees geodetiques pour GPS
#'
#' Les localisations geodetiques des merveilles utilisees
#' pour construire les problemes GPS. Les informations sont
#' le \code{nom} de la merveille, la \code{latitude} de la
#' merveille, la \code{longitude} de la merveille et
#' son \code{altitude}.
#'
#' @docType data
#'
#' @usage data(merveilles)
#'
#' @format Un objet de classe \code{"tibble"}
#'
#' @keywords datasets
#'
#' @examples
#' data(merveilles)
#' noms <- merveilles$nom
#' longitudes <- merveilles$longitude
"merveilles"
