#' Permet de creer les positions et le temps de quatre satellites
#'
#' @param data Une tibble contenant les positions des endroits sur terre. Le tibble contient le \code{nom},
#'   la \code{latitude} en degres, la \code{longitude} en degres et l'\code{altitude} en metres.
#' @param k Le numero de l'endroit dont on veut trouver la position GPS.
#' @param num_satellites Le nombre de satellites a creer. La valeur par defaut est 4.
#' @param arrondi Si \code{TRUE}, nous trouvons des positions de satellites entieres. Si \code{FALSE}, nous trouvons des positions
#'   decimales plus proches de la realite.
#' @return sat_final Une tibble dont les lignes representent les satellites et les colonnes les coordonnees spatiales et temporelles.
#'   En coordonnees cartesiennes x, y et z, ainsi qu'en temps t.
#' @export

creation_gps <- function(data,
                         num,
                         num_satellites = 4,
                         arrondi = FALSE){

  if (num > nrow(data)) stop("Le numero de l'endroit est plus grand que le nombre d'endroits dans la base de donnees.")

  # Initialisation des constantes
  Rt <- 6378137 # Le rayon moyen de la Terre en metres
  Rs <- 20200000 # L'altitude des satellites en metres
  c <- 299792458/(Rt*1000) # La vitesse de la lumiere en rayons de la Terre par millisecondes

  # Position de l'endroit sur Terre
  endroit <- data[num, ]
  latitude <- deg2rad(endroit$latitude)
  longitude <- deg2rad(endroit$longitude)
  altitude <- endroit$altitude

  # Conversion en coordonnees cartesiennes
  x <- (Rt+altitude)*cos(latitude)*cos(longitude)
  y <- (Rt+altitude)*cos(latitude)*sin(longitude)
  z <- (Rt+altitude)*sin(latitude)
  pos <- matrix(c(x/Rt, y/Rt, z/Rt), 1, 3)

  # Angle minimal et maximal pour avoir des satellites visibles
  phimax <- pi/2
  phimin <- acos((Rt+altitude)/Rs)

  # Creation des positions des 4 satellites
  #sat <- matrix(0, num_satellites, 3)
  num_choix <- choose(n = num_satellites, k = 2) # Nombre de combinaisons de 2 satellites parmi num_satellites
  test <- logical(num_choix)
  D <- matrix(0, num_satellites-1, 3)

  # On s'assure que les satellites ne soient pas coplanaires en calculant le rang de la matrice
  # On place les satellites pour qu'ils soient à plus de 15 degrés l'un de l'autre
  cutoff <- 1
  while ((Matrix::rankMatrix(D) < 3) && (any(test == FALSE))){

    if (arrondi){
      # sat <- matrix(sample(-3:3, num_satellites*3, replace = TRUE), num_satellites, 3)
      # On peut choisir la position arrondie des satellites en sachant dans quel octant l'endroit se trouve.
      # On peut donc s'assurer que les satellites sont visibles.
      sat <- matrix(0, num_satellites, 3)
      if (x >= 0) sat[(1:num_satellites), 1] <- matrix(sample(0:3, num_satellites), num_satellites, 1)
      else sat[(1:num_satellites), 1] <- matrix(sample(-3:0, num_satellites), num_satellites, 1)
      if (y >= 0) sat[(1:num_satellites), 2] <- matrix(sample(0:3, num_satellites), num_satellites, 1)
      else sat[(1:num_satellites), 2] <- matrix(sample(-3:0, num_satellites), num_satellites, 1)
      if (z >= 0) sat[(1:num_satellites), 3] <- matrix(sample(0:3, num_satellites), num_satellites, 1)
      else sat[(1:num_satellites), 3] <- matrix(sample(-3:0, num_satellites), num_satellites, 1)
    }
    else{
      # On trouve les angles aleatoires des satellites
      lambda <- runif(num_satellites, min = -pi, max = pi)
      phi <- runif(num_satellites, min = phimin, max = phimax)

      # Positions des satellites
      xsat <- (Rt+Rs)*cos(phi)*cos(lambda)
      ysat <- (Rt+Rs)*cos(phi)*sin(lambda)
      zsat <- (Rt+Rs)*sin(phi)
      sat <- matrix(c(xsat, ysat, zsat)/Rt, num_satellites, 3)
    }

    # Pas de satellites coplanaires
    for (i in (1:(num_satellites-1))){
      D[i, ] <- sat[num_satellites, ] - sat[i, ]
    }

    # Pas de satellites a moins de 15 degres l'un de l'autre
    k <- 1
    for (i in (1:num_satellites)){
      if (i == num_satellites) sequence <- NULL
      else sequence <- (i + 1):num_satellites
      for (j in sequence){
        test[k] <- rad2deg(angle2vec(sat[i, ], sat[j, ])) > 15
        k <- k + 1
      }
    }
    k <- NULL

    cutoff <- cutoff + 1
    if (cutoff > 1000) stop("Trop grand nombre d'iterations, arret du programme.")

  }

  if (arrondi) sat_rot <- sat # On ne fait pas de rotation si on veut des valeurs arrondies
  else {
    # Matrices de rotation
    # On s'assure de cette façon que les satellites sont visibles
    Ry <- matrix(c(cos(latitude), 0, sin(latitude),
                   0, 1, 0,
                   -sin(latitude), 0, cos(latitude)), 3, 3, byrow = TRUE)
    Rz <- matrix(c(cos(longitude), -sin(longitude), 0,
                   sin(longitude), cos(longitude), 0,
                   0, 0, 1), 3, 3, byrow = TRUE)
    sat_rot <- sat %*% t(Rz %*% Ry)
  }

  # Calcul du temps de parcours du signal pour se rendre de l'endroit jusqu'au satellite
  temps_parcours <- matrix(0, num_satellites, 1)
  for (i in (1:num_satellites)){
    temps_parcours[i] <- norm(sat_rot[i, ] - pos, "2")/c
  }

  # Temps de reception
  temps_reception <- max(temps_parcours)*(1 + runif(1))

  # Temps d'envoi du signal
  temps_parcours <- temps_reception - temps_parcours

  # Creation d'une matrice pour enregistrer les reponses
  sat_final <- cbind((1:num_satellites), sat_rot, temps_parcours)
  #sat_final <- tibble::as.tibble(cbind(sat_rot, temps_parcours))
  colnames(sat_final) <- c("Satellite", "x", "y", "z", "t")

  return(sat_final)

}


