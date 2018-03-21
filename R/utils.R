deg2rad <- function(angle){
  return(angle*pi/180)
}

rad2deg <- function(angle){
  return(angle*180/pi)
}

angle2vec <- function(v1, v2){
  n1 <- norm(v1, "2")
  n2 <- norm(v2, "2")
  if ((n1 == 0) || (n2 == 0)) return(0)
  else{
    angle <- acos((v1 %*% v2)/(n1 * n2))
    if (is.na(angle)) return(0)
    else return(angle)
  }
}
