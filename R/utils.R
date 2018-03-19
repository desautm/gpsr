deg2rad <- function(angle){
  return(angle*pi/180)
}

rad2deg <- function(angle){
  return(angle*180/pi)
}

angle2vec <- function(v1, v2){
  n1 <- norm(v1, "2")
  n2 <- norm(v2, "2")
  return(acos((v1 %*% v2)/(n1 * n2)))
}
