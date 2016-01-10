
vplot<-function(psmObject, ...) {
  
  # Parada si la clase no es psm
  if (class(psmObject) != "psm")
    stop("Ooops, Esperaba un objeto de la clase psm")
  
  datadf <- data.frame()
  datadf<-psmObject$data
  datadf$ecdf<-datadf$ecdf*100
  
  xyplot(ecdf ~ price | group, group = variable, type = "l", data = datadf, ...)
  }
