getmin<-function(psmObject){
  # Parada si la clase no es psm
  if (class(psmObject) != "psm")
    stop("Ooops, Esperaba un objeto de la clase psm")
  minimos<-lapply(psmObject$results, '[[', 'minprice')
  minimos<-as.data.frame(t(as.data.frame(minimos)))
  names(minimos)<-c("x","y")
  minimos$y<-minimos$y*100
  return(minimos)}
min<-getmin(psmObject)

getmax<-function(psmObject){
  # Parada si la clase no es psm
  if (class(psmObject) != "psm")
    stop("Ooops, Esperaba un objeto de la clase psm")
  maximos<-lapply(psmObject$results, '[[', 'maxprice')
  maximos<-as.data.frame(t(as.data.frame(maximos)))
  names(maximos)<-c("x","y")
  maximos$y<-maximos$y*100
  return(maximos)}
max<-getmax(psmObject)

getopt<-function(psmObject){
  # Parada si la clase no es psm
  if (class(psmObject) != "psm")
    stop("Ooops, Esperaba un objeto de la clase psm")
  optimos<-lapply(psmObject$results, '[[', 'opprice')
  optimos<-as.data.frame(t(as.data.frame(optimos)))
  names(optimos)<-c("x","y")
  optimos$y<-optimos$y*100
  return(optimos)}
opt<-getopt(psmObject)

getindiff<-function(psmObject){
  # Parada si la clase no es psm
  if (class(psmObject) != "psm")
    stop("Ooops, Esperaba un objeto de la clase psm")
  indiferentes<-lapply(psmObject$results, '[[', 'ipprice')
  indiferentes<-as.data.frame(t(as.data.frame(indiferentes)))
  names(indiferentes)<-c("x","y")
  indiferentes$y<-indiferentes$y*100
  return(indiferentes)}
indiff<-getindiff(psmObject)