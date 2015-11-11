
summary.psm<-function(psmObject){

  if (class(psmObject) != "psm")
    stop("Ooops, Esperaba un objeto de la clase psm")

  attach(psmObject)
  minprice<-vector()
  maxprice<-vector()
  ipprice<-vector()
  opprice<-vector()

  for (i in 1:n.groups){
    minprice[i]<-results[[i]]$minprice[1]
    maxprice[i]<-results[[i]]$maxprice[1]
    ipprice[i]<-results[[i]]$ipprice[1]
    opprice[i]<-results[[i]]$opprice[1]
  }
  df<-data.frame(Group=names.groups,
                 Min = minprice,
                 Max = maxprice,
                 Indifference = ipprice,
                 Optimal = opprice)
  if (substring(Sys.getlocale("LC_CTYPE"), 1, 5)=="es_ES"){
    names(df)<-c("Grupo","Mín", "Máx", "Indiferencia", "Óptimo")
  }
  detach(psmObject)
  return(df)
}
