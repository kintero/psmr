

plot.psm <- function(psmObject, 
                     col = c("chartreuse4", "chartreuse3", "red3", "red4"), 
                     names = c("Too cheap", "Cheap", "Expensive", "Too expensive")) {

  # Parada si la clase no es psm
  if (class(psmObject) != "psm")
    stop("Ooops, Esperaba un objeto de la clase psm")

  if (length(col) < 4)
    stop ("Es necesario indicar cuatro colores")

  # Colores
  col_toocheap = col[1]
  col_cheap = col[2]
  col_expensive = col[3]
  col_tooexpensive = col[4]
  col_vector = c(col_toocheap, col_cheap, col_expensive, col_tooexpensive)
  
  if (length(names) < 4)
    stop ("Es necesario indicar cuatro nombres")

  # Inicialización de objeto que será utilizado por lattice para dibujar
  # las curvas
  datadf <- data.frame()

  # Inicialización del objeto que será utilizado para dibujar los puntos
  # de intersección. Solo serán dibujados en el caso de ser un único grupo
  puntos <- data.frame()

  # Bucle para extraer los valores de interés según el número de grupos
  for (i in 1:psmObject$n.groups) {
    tmp <- rbind(psmObject$results[[i]]$toocheap, psmObject$results[[i]]$cheap,
                 psmObject$results[[i]]$expensive, psmObject$results[[i]]$tooexpensive)
    # El primero se graba directamente en datadf, los siguientes se adjuntan
    # con rbind
    if (i == 1) {
      datadf <- tmp
    } else {
      datadf <- rbind(datadf, tmp)
    }
    # Se opera de forma similar con los puntos
    puntos[i, 1] <- psmObject$results[[i]]$minprice[1]
    puntos[i, 2] <- psmObject$results[[i]]$minprice[2]
    puntos[i, 3] <- psmObject$results[[i]]$maxprice[1]
    puntos[i, 4] <- psmObject$results[[i]]$maxprice[2]
    puntos[i, 5] <- psmObject$results[[i]]$ipprice[1]
    puntos[i, 6] <- psmObject$results[[i]]$ipprice[2]
    puntos[i, 7] <- psmObject$results[[i]]$opprice[1]
    puntos[i, 8] <- psmObject$results[[i]]$opprice[2]
    names(puntos) <- c("xminprice", "yminprice", "xmaxprice", "ymaxprice",
                       "xipprice", "yipprice", "xopprice", "yopprice")

  }
  
  levels(datadf$variable)<-names

  # Si existe un solo grupo (por defecto) se utiliza la siguiente función
  # que añade los puntos de intersección en el gráfico
  if (psmObject$n.groups == 1) {
    xyplot(ecdf ~ price | group, group = variable, type = "l", data = datadf,
           col = col_vector, lty = c(1, 2, 2, 1), lwd = 2, auto.key = TRUE, 
           par.strip.text=list(col=c("white")),
           strip=strip.custom(bg="black"),
           key = list(type = "l", text = list(label = levels(datadf$variable)),
                      lines = list(lty = c(1, 2, 2, 1), lwd = 2), 
                      column = nlevels(datadf$variable),
                      space = "top", col = col_vector), xlab = list(label = "Price",
                                                                    fontsize = 14), ylab = list(label = ""), scales = list(y = list(tck = c(1,
                                                                                                                                            0)), x = list(tck = c(1, 0))), panel = function(x, y, ...) {
                                                                                                                                              panel.xyplot(x, y, ...)
                                                                                                                                              panel.points(puntos$xminprice, puntos$yminprice, col = "black",
                                                                                                                                                           pch = 19)
                                                                                                                                              panel.points(puntos$xmaxprice, puntos$ymaxprice, col = "black",
                                                                                                                                                           pch = 19)
                                                                                                                                              panel.points(puntos$xipprice, puntos$yipprice, col = "black",
                                                                                                                                                           pch = 19)
                                                                                                                                              panel.points(puntos$xopprice, puntos$yopprice, col = "black",
                                                                                                                                                           pch = 19)
                                                                                                                                            })

    # En otro caso no se dibujan los puntos.
  } else {
    xyplot(ecdf ~ price | group, group = variable, type = "l", data = datadf,
           col = col_vector, lty = c(1, 2, 2, 1), lwd = 2, auto.key = TRUE,
           par.strip.text=list(col=c("white")),
           strip=strip.custom(bg="black"),
           key = list(type = "l", text = list(label = levels(datadf$variable)),
                      lines = list(lty = c(1, 2, 2, 1), lwd = 2),
                      column = nlevels(datadf$variable),
                      space = "top", col = col_vector), xlab = list(label = "Price",
                                                                    fontsize = 14), ylab = list(label = ""),  aspect = 0.5)

  }
}
