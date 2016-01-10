
##' Calculate Price Sensitivity Meter.
##'
##' @description Return a list with "Point of Marginal Cheapness", "Point of Marginal Expensiveness", "Optimal Price Point".
##' @param data data.frame object with data.
##' @param toocheap name of variable about too cheap price.
##' @param cheap name of variable with the answers about cheap price.
##' @param expensive name of variable with the answers about expensive price.
##' @param tooexpensive name of variable with the answers about too expensive price.
##' @param by name of variable with groups.
##'


psm <- function(data, toocheap, cheap, expensive, tooexpensive, by = NULL) {

  if (class(data) != "data.frame")
    stop("Ooops, Esperaba un data.frame")

  # Identificar el índice de las variables en el data.frame
  idx_toocheap <- match(as.character(toocheap), names(data))
  idx_cheap <- match(as.character(cheap), names(data))
  idx_expensive <- match(as.character(expensive), names(data))
  idx_tooexpensive <- match(as.character(tooexpensive), names(data))

  # Si el parámentro by es nulo (por defecto), se le añade el valor
  # 'Generic', en otro caso se busca el índice de la variable en el
  # data.frame al igual que los demás parámetros. Finalmente, con cada
  # una de las variables, incluyendo, by se construye un data.frame
  if (is.null(by)) {
    by <- "Generic"
    data <- cbind(data[, idx_toocheap], data[, idx_cheap], data[, idx_expensive],
                  data[, idx_tooexpensive], by)
  } else {
    idx_by <- match(as.character(by), names(data))
    by <- as.vector(data[, idx_by])
    by <- levels(as.factor(by))
    data <- cbind.data.frame(data[, idx_toocheap],
                             data[, idx_cheap], data[, idx_expensive],
                             data[, idx_tooexpensive], data[, idx_by])
  }

  # Función Ecdf().
  Ecdf<-function(x, crec=TRUE){
    df<-as.data.frame(table(as.factor(x)))
    df$Freq<-df$Freq/sum(df$Freq)
    df$FreqA<-cumsum(df$Freq)
    vector<-df$FreqA
    names(vector)<-df$Var1
    if (crec){
      return(vector)
    }else{
      return(1-vector)
    }
  }

  # Se inicializa una lista que almacenará en un primer
  # momento los resultados
  psmLista <- list()

  # Bucle para calcular varios PSM en función del número de grupos
  for (i in 1:length(by)) {

    # Tratamiento profiláctico
    toocheap <- as.numeric(data[which(data[, 5] == by[i]), 1])
    cheap <- as.numeric(data[which(data[, 5] == by[i]), 2])
    expensive <- as.numeric(data[which(data[, 5] == by[i]), 3])
    tooexpensive <- as.numeric(data[which(data[, 5] == by[i]), 4])

    ## Cálculo de las ecdf
    # Decrecientes
    tch <- Ecdf(toocheap, crec = FALSE)
    ch <- Ecdf(cheap, crec = FALSE)
    # Crecientes
    ex <- Ecdf(expensive)
    tex <- Ecdf(tooexpensive)

    # Se crean varios data.frame según el tipo de precio, con
    # la ecdf correspondiente y el
    # grupo generado por la iteración del for
    # (Si by = NULL el grupo será 'Generic')
    ecdf_toocheap <- data.frame(variable = c("toocheap"), ecdf = tch,
                                price = as.numeric(names(tch)), group = by[i])
    ecdf_cheap <- data.frame(variable = c("cheap"), ecdf = ch,
                             price = as.numeric(names(ch)), group = by[i])
    ecdf_expensive <- data.frame(variable = c("expensive"), ecdf = ex,
                                 price = as.numeric(names(ex)), group = by[i])
    ecdf_tooexpensive <- data.frame(variable = c("tooexpensive"), ecdf = tex,
                                    price = as.numeric(names(tex)), group = by[i])

    # Extracción de los data.frame anteriores las parejas de puntos con
    # los que se construirán las curvas del PSM

    # Too cheap
    x_toocheap <- ecdf_toocheap[, 3]
    y_toocheap <- ecdf_toocheap[, 2]

    # Cheap
    x_cheap <- ecdf_cheap[, 3]
    y_cheap <- ecdf_cheap[, 2]

    # Expensive
    x_expensive <- ecdf_expensive[, 3]
    y_expensive <- ecdf_expensive[, 2]

    # Too expensive
    x_tooexpensive <- ecdf_tooexpensive[, 3]
    y_tooexpensive <- ecdf_tooexpensive[, 2]

    ## Encontrar los puntos de intersección de las curvas del PSM. Se utiliza la función aprox 
    ## para igualar las longitudes de los vectores de las ecdf y evitar alertas.

    ## Point of Marginal Cheapness toocheap vs expensive
    y_expensivestar <- approx(x_toocheap, y_toocheap, xout = x_expensive,
                              yleft = Inf, yright = -Inf)$y
    fidy <- sum(y_expensive <= y_expensivestar)
    y_toocheapstar <- approx(x_expensive, y_expensive, xout = x_toocheap,
                             yleft = -Inf, yright = Inf)$y
    gidy <- sum(y_toocheap >= y_toocheapstar)

    # Cálculo de las pendientes, cálculo de las ordenadas y operaciones para
    # obtener la intersección a partir de la igualdad de las rectas
    fx <- y_expensive[fidy]
    gx <- y_toocheap[gidy]
    m <- (y_expensive[fidy + 1] - y_expensive[fidy])/(x_expensive[fidy +
                                                                    1] - x_expensive[fidy])
    a <- (y_toocheap[gidy + 1] - y_toocheap[gidy])/(x_toocheap[gidy +
                                                                 1] - x_toocheap[gidy])
    n <- (fx - m * x_expensive[fidy])
    b <- (gx - a * x_toocheap[gidy])
    xminprice <- (b - n)/(m - a)
    yminprice <- n + m * (xminprice)

    ## Point of Marginal Expensiveness tooexpensive vs cheap
    y_tooexpensivestar <- approx(x_cheap, y_cheap, xout = x_tooexpensive,
                                 yleft = Inf, yright = -Inf)$y
    fidy <- sum(y_tooexpensive <= y_tooexpensivestar)
    y_cheapstar <- approx(x_tooexpensive, y_tooexpensive, xout = x_cheap,
                          yleft = -Inf, yright = Inf)$y
    gidy <- sum(y_cheap >= y_cheapstar)

    fx <- y_tooexpensive[fidy]
    gx <- y_cheap[gidy]
    m <- (y_tooexpensive[fidy + 1] - y_tooexpensive[fidy])/(x_tooexpensive[fidy +
                                                                             1] - x_tooexpensive[fidy])
    a <- (y_cheap[gidy + 1] - y_cheap[gidy])/(x_cheap[gidy + 1] - x_cheap[gidy])
    n <- (fx - m * x_tooexpensive[fidy])
    b <- (gx - a * x_cheap[gidy])
    xmaxprice <- (b - n)/(m - a)
    ymaxprice <- n + m * (xmaxprice)

    ## Optimal Price Point tooexpensive vs toocheap
    y_tooexpensivestar <- approx(x_toocheap, y_toocheap, xout = x_tooexpensive,
                                 yleft = Inf, yright = -Inf)$y
    fidy <- sum(y_tooexpensive <= y_tooexpensivestar)
    y_toocheapstar <- approx(x_tooexpensive, y_tooexpensive, xout = x_toocheap,
                             yleft = -Inf, yright = Inf)$y
    gidy <- sum(y_toocheap >= y_toocheapstar)

    fx <- y_tooexpensive[fidy]
    gx <- y_toocheap[gidy]
    m <- (y_tooexpensive[fidy + 1] - y_tooexpensive[fidy])/(x_tooexpensive[fidy +
                                                                             1] - x_tooexpensive[fidy])
    a <- (y_toocheap[gidy + 1] - y_toocheap[gidy])/(x_toocheap[gidy +
                                                                 1] - x_toocheap[gidy])
    n <- (fx - m * x_tooexpensive[fidy])
    b <- (gx - a * x_toocheap[gidy])
    xopprice <- (b - n)/(m - a)
    yopprice <- n + m * (xopprice)

    # Indifference Point expensive vs cheap
    y_expensivestar <- approx(x_cheap, y_cheap, xout = x_expensive,
                              yleft = Inf, yright = -Inf)$y
    fidy <- sum(y_expensive <= y_expensivestar)
    y_cheapstar <- approx(x_expensive, y_expensive, xout = x_cheap,
                          yleft = -Inf, yright = Inf)$y
    gidy <- sum(y_cheap >= y_cheapstar)

    fx <- y_expensive[fidy]
    gx <- y_cheap[gidy]
    m <- (y_expensive[fidy + 1] - y_expensive[fidy])/(x_expensive[fidy +
                                                                    1] - x_expensive[fidy])
    a <- (y_cheap[gidy + 1] - y_cheap[gidy])/(x_cheap[gidy + 1] - x_cheap[gidy])
    n <- (fx - m * x_expensive[fidy])
    b <- (gx - a * x_cheap[gidy])
    xipprice <- (b - n)/(m - a)
    yipprice <- n + m * (xipprice)

    # Los resultados de cada iteración se alojarán en la lista creada al
    # inicio.
    psmLista[[i]] <- list(toocheap = ecdf_toocheap, cheap = ecdf_cheap,
                          expensive = ecdf_expensive, tooexpensive = ecdf_tooexpensive,
                          minprice = c(xminprice, yminprice), maxprice = c(xmaxprice,
                                                                           ymaxprice), ipprice = c(xipprice, yipprice), opprice = c(xopprice,
                                                                                                                                    yopprice), range = xmaxprice - xminprice)

  }

  # Con el número de grupos, el nombre de cada grupo y la lista
  # resultante del bucle anterior se contruye otra lista que se le
  # atribuirá una clase llamada 'psm'
  ecdfList<-list()
  ecdfList$toocheap<-do.call(rbind, lapply(psmLista, '[[', 'toocheap'))
  ecdfList$cheap<-do.call(rbind, lapply(psmLista, '[[', 'cheap'))
  ecdfList$expensive<-do.call(rbind, lapply(psmLista, '[[', 'expensive'))
  ecdfList$tooexpensive<-do.call(rbind, lapply(psmLista, '[[', 'tooexpensive'))
  ecdfDF<-do.call(rbind, ecdfList)
  row.names(ecdfDF)<-NULL
  
  psmObject <- list(n.groups = length(by), 
                    names.groups = by,
                    data =  ecdfDF,
                    results = psmLista)
  names(psmObject$results)<-psmObject$names.groups
  class(psmObject) <- "psm"
  return(psmObject)
}
