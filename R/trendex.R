#' Trend Extraction
#'
#'
#' Trend extraction through the empirical mode
#' decomposition (EMD) and ensemble EMD (EEMD) from non-stationary time series.
#'
#' @param x a regular time series (ts object).
#' @param ... Additional arguments to pass to the function \code{\link[Rlibeemd]{ceemdan}}.
#'
#' @return An object of class "trendex".
#'
#' @export
#'
#' @importFrom stats t.test var is.ts

trendex <- function(x, ...){
  xname = deparse(substitute(x))
  if(!is.ts(x)){
    stop("x is not ts object")
  }

  # normalize time series
  sx <- x
  # sd of noise add -  max orthongality
  noise <- sdnoise(sx)$noiseorth
  # decomposition
  decomp <- Rlibeemd::ceemdan(sx, noise_strength = noise)
  # reconstruction rule
  n <- length(decomp[1,])
  reconst <- decomp
  colnames(reconst) <- paste0("d", 1:n)
  for(i in 1:n){
    reconst[,i]<- apply(as.matrix(decomp[,1:i]), 1, sum)
  }
  # teste t
  rt <- apply(reconst, 2, function(x) t.test(x)$p.value)
  id <- min(which(rt<0.1))
  if(id==Inf){
    id <- n
  }
  xtrend <- apply(as.matrix(decomp[,-(1:(id-1))]), 1, sum)
  attributes(xtrend) <- attributes(x)
  xcycle <- x - xtrend
  res <- list(cycle = xcycle, trend = xtrend, xname = xname, call = as.call(match.call()),
              title = "Trend Extraction", method = "ceemdan", x = x)

  return(structure(res, class = "trendex"))
}




sdnoise <- function(x, ...){
  noise <- seq(0.05, 1, 0.05)
  orth <- vector()
  for(i in 1:length(noise)){
    imfx <- Rlibeemd::ceemdan(x, noise_strength = noise[i])
    orth[i] <- vardecomp(x = x, imfx = imfx)
  }
  id <- which.min(abs(orth - 1))
  noiseorth <- noise[id]
  return(list(noise=noise, orth=orth, noiseorth=noiseorth))
}


vardecomp <- function(x, imfx){
  vari <- apply(imfx, 2, var)
  sum(vari)/var(x)
}


#' Transforma numero e texto formatado
#'
#' Transforma numero e texto formatado para serem mostrados na
#' tabela, o numero arredondado para 3 casas decimais e separado
#' por virgula
#'
#' @param x um objeto com valores numericos
#' @param dig the minimum number of digits to the right
#' of the decimal point in formatting real numbers
#' in non-scientific formats
#'
#' @return o mesmo objeto que foi fornecido com numeros
#' transformados em texto formatado
#'
#' @export
#'
#' @importFrom graphics grid legend lines par plot


n2tab <- function(x, dig = 3){
  format(round(x, dig), digits = dig, nsmall = dig, decimal.mark=",")
}

#' Methods for mFilter objects
#'
#' Common methods for all mFilter objects usually created by the mFilter function.
#'
#' @param x an object of class "trendex"; usually, a result of a call to \code{\link{trendex}}.
#' @param reference.grid logical. if true grid lines are drawn.
#' @param col color of the graph (see \code{\link[graphics]{plot}}).
#' @param ... further arguments passed to or from other methods.
#'
#' @return "trendex" object
#'
#' @export
#'

plot.tendex <- function (x, reference.grid = TRUE, col = "steelblue", ...)
{
  if (!inherits(x, "tendex"))
    stop("method is only for tendex objects")
  opar <- par(no.readonly = TRUE)
  par(mfrow = c(2, 1), mar = c(3, 2, 2, 1))
  ag <- list(...)
  if (is.null(ag$main))
    main <- paste(x$title, "of", x$xname)
  ylim <- range(c(x$x, x$trend), na.rm = TRUE)
  plot(x$x, type = "l", main = main, ylab = "", ylim = ylim,
       col = col, ...)
  lines(x$trend, col = "red")
  if (reference.grid)
    grid()
  legend("topleft", legend = c(x$xname, "trend"), col = c(col,
                                                          "red"), lty = rep(1, 2), ncol = 2)
  plot(x$cycle, type = "l", main = "Cyclical component (deviations from trend)",
       ylab = "", col = col, ...)
  if (reference.grid)
    grid()
  par(opar)
  invisible(x)
}
