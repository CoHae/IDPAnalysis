#' Displays VLXT, VSL2 and PONDRFIT in a 3 dimensional scatterplot
#'
#' @param dataset A dataset
#' @return A 3 dimensional scatterplot
#' @examples
#' scatterpl3d(TPRdataset)

scatterpl3d <- function(dataset) {
dataset$PONDRFIT <- 100* dataset$PONDRFIT
scatterplot3d(dataset$PONDRFIT, dataset$VLXT, dataset$VSL2,
              pch = 16,
              xlab = "PONDR_FIT",
              ylab = "VLXT",
              zlab = "VSL2",
              xlim=c(0,100),
              ylim=c(0,100),
              zlim=c(0,100)
)
dataset$PONDRFIT <- dataset$PONDRFIT/100
}
