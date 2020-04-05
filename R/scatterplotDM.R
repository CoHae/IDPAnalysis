#' Displays VLXT, VSL2 and PONDRFIT next to each other for the 3 groups
#'
#' @param dataset A dataset
#' @return 3 dimensional scatterplot
#' @examples
#' scatterplDM(TPRdataset)


scatterpl3DDM <- function(dataset) {
dataset <- dataset[order(dataset$DM),]

#PONDRFIT% is as 0.2 in the dataset, I make it 20.
dataset$PONDRFIT <- dataset$PONDRFIT*100

colors <- c("tomato", "steelblue", "palegreen3", "yellow")

# "as.factor" necessary to designate $DM as categorical variables
# that determines color indicators
colors <- colors[as.factor(dataset$DM)]

levels(dataset$DM) <- c("No Disease or Mutagenesis noted",
                       "Disease noted",
                      "Disease and Mutagenesis noted",
                       "Mutagenesis noted")
#Avg disorder: PONDRFIT, VLXT, VSL2 are in columns 16 through 18,
# axis limits are 0 to 1
#scatterplot3d(TPRanalysiswithDM[,16:18], pch = 16, color=colors,
#             xlim=c(0,100),
#             ylim=c(0,100),
#             zlim=c(0,100))

#Avg disorder: PONDRFIT%, VLXT%, VSL2% are in columns 20 through 22
scatterplot3d(dataset$PONDRFIT, dataset$VLXT, dataset$VSL2, pch = 16, color=colors,
              xlab = "PONDR_FIT",
              ylab = "VLXT",
              zlab = "VSL2",
              xlim=c(0,100),
              ylim=c(0,100),
              zlim=c(0,100))

legend("topleft", legend = levels(dataset$DM), cex = 0.7,
       #       bty = "n", #bg = "transparent",
       col = c("tomato", "steelblue", "palegreen3", "yellow"), pch = 16)
#      ,inset = -0.50, xpd = TRUE, horiz = TRUE)

# don't do the next statement bc the return of a function is its last assessment
# $PONDRFIT <- dataset$PONDRFIT/100
}
