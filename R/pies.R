#' Displays VLXT, VSL2 and PONDRFIT as pie graphs
#'
#' @param dataset A dataset
#' @return 3 pie graphs
#' @examples
#' pies(TPRdataset)

pies <- function(dataset) {
  # grouped frequency. Displays VLXT, SL2 and PONDRFIT next to each other for the 3 groups
  predictor <- "VLXT"
  dataset$groupedVLXT <- cut(dataset$VLXT, c(-Inf, 10,30, Inf),
                             labels=c("0 to 10", "10 to 30", "greater 30"))
  groups <- "0 to 10"
  occurences <- sum(dataset$groupedVLXT == "0 to 10")
  groups <- c(groups, "10 to 30")
  occurences <- c(occurences, sum(dataset$groupedVLXT == "10 to 30"))
  groups <- c(groups, "greater 30")
  occurences <- c(occurences, sum(dataset$groupedVLXT == "greater 30"))
#
#  groupedplot <- data.frame(groups, occurences, predictor)

  #save this info to make pie charts later
  pieVLXT <- occurences

  #VSL2-------------------------------------------------------
  predictor <- "VSL2"

  dataset$groupedVSL2 <- cut(dataset$VSL2, c(-Inf, 10,30, Inf),
                             labels=c("0 to 10", "10 to 30", "greater 30"))
  groups <- "0 to 10"
  occurences <- sum(dataset$groupedVSL2 == "0 to 10")
  groups <- c(groups, "10 to 30")
  occurences <- c(occurences, sum(dataset$groupedVSL2 == "10 to 30"))
  groups <- c(groups, "greater 30")
  occurences <- c(occurences, sum(dataset$groupedVSL2 == "greater 30"))

#  groupedVSL2 <- data.frame(groups, occurences, predictor)
#  groupedplot <- rbind(groupedplot, groupedVSL2)

  #save this info to make pie charts later
  pieVSL2 <- occurences

  #PONDR FIT-------------------------------------------
  predictor <- "PONDRFIT"
  dataset$PONDRFIT <- 100* dataset$PONDRFIT
  dataset$groupedPONDRFIT <- cut(dataset$PONDRFIT, c(-Inf, 10,30, Inf),
                                 labels=c("0 to 10", "10 to 30", "greater 30"))
  groups <- "0 to 10"
  occurences <- sum(dataset$groupedPONDRFIT == "0 to 10")
  groups <- c(groups, "10 to 30")
  occurences <- c(occurences, sum(dataset$groupedPONDRFIT == "10 to 30"))
  groups <- c(groups, "greater 30")
  occurences <- c(occurences, sum(dataset$groupedPONDRFIT == "greater 30"))

#  groupedPONDRFIT <- data.frame(groups, occurences, predictor)
  # rbind adds rows to data frame
#  groupedplot <- rbind(groupedplot, groupedPONDRFIT)

  #save this info to make pie charts later
  piePONDRFIT <- occurences

  par(mfrow = c(1, 3))
  pct <- round(pieVLXT/sum(pieVLXT)*100)
  pielables <- paste(groups, pct) # add percents to labels
  pielables <- paste(pielables,"%",sep="") # ad % to labels

  pie(pieVLXT, labels = pielables, col=rainbow(length(pielables)),
      main = "VLXT % Disorder Score")

  pct <- round(pieVSL2/sum(pieVSL2)*100)
  pielables <- paste(groups, pct) # add percents to labels
  pielables <- paste(pielables,"%",sep="") # ad % to labels

  pie(pieVSL2, labels = pielables, col=rainbow(length(pielables)),
      main = "VSL2 % Disorder Score")

  pct <- round(piePONDRFIT/sum(piePONDRFIT)*100)
  pielables <- paste(groups, pct) # add percents to labels
  pielables <- paste(pielables,"%",sep="") # ad % to labels

  pie(piePONDRFIT, labels = pielables, col=rainbow(length(pielables)),
      main = "PONDRFIT % Disorder Score")

}
