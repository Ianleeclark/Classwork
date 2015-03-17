calcCorr <- function (x) {
  # Function to calculate average correlation CEO correlation for sales,
  #   profits, and 5 year return.
  #
  # Args: 
  #   x: Data frame containing CEO data
  #
  # Returns:
  #   CEO data frame with new correlation columns
  
  totalComp <- x$TotalSal
  IndustryAvgs <- data.frame(salesCor   = rep(NA, 1),
                             profitsCor = rep(NA, 1),
                             roiCor     = rep(NA, 1))
  
  IndustryAvgs$salesCor   <- cor(totalComp, x$Sales.)    
  IndustryAvgs$profitsCor <- cor(totalComp, x$Profits.) 
  IndustryAvgs$roiCor     <- cor(totalComp, x$Return5Yrs) 
  
  return(IndustryAvgs)
}


avgSals <- function(x) {
  # Function to calculate average CEO total compensation per industry
  #
  # Args: 
  #   x: Data frame containing CEO data
  #
  # Returns:
  #   The greatest-to-least ordering of CEO total compensation

  len <- length(unique(x$Industry))
  IndustryAvgs <- data.frame(Industry = unique(x$Industry), AvgSal = rep(NA, len), stringsAsFactors = FALSE)
  FreqTable <- as.data.frame(table(x$Industry))
  
  for(i in 1:len) {
    industry <- IndustryAvgs$Industry[i]
    out <- (split(x, x$Industry == industry))["TRUE"]
    freq <- (FreqTable[FreqTable$Var1 == industry,])$Freq
    
    IndustryAvgs$AvgSal[i] <- sum(out[[1]]$TotalSal) / freq
  }
  
  
  return(IndustryAvgs[order(-IndustryAvgs[, "AvgSal"]),])  
}

loadCEOData <- function() {
  # Function to load and clean CEO data from Excel worksheet into a data frame
  #
  # Returns:
  #   CEO data frame

  ceo.data <- readWorksheetFromFile("./CEO.xlsx", sheet = "CEO_Data", region = "A1:U801")
  
  ceo.data$Other. <- as.numeric(ceo.data$Other.) 
  ceo.data$Profits. <- as.numeric(ceo.data$Profits.)

  ceo.data[is.na(ceo.data)] <- 0
  
  ceo.data$TotalSal <- rowSums(ceo.data[c("Salary.", "Bonus.", "Other.", "Stock_Gains.")])

  return(ceo.data)
}

otherQuestions <- function(x) {
  # Function to answer questions; done in print() format to later put into TeX
  #
  # Args: 
  #   x: Data frame containing CEO data


  #Answers Question 3
  ceo.degrees <- x[c("MBA", "MSPhD")]
  deg <- sapply(ceo.degrees, function(x) {mean(x) * 100})
  print(deg)
  
  #Answers Question 4
  ceo.states <- as.data.frame(table(x$STofBirth))
  m <- ggplot() + geom_bar(aes(x = ceo.states$Var1, y = ceo.states$Freq, width = 1.0), stat = "identity") 
                + xlab("States") 
                + ylab("Occurrence of State") 
                + theme(panel.background = element_blank(), plot.title = element_text(size = 20, face = "bold"), legend.position = "none") 
                + ggtitle("Occurences of CEO Per State") + geom_text(aes(35.9, 87, angle = 90,  label = "New York", color = "red"))
  print(m)
  
  #Answers Question 5
  print(calcCorr(x))
  
  #Answers Question 6
  print(avgSals())
}

