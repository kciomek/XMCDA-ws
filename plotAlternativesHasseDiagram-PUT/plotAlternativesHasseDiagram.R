# Usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < plotAlternativesHasseDiagram.R

rm(list=ls())

library(hasseDiagram)
library(RXMCDA)
library(RCurl)

inDirectory <- normalizePath(commandArgs()[5])
outDirectory <- normalizePath(commandArgs()[6])
errorMessage <- NULL

inputFiles <- c("alternatives.xml",
                "preferenceRelation.xml",
                "parameters.xml")
isMandatory <- c(T, T, T)

trees <- list()
setwd(inDirectory)

for (i in seq_len(length(inputFiles))) {
  tree <- NULL
  
  if (file.exists(inputFiles[i])) {  
    tmpErr <- try({
      tree <- xmlTreeParse(inputFiles[i], useInternalNodes=TRUE)
    })
    
    if (inherits(tmpErr, 'try-error')) {
      trees[[i]] <- paste("Error reading file ", inputFiles[i],": ", gsub("\n$", "", tmpErr[1]), sep = "")
    } else if (checkXSD(tree) == 0) {
        trees[[i]] <- paste(inputFiles[i], " is not XMCDA valid.", sep="")
    } else {
        trees[[i]] <- tree
    }
  } else {
    if (isMandatory[i]) {
      trees[[i]] <- paste("Missing file: ", inputFiles[i], ".", sep="")
    } else {
      trees[i] <- list(NULL)
    }
  }
}

names(trees) <- gsub("\\.xml$", "", inputFiles)

fileErrors <- which(sapply(trees, typeof) == "character")
    
if (length(fileErrors) == 0) {
  # There was not file error.
  dataError <- NULL
  
  ############# mandatory
  
  #### alternatives
  
  if (is.null(dataError)) {
    data <- getNumberOfAlternatives(trees$alternatives)
    if (data$status == "OK") nrAlternatives <- data[[1]]
    else dataError <- data$status
  }
  
  if (is.null(dataError)) {
    data <- getAlternativesIDs(trees$alternatives)
    if (data$status == "OK") alternativesIDs <- data[[1]]
    else dataError <- data$status
  }
  
  #### assignment Comparisons
  
  comparisons <- NULL
  
  if (is.null(dataError)) {
    data <- getAlternativesComparisonsValues(trees$preferenceRelation, alternativesIDs)
    if (data$status == "OK") comparisons <- data[[1]]
    else dataError <- data$status
  }
  
  if (is.null(dataError)) {
    data <- getParameters(trees$parameters, "cluster")
    if (data$status == "OK") cluster <- data[[1]]
    else dataError <- data$status
  }
  
  if (is.null(dataError)) {
    data <- getParameters(trees$parameters, "transitiveReduction")
    if (data$status == "OK") transitiveReduction <- data[[1]]
    else dataError <- data$status
  }
  
  if (is.null(dataError)) {
    data <- getParameters(trees$parameters, "shape")
    if (data$status == "OK") shape <- data[[1]]
    else dataError <- data$status
  }
  
  if (is.null(dataError)) {
    data <- getParameters(trees$parameters, "arrows")
    if (data$status == "OK") arrows <- data[[1]]
    else dataError <- data$status
  }
  
  if (is.null(dataError)) {
    comparisonsMatrix <- matrix(data = c(FALSE),
                                ncol = nrAlternatives,
                                nrow = nrAlternatives)
    colnames(comparisonsMatrix) <- rownames(comparisonsMatrix) <- alternativesIDs
    
    for (i in seq_len(nrow(comparisons))) {
      comparisonsMatrix[comparisons[i, 1],
                        comparisons[i, 2]] <- TRUE
    }
    
    tmpErr <- try({
      tmp_out_file <- "tmp_out.png"
      setwd(outDirectory)
      png(tmp_out_file, type="cairo")
      hasse(comparisonsMatrix,
            alternativesIDs,
            parameters = list(newpage = FALSE,
                              cluster = cluster,
                              transitiveReduction = transitiveReduction,
                              shape = shape,
                              arrows = arrows))
      dev.off()
      img = readBin(tmp_out_file, "raw", file.info(tmp_out_file)[1, "size"])
      b64 = base64Encode(img, "character")
      file.remove(tmp_out_file)
      
      #pdf("diagram.pdf")
      #hasse(comparisonsMatrix,
      #      alternativesIDs,
      #      parameters = list(newpage = FALSE,
      #                        cluster = cluster,
      #                        transitiveReduction = transitiveReduction,
      #                        shape = shape,
      #                        arrows = arrows))
      #dev.off()
    })
    
    if (inherits(tmpErr, 'try-error')) {
      # Execution error.
      errorMessage <- paste("Execution error: ", gsub("\n$", "", gsub("^.*: ", "", tmpErr[1])), sep = "")
    }
    else {
      # Success.
      setwd(outDirectory)
      
      tree <- newXMLDoc()
      newXMLNode("xmcda:XMCDA", 
                 attrs = c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
                 suppressNamespaceWarning = TRUE, 
                 namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
                 parent = tree)
      putAlternativesPlot(tree, b64[1], alternativesIDs, mcdaConcept = "hasseDiagram")
      saveXML(tree, file = "hasseDiagram.xml")
    }
  }
  else {
    # There was data error.
    errorMessage <- dataError
  }
} else {
  # There was at least one file error.
  errorMessage <- paste(unlist(trees[fileErrors]), collapse = " ")
}

setwd(outDirectory)

tree <- newXMLDoc()
newXMLNode("xmcda:XMCDA", 
           attrs = c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
           suppressNamespaceWarning = TRUE, 
           namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
           parent = tree)

if (is.null(errorMessage)) {
  putLogMessage(tree, "OK", name = "executionStatus")
} else {
  putLogMessage(tree, errorMessage, name = "Error")
}

saveXML(tree, file = "messages.xml")
