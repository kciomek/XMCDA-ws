# Usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < RORUTADIS-GroupAssignments.R

rm(list=ls())

library(rorutadis)
library(RXMCDA)

inDirectory <- normalizePath(commandArgs()[5])
outDirectory <- normalizePath(commandArgs()[6])
errorMessage <- NULL

inputFiles <- c("alternatives.xml",
                "categories.xml",
                "necessary.xml")
isMandatory <- c(T, T, T)

trees <- list()
setwd(inDirectory)

allFiles <- list.files(".")
for (file in allFiles) {
  if (substring(file, 1, 11) == "assignments") {
    inputFiles <- c(inputFiles, file)
    isMandatory <- c(isMandatory, T)
  }
}

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
  
  if (length(trees) <= 3) {
    dataError <- "No assignments file found."
  }
  
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
  
  #### categories
  
  if (is.null(dataError)) {
    data <- getNumberOfCategories(trees$categories)
    if (data$status == "OK") nrCategories <- data[[1]]
    else dataError <- data$status
  }
  
  if (is.null(dataError)) {
    data <- getCategoriesIDs(trees$categories)
    if (data$status == "OK") categoriesIDs <- data[[1]]
    else dataError <- data$status
  }
  
  ############# parameters
  
  if (is.null(dataError)) {
    data <- getParameters(trees$necessary, "necessary")
    if (data$status == "OK") necessary <- data[[1]]
    else dataError <- data$status
  }
  
  ############# assignments
  
  inputAffectations <- list()
  
  for (i in 4:length(trees)) {
    data <- getAlternativesAffectations(trees[[i]], alternativesIDs, categoriesIDs)
    if (data$status == "OK") {
      inputAffectations[[length(inputAffectations) + 1]] <- data[[1]]
    }
    else {
      dataError <- data$status
      break
    }
  }
  
  if (is.null(dataError)) {
    tmpErr <- try({
      alternativesAffectations <- mergeAssignments(inputAffectations, necessary)
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
      putAlternativesAffectations(tree, alternativesAffectations, alternativesIDs, categoriesIDs, TRUE)
      saveXML(tree, file = "assignments.xml")
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
