# Usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < RORUTADIS-GroupAssignments.R

rm(list=ls())

library(rorutadis)
library(RXMCDA)

stopifnot(packageVersion("rorutadis") >= "0.3.1") # rorutadis in version 0.3.1 or later is required

inDirectory <- normalizePath(commandArgs()[5])
outDirectory <- normalizePath(commandArgs()[6])
errorMessage <- NULL

inputFiles <- c("alternatives.xml",
                "categories.xml")
isMandatory <- c(T, T)

trees <- list()
setwd(inDirectory)

necessaryAssignmentsFiles <- list.files(".", pattern="^necessaryAssignmentsDM\\d+\\.xml$")
possibleAssignmentsFiles <- list.files(".", pattern="^possibleAssignmentsDM\\d+\\.xml$")

for (file in c(necessaryAssignmentsFiles, possibleAssignmentsFiles)) {
  inputFiles <- c(inputFiles, file)
  isMandatory <- c(isMandatory, T)
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
  
  for (file in necessaryAssignmentsFiles) {
    dmId <- gsub("(^necessaryAssignmentsDM)|(\\.xml$)", "", file)
    
    if (!(paste("possibleAssignmentsDM", dmId, ".xml", sep = "") %in% possibleAssignmentsFiles)) {
      dataError <- paste("Possible assignments for DM", dmId, " not found.", sep = "")
      break
    }
  }
  
  for (file in possibleAssignmentsFiles) {
    dmId <- gsub("(^possibleAssignmentsDM)|(\\.xml$)", "", file)
    
    if (!(paste("necessaryAssignmentsDM", dmId, ".xml", sep = "") %in% necessaryAssignmentsFiles)) {
      dataError <- paste("Necessary assignments for DM", dmId, " not found.", sep = "")
      break
    }
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
  
  ############# assignments
  
  inputAffectations <- list()
  
  if (is.null(dataError)) {
    for (i in 3:length(trees)) {
      data <- getAlternativesAffectations(trees[[i]], alternativesIDs, categoriesIDs)
      if (data$status == "OK") {
        inputAffectations[[length(inputAffectations) + 1]] <- data[[1]]
      }
      else {
        dataError <- data$status
        break
      }
    }
  
    names(inputAffectations) <- names(trees)[-c(1, 2)]
  }
  
  if (is.null(dataError)) {
    tmpErr <- try({
      necessaryAssignmentDataIds <-
        which(unlist(sapply(names(inputAffectations), function(name) { substr(name, 0, 9) == "necessary"} )))
      possibleAssignmentDataIds <-
        which(unlist(sapply(names(inputAffectations), function(name) { substr(name, 0, 8) == "possible"} )))

      necessaryNecessaryAlternativesAffectations <-
        mergeAssignments(inputAffectations[necessaryAssignmentDataIds], TRUE)
      necessaryPossibleAlternativesAffectations <-
        mergeAssignments(inputAffectations[necessaryAssignmentDataIds], FALSE)
      possibleNecessaryAlternativesAffectations <- 
        mergeAssignments(inputAffectations[possibleAssignmentDataIds], TRUE)
      possiblePossibleAlternativesAffectations <- 
        mergeAssignments(inputAffectations[possibleAssignmentDataIds], FALSE)
    })
    
    if (inherits(tmpErr, 'try-error')) {
      # Execution error.
      errorMessage <- paste("Execution error: ", gsub("\n$", "", gsub("^.*: ", "", tmpErr[1])), sep = "")
      print (errorMessage)
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
      putAlternativesAffectations(tree, necessaryNecessaryAlternativesAffectations, alternativesIDs, categoriesIDs, TRUE)
      saveXML(tree, file = "necessaryNecessaryAssignments.xml")
      
      tree <- newXMLDoc()
      newXMLNode("xmcda:XMCDA", 
                 attrs = c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
                 suppressNamespaceWarning = TRUE, 
                 namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
                 parent = tree)
      putAlternativesAffectations(tree, necessaryPossibleAlternativesAffectations, alternativesIDs, categoriesIDs, TRUE)
      saveXML(tree, file = "necessaryPossibleAssignments.xml")
      
      tree <- newXMLDoc()
      newXMLNode("xmcda:XMCDA", 
                 attrs = c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
                 suppressNamespaceWarning = TRUE, 
                 namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
                 parent = tree)
      putAlternativesAffectations(tree, possibleNecessaryAlternativesAffectations, alternativesIDs, categoriesIDs, TRUE)
      saveXML(tree, file = "possibleNecessaryAssignments.xml")
      
      tree <- newXMLDoc()
      newXMLNode("xmcda:XMCDA", 
                 attrs = c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
                 suppressNamespaceWarning = TRUE, 
                 namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
                 parent = tree)
      putAlternativesAffectations(tree, possiblePossibleAlternativesAffectations, alternativesIDs, categoriesIDs, TRUE)
      saveXML(tree, file = "possiblePossibleAssignments.xml")
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
