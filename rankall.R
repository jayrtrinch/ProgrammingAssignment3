
rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  table.no <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  if (!outcome %in% names(table.no)) stop("invalid outcome", call. = F)
  
  data <- subset(data, select = c(2, 7, table.no[[outcome]]))
  
  data[[3]] <- suppressWarnings(as.numeric(data[[3]]))
  
  rankhospital <- function(sub) {
    
    sub <- sub[order(sub[[1]]), ]
    
    sub[[3]] <- rank(sub[[3]], na.last = "keep", ties.method = "first")
    
    
    if (num == "best") as.character(subset(sub, select = 1, sub[[3]] == 1))

    else if (num == "worst") as.character(subset(sub, select = 1, sub[[3]] == max(sub[[3]], na.rm = T)))

    else as.character(subset(sub, select = 1, sub[[3]] == num))
    
  }

  list <- sapply(split(data, data$State), rankhospital)
  state <- names(list)
  list[list == "character(0)"] <- NA
  df <- data.frame(hospital = list, state = state, stringsAsFactors = F)
  df

}