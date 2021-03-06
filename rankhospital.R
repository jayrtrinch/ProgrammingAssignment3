
rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  table.no <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  if (!state %in% data$State) stop("invalid state", call. = F)
  
  if (!outcome %in% names(table.no)) stop("invalid outcome", call. = F)
  
  sub <- subset(data, select = c(2, table.no[[outcome]]), subset = (State == state))
  
  sub <- sub[order(sub[[1]]), ]  
  
  sub[[2]] <- suppressWarnings(as.numeric(sub[[2]]))
  
  sub[[2]] <- rank(sub[[2]], na.last = "keep", ties.method = "first")
  
  if (num == "best") as.character(subset(sub, select = 1, sub[[2]] == 1))
  
  else if (num == "worst") as.character(subset(sub, select = 1, sub[[2]] == max(sub[[2]], na.rm = T)))
  
  else as.character(subset(sub, select = 1, sub[[2]] == num))
}