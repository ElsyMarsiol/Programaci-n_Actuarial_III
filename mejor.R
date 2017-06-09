
setwd("~/GitHub/Programaci-n_Actuarial_III")
mejor <- function(state,result){
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    x <- levels(factor(data[,7]))
    v <- c("infarto", "falla", "neumonia")
    
    if (state %in% x == F){
        stop("estado inválido")
        break
    }
    if (result == "infarto") r <- 11
    else if (result == "falla") r <- 17
    else if (result == "neumonia") r <- 23
    else if (result %in% v == F){
        stop("resultado inválido")
        break
    }
    mydata <- data[data$State == state,]
    verde <- mydata[,c(2,r)]
    if (sum(verde[,2]=="Not Available") < 1) {
        des <- verde[order(as.numeric(verde[,2])),]
        des2 <- des[which(des[,2] == des[1,2]),]
        fo <- des2[order(des2[,1]),]
        fo[1,1]
        
    }
    else {
        final <- verde[- grep("Not", verde[,2]),]
        des <- final[order(as.numeric(final[,2])),]
        des2 <- des[which(des[,2] == des[1,2]),]
        fo <- des2[order(des2[,1]),]
        fo[1,1]
    }
}
mejor("TX", "falla")



