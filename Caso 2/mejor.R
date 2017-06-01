
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
    mnd <- mydata[,c(2,r)]
    if (sum(mnd[,2]=="Not Available") < 1) {
        out <- mnd[order(as.numeric(mnd[,2])),]
        out2 <- out[which(out[,2] == out[1,2]),]
        fo <- out2[order(out2[,1]),]
        fo[1,1]
        
    }
    else {
        final <- mnd[- grep("Not", mnd[,2]),]
        out <- final[order(as.numeric(final[,2])),]
        out2 <- out[which(out[,2] == out[1,2]),]
        fo <- out2[order(out2[,1]),]
        fo[1,1]
    }
}
mejor("TX", "falla")






