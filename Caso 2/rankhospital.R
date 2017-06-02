rankhospital <- function(state, result, num = "mejor"){
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
        
        des<- verde[order(as.numeric(final[,2])),]
        if (num == "mejor") num <- 1
        else if (num == "peor") num <- nrow(des)
        else if (num > nrow(des)) {
            stop(return(NA))
        }
        i <- 0
        while (out[i+1,2] != out[num,2]){
            i <- i + 1
        }
        dif <- num - i
        des2 <- des[which(des[,2] == des[num,2]),]
        fo <- des2[order(des2[,1]),]
        fo[dif,1] 
    }
    
    else  {
        final <- verde[- grep("Not", verde[,2]),]
        des <- final[order(as.numeric(final[,2])),]
        if (num == "mejor") num <- 1
        else if (num == "peor") num <- nrow(des)
        else if (num > nrow(des)) {
            stop(return(NA))
        }
        i <- 0
        while (des[i+1,2] != des[num,2]){
            i <- i + 1
        }
        dif <- num - i
        des2 <- des[which(des[,2] == des[num,2]),]
        fo <- des2[order(des2[,1]),]
        fo[dif,1]
    }
}

rankhospital("TX","falla",4)

