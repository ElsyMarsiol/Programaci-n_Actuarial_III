rankingcompleto <- function(result, num = "mejor") {
    data <- read.csv("outcome-of-care-measures.csv")
    v <- c("infarto", "falla", "neumonia")
    if (result == "infarto") col <- 11
    else if (result == "falla") col <- 17
    else if (result == "neumonia") col <- 23
    else if (result %in% v == F){
        stop("resultado inválido")
    }
    data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
    data[, 2] <- as.character(data[, 2])
    
    desput <- vector() 
    estados <- levels(data[, 7])
    for(i in 1:length(estados)) {
        databystate <- data[grep(estados[i], data[,7]), ]
        ordered <- databystate[order(databystate[, col], databystate[, 2], na.last = NA), ]
        
        if(num == "mejor") hospital <- ordered[1, 2]
        else if(num == "peor") hospital <- ordered[nrow(ordered), 2]
        else hospital <- ordered[num, 2]
        
        desput <- append(desput, c(hospital, estados[i]))
    }
    desput <- as.data.frame(matrix(desput, length(estados), 2, byrow = TRUE))
    colnames(desput) <- c("hospital", "state")
    rownames(desput) <- estados
    desput
}
tail(rankingcompleto("falla"), 10)

