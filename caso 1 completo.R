
#PARTE 1
mediacontaminante <- function(directorio = "C:/Users/ELSYMARISOL/Desktop/specdata", contaminante, id){
    setwd(directorio)
    nobs<-c()
    for (j in id){
        
        data <- read.csv(sprintf("%03d.csv", j))
        
        if (contaminante == "sulfate"){
            nobs <- c(nobs,data$sulfate)
        } 
        if (contaminante == "nitrate"){
            nobs <- c(nobs,data$nitrate)
        } 
    }
    media <- mean(nobs, na.rm = T)
    media
    
}

# Parte 2
completos <- function(directorio = "C:/Users/ELSYMARISOL/Desktop/specdata", id = 1:332) {
    nobs <- numeric()
    for(j in id) {
        casoscom<- read.csv (sprintf("%03d.csv", j)) 
        nobs <-c(nobs, sum(complete.cases(casoscom))) 
        
    }
    dataframe<- data.frame(id, nobs)
    print(dataframe)
}

# Parte 3
corr <- function(directorio ="C:/Users/ELSYMARISOL/Desktop/specdata", horizonte = 0){
    corre <- c()
    cont <- 0
    setwd(directorio)
    
    for (j in 1:332){
        datos <- read.csv(sprintf("%03d.csv", j))
        
    }
    
    x <- data.matrix(datos)           
    com <- x[complete.cases(x),]     
    n <- nrow(com)                    
    if (n>horizonte){               
        corre <- c(corre, cor(com[,2],com[,3]))
    }
    corre
}
