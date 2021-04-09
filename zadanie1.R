# zadanie 1
checkIfDivisible <- function(x,y){
  if(x %% y == 0){
    result <- paste("Great, number ", x, " is divisible by ", y, sep=" ") 
  }
  else{
    result <- paste("Sorry, number ", x, " is not divisible by ", y, sep=" ") 
  }
  result
}

#zadanie 2
v1 <- 120
v2 <- 90
s1 <- 0.5
s2 <- 0.5
avg <- (s1+s2)/(s1/v1+s2/v2)


#zadanie 3
rPearson <- function(x,y){
  v1 <- as.vector(x)
  v2 <- as.vector(y)
  if (length(v1) == length(v2)){
    result <- cor(v1,v2)
  }
  else{
    result <- "Vectors are not of same length, please try again"
  }
}

df <- read.csv2("dane.csv")
coeff <- rPearson(df$waga,df$wzrost)
# r = 0.9793459 - pokazuje on wysoką dodatnią zależność pomiędzy wzrostem a wagą

#zadanie 4
stworzDataFrame <- function(ile=1){
  counter <- ile+1
  cat(komunikat <- "Podaj proszę nazwy kolumn oddzielone przecinkiem, a w następnych wierszach dane odzielone przecinkiem: ")
  odp <- strsplit(readLines(con =stdin(), n=counter),",")
  df2 <- as.data.frame(do.call(rbind, odp))
  names(df2) <- lapply(df2[1, ], as.character)
  df2 <- df2[-1,] 
  df2
}
dane <- stworzDataFrame(3)



#zadanie5

sciezka <- "./smogKrakow"
nazwaKolumny <- "3_pressure"

liczZplikow <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",DlaIluPlikow=1){ 
  if(suppressWarnings(!is.na(as.numeric(substr(nazwaKolumny,1,1))))){
    nazwaKolumny <- paste("X", nazwaKolumny, sep="")
  }
  #cat (nazwaKolumny)
  df <- data.frame()
  pliki<- list.files(sciezka)
  #cat (pliki)
  #cat (DlaIluPlikow)
  cat("\n")
  if (DlaIluPlikow <= length(pliki)){
    myseq <- seq(1, DlaIluPlikow, 1)
    
    for (val in myseq){
      #cat (pliki[val])
      ndf <- read.csv(paste(sciezka, pliki[val], sep="/"))
      df <- rbind(df,ndf)
    }
    
    vals <- na.omit(df[[nazwaKolumny]])
    if (toupper(jakaFunkcja)=="MEAN"){
      result <- mean(vals)
    }
    else if(toupper(jakaFunkcja)=="MAX"){
      result <- max(vals)
    }
    else if(toupper(jakaFunkcja)=="MIN"){
      result <- min(vals)
    }
    else if(toupper(jakaFunkcja)=="MEDIAN"){
      result <- median(vals)
    }
    else{
      cat("Function not recognized, please try again")
    }
    result
  }
  else{
    cat("Given number of files is greater than the actual number within directory. Please, try again")
  }
  
  
}
liczZplikow(sciezka,nazwaKolumny,"mean",12)




