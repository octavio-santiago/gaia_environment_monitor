#import from C#
#args <- commandArgs()
#riskValueFilepath <- args[2]
#VaR <- args[3]

#import Libs
library("dplyr", lib.loc="~/R/win-library/3.5")
library("twitteR", lib.loc="~/R/win-library/3.5")
library("RColorBrewer", lib.loc="~/R/win-library/3.5")
library("ggplot2", lib.loc="~/R/win-library/3.5")
library("ggraph", lib.loc="~/R/win-library/3.5")
library("igraph", lib.loc="~/R/win-library/3.5")
library("quanteda", lib.loc="~/R/win-library/3.5")
library("stringi", lib.loc="~/R/win-library/3.5")
library("tidytext", lib.loc="~/R/win-library/3.5")
library("widyr", lib.loc="~/R/win-library/3.5")
library("stringr", lib.loc="~/R/win-library/3.5")


#leitura csv de palavras chave
#riskValueFilepath <- "C:/Users/Octavio/Desktop/Hackaton/locais_palavra_chave.csv"
riskValueFilepath <- "~/locais_palavra_chave.csv"

data <- read.csv(riskValueFilepath, header = TRUE)
attach(data)

#Acesso Twitter
# coloque as chaves da API e pegar credenciais
api_key             <- "RLGB0wCo5gvVIxpl7opun5Hdz"
api_secret          <- "XZUaU58OOe2BbLaWMnbrCL34ZkrVDLgaG59ZSVwaNAMcrBWMrk"
access_token        <- "3082158117-B0BqsKnq7MSxpkcHcgs4ZFpdGCGqQlyocfxbPYj"
access_token_secret <- "jev9ifvBMItv4AGuPx9FqZuJcfnl3vakDfS4Dx4sBiumK"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

searchWord <- data$Localidade
#bairros <- data$Bairros
palavrasIgnoradas <- data$PalavrasIgnoradas
lat <- data$Lat
long <- data$Long
#imp <- searchTwitter("Rio de Janeiro", n = 1)

temaFilter <- data$PalavraChave


filteredId <- c()
filteredText <- c()
filteredDate <- c()
filteredRetweet <- c()
temaFilterVec <- c()
locLat <- c()
locLong <- c()

for (y in 1:length(searchWord)){
  if (searchWord[y] == ""){
    searchWord[y] <- "NAnotfound"
  }
}

#Loop localizacao
for (x in 1:length(searchWord)){
  locLat <- c(toString(lat[x]))
  locLong <- c(toString(long[x]))
  imp <- searchTwitter(toString(searchWord[x]), n = 50)
  #loop tweet
  for (i in 1:length(imp)){
    #loop palavra chave
    for (j in 1:length(temaFilter)){
      word <- toString(temaFilter[j])
      #print(word)
      if (toString(temaFilter[j]) == ""){
        print(toString(temaFilter[j]))
      }
      else{
        if (!is.na(str_match(imp[[i]]$text,toString(temaFilter[j])))){
          filteredText <- c(filteredText, imp[[i]]$text)
          filteredDate <- c(filteredDate, toString(imp[[i]]$created))
          filteredId <- c(filteredId, imp[[i]]$id)
          filteredRetweet <- c(filteredRetweet, imp[[i]]$retweetCount)
          temaFilterVec <- c(temaFilterVec, toString(temaFilter[j]))
      }
      
        
        #print(imp[[i]])
      }
    }
    
  }
  
}

dataOutput <- data.frame("ID" = filteredId, "Date" = filteredDate, "Text" = filteredText, "Retweet"=filteredRetweet, "Tema"= temaFilterVec, "Latitude"=locLat, "Longitude"=locLong)

# Write CSV in R
write.table(dataOutput, file = "dataOutput.csv",row.names=FALSE, na="",col.names=TRUE, sep=";")
