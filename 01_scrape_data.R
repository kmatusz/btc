# function definition
scrapData <- function(x){
  
  # function to scrap the OHLC data from 
  # www.coinmarketcap.com
  
  # load packages
  library(XML)
  library(RCurl)
  library(rlist)
  
  # set locale to English
  Sys.setlocale("LC_TIME", "C")
  
  # set url
  theurl <- getURL(
    paste0("https://coinmarketcap.com/currencies/", 
           x, 
           "/historical-data/?start=20130428&end=21000101"),
    .opts = list(ssl.verifypeer = FALSE))
  
  # read html source
  tables <- readHTMLTable(theurl, stringsAsFactors = FALSE)
  
  # clean the list object
  tables <- list.clean(tables, fun = is.null, recursive = FALSE)
  
  # unlist the list
  table <- tables[[3]]
  
  # rename variables
  table$Open <- table$`Open*`
  table$`Open*` <- NULL
  table$Close <- table$`Close**`
  table$`Close*` <- NULL
  
  # convert characters to numericals
  table$Open      <- as.numeric(gsub(",", "", table$Open))
  table$High      <- as.numeric(gsub(",", "", table$High))
  table$Low       <- as.numeric(gsub(",", "", table$Low))
  table$Close     <- as.numeric(gsub(",", "", table$Close))
  table$Volume    <- as.numeric(gsub(",", "", table$Volume))
  table$MarketCap <- as.numeric(gsub(",", "", table[, "Market Cap"]))
  table[, "Market Cap"] <- NULL
  
  # convert the date from chr into Date
  table$Date <- as.Date(table$Date, format = "%b %d, %Y")
  
  # sort the data  
  table <- table[order(as.numeric(table$Date)), ]
  table <- table[, c("Date", "Open", "High", "Low", "Close", 
                     "Volume",  "MarketCap")]
  rownames(table) <- 1:nrow(table)
  
  return(table)
}

# function call
library(tidyverse)

btc <- scrapData("bitcoin")
summary(btc)
btc %>% as.tibble %>% tail

eth <- scrapData("ethereum")
summary(eth)
eth %>% as.tibble %>% tail

xrp <- scrapData("xrp")
summary(xrp)
xrp %>% as.tibble %>% tail

bch <- scrapData("bitcoin-cash")
summary(bch)
bch %>% as.tibble %>% tail

eos <- scrapData("eos")
summary(eos)
eos %>% as.tibble %>% tail