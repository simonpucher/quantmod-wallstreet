# read more: http://amunategui.github.io/wallstreet/

# delete workspace
remove(list = ls())
# clear console window in rstudio
cat("\014")

# install all required libraries
if (!require(quantmod)) install.packages('quantmod')
library(quantmod)
if (!require(xts)) install.packages('xts')
library(xts)
if (!require(xgboost)) install.packages('xgboost')
library(xgboost)
if (!require(pROC)) install.packages('pROC')
library(pROC)

# display a simple bar chart
getSymbols(c("AMZN"))
barChart(AMZN,theme='white.mono',bar.type='hlc')

# display a complex chart
getSymbols(c("^GSPC"))
chartSeries(GSPC, subset='last 3 months')
addBBands(n = 20, sd = 2, ma = "SMA", draw = 'bands', on = -1)



# download all data
url = "http://www.nasdaq.com/quotes/nasdaq-100-stocks.aspx?render=download"
nasdaq100 = read.csv(file=url)
#y = paste( unlist(x[1]), collapse=',')
#z = strsplit(y, ",")
#symbolius <- c(z[1])
#getSymbols(c(z[1]))

for(i in nasdaq100$Symbol[startsWith(nasdaq100$Symbol, "A")]) {
 cat("Downloading time series for symbol '", i, "' ...\n", sep = "")
 status <- tryCatch(getSymbols(i, env = nasdaq, src = "google", from = as.Date("2000-01-01")), error = identity)
 if(inherits(status, "error"))
 cat("Symbol '", i, "' not downloadable!\n", sep = "")
 }

 for(i in nasdaq100$Symbol) {
 cat("Downloading ", i)
getSymbols(c(i))
  }



# get all symbols: https://www.nasdaq.com/screening/company-list.aspx
# get market data for all symbols making up the Nasdaq 100 index
Nasdaq100_Symbols <- c("AAPL", "ADBE", "ADI", "ADP", "ADSK", "AKAM", "ALXN",
"AMAT", "AMGN", "AMZN", "ATVI", "AVGO", "BBBY", "BIDU", "BIIB",
"CA", "CELG", "CERN", "CHKP", "CHRW", "CHTR", "CMCSA",
"COST", "CSCO", "CTRX", "CTSH", "CTXS", "DISCA", "DISCK", "DISH",
"DLTR", "EBAY", "EQIX", "ESRX", "EXPD", "EXPE", "FAST",
"FB", "FFIV", "FISV", "FOXA", "GILD", "GMCR", "GOOG", "GOOGL",
"GRMN", "HSIC", "ILMN", "INTC", "INTU", "ISRG", "KLAC", "KHC",
"LBTYA", "MAR", "MAT", "MDLZ",
"MNST", "MSFT", "MU", "MXIM", "MYL", "NFLX", "NTAP", "NVDA",
"NXPI", "ORLY", "PAYX", "PCAR", "PCLN", "QCOM", "QVCA", "REGN",
"ROST", "SBAC", "SBUX", "SIAL", "SIRI", "SPLS", "SRCL",
"STX", "SYMC", "TRIP", "TSCO", "TSLA", "TXN", "VIAB",
"VOD", "VRSK", "VRTX", "WDC", "WYNN", "XLNX")
getSymbols(Nasdaq100_Symbols)

# merge them all together
nasdaq100 <- data.frame(as.xts(merge(AAPL, ADBE, ADI, ADP, ADSK, AKAM,
                 ALXN, AMAT, AMGN, AMZN, ATVI, AVGO, BBBY, BIDU, BIIB,
               CA, CELG, CERN, CHKP, CHRW, CHTR, CMCSA,
                 COST, CSCO, CTRX, CTSH, CTXS, DISCA, DISCK, DISH,
                 DLTR, EBAY, EQIX, ESRX, EXPD, EXPE, FAST,
                 FB, FFIV, FISV, FOXA, GILD, GMCR, GOOG, GOOGL,
                 GRMN, HSIC, ILMN, INTC, INTU, ISRG, KLAC, KHC,
                 LBTYA, MAR, MAT, MDLZ,
                 MNST, MSFT, MU, MXIM, MYL, NFLX, NTAP, NVDA,
                 NXPI, ORLY, PAYX, PCAR, PCLN, QCOM, QVCA, REGN,
                 ROST, SBAC, SBUX, SIAL, SIRI, SPLS, SRCL,
                 STX, SYMC, TRIP, TSCO, TSLA, TXN, VIAB,
                 VOD, VRSK, VRTX, WDC, WYNN, XLNX)))
head(nasdaq100[,1:12],2)

# set outcome variable
outcomeSymbol <- 'FISV.Volume'

# shift outcome value to be on same line as predictors
nasdaq100 <- xts(nasdaq100,order.by=as.Date(rownames(nasdaq100)))
nasdaq100 <- as.data.frame(merge(nasdaq100, lm1=lag(nasdaq100[,outcomeSymbol],-1)))
nasdaq100$outcome <- ifelse(nasdaq100[,paste0(outcomeSymbol,'.1')] > nasdaq100[,outcomeSymbol], 1, 0)

# remove shifted down volume field as we don't care by the value
nasdaq100 <- nasdaq100[,!names(nasdaq100) %in% c(paste0(outcomeSymbol,'.1'))]

# cast date to true date and order in decreasing order
nasdaq100$date <- as.Date(row.names(nasdaq100))
nasdaq100 <- nasdaq100[order(as.Date(nasdaq100$date, "%m/%d/%Y"), decreasing = TRUE),]

# calculate all day differences and populate them on same row
GetDiffDays <- function(objDF,days=c(10), offLimitsSymbols=c('outcome'), roundByScaler=3) {
        # needs to be sorted by date in decreasing order
        ind <- sapply(objDF, is.numeric)
        for (sym in names(objDF)[ind]) {
                if (!sym %in% offLimitsSymbols) {
                        print(paste('*********', sym))
                        objDF[,sym] <- round(scale(objDF[,sym]),roundByScaler)

                        print(paste('theColName', sym))
                        for (day in days) {
                                objDF[paste0(sym,'_',day)] <- c(diff(objDF[,sym],lag = day),rep(x=0,day)) * -1
                        }
                }
        }
        return (objDF)
}

# call the function with the following differences
nasdaq100 <- GetDiffDays(nasdaq100, days=c(1,2,3,4,5,10,20), offLimitsSymbols=c('outcome'), roundByScaler=2)

# drop most recent entry as we don't have an outcome
nasdaq100 <- nasdaq100[2:nrow(nasdaq100),]

# take a peek at YHOO features:
# dput(names(nasdaq100)[grepl('YHOO.',names(nasdaq100))])
dput(names(nasdaq100)[grepl('AAPL.',names(nasdaq100))])

# well use POSIXlt to add day of the week, day of the month, day of the year
nasdaq100$wday <- as.POSIXlt(nasdaq100$date)$wday
nasdaq100$yday <- as.POSIXlt(nasdaq100$date)$mday
nasdaq100$mon<- as.POSIXlt(nasdaq100$date)$mon

# remove date field and shuffle data frame
nasdaq100 <- subset(nasdaq100, select=-c(date))
nasdaq100 <- nasdaq100[sample(nrow(nasdaq100)),]

# let's model xgboost
predictorNames <- names(nasdaq100)[names(nasdaq100) != 'outcome']

set.seed(1234)
split <- sample(nrow(nasdaq100), floor(0.7*nrow(nasdaq100)))
train <-nasdaq100[split,]
test <- nasdaq100[-split,]

bst <- xgboost(data = as.matrix(train[,predictorNames]),
               label = train$outcome,
               verbose=0,
               eta = 0.1,
               gamma = 50,
               nround = 50,
               colsample_bytree = 0.1,
               subsample = 0.6,
               objective="binary:logistic")

predictions <- predict(bst, as.matrix(test[,predictorNames]), outputmargin=TRUE)

# pROC
auc <- roc(test$outcome, predictions)
print(paste('AUC score:', auc$auc))
