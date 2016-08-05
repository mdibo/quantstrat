.blotter <- new.env()
.strategy <- new.env()
ls(.blotter) # .blotter holds the portfolio and account object
ls(.strategy) # .strategy holds the orderbook and strategy object


# 1) Load Quantstrat library

library(quantstrat)
library(tseries)
library(IKTrading)
library(PerformanceAnalytics)

# 2) Initialize currency

currency('USD')

# 3) Initialize dates and initial equity

initDate <- '2008-12-31'
startDate <- '2009-01-01'
endDate <- '2012-12-31'

initEq <- 100000
N = 20
N.ADF = 60
alpha = 1
buyThresh = -2
sellThresh = -buyThresh
exitlong = 1
exitshort = 1

Sys.setenv(TZ = 'UTC')

# 4) fetch market data

symb1 <- 'C'
symb2 <- 'BAC'
getSymbols(symb1, from=startDate, to=endDate, adjust=TRUE) 
getSymbols(symb2, from=startDate, to=endDate, adjust=TRUE) 

spread <- OHLC(C)-OHLC(BAC)
colnames(spread)<-c("open","high","low","close") 

head(spread)

symbols <- c("spread") 
stock(symbols, currency = 'USD', multiplier = 1)

chart_Series(spread)
add_TA(EMA(Cl(spread), n=20), on=1, col="blue", lwd=1.5)
legend(x=5, y=50, legend=c("EMA 20"),
       fill=c("blue"), bty="n")

# 5) Inititalize strategy, portfolio, account and orders

qs.strategy <- 'pairStrat'

initPortf(qs.strategy, symbols = symbols, initDate=initDate)
initAcct(qs.strategy, portfolios=qs.strategy, initDate=initDate, initEq=initEq)
initOrders(qs.strategy,initDate=initDate)



# 6) Save strategy

strategy(qs.strategy, store = TRUE)
# rm.strat(pairStrat) # only when trying a new test
ls(.blotter) # .blotter holds the portfolio and account object
ls(.strategy) # .strategy holds the orderbook and strategy object


# 7) Add indicators
# a) Z-Score

PairRatio <- function(x) { #returns the ratio of close prices for 2 symbols
  x1 <- get(x[1])
  x2 <- get(x[2])
  rat <- log10(Cl(x1) / Cl(x2))
  colnames(rat) <- 'Price.Ratio'
  rat
} 

Price.Ratio <- PairRatio(c(symb1[1],symb2[1]))


MaRatio <- function(x){
  
  Mavg <- rollapply(x, N , mean)
  colnames(Mavg) <- 'Price.Ratio.MA'
  Mavg
}

Price.Ratio.MA <- MaRatio(Price.Ratio)


Sd <- function(x){
  
  Stand.dev <- rollapply(x, N, sd)
  colnames(Stand.dev) <- "Price.Ratio.SD"
  Stand.dev
}

Price.Ratio.SD <- Sd(Price.Ratio)


ZScore <- function(x){
  
  a1 <- x$Price.Ratio
  b1 <- x$Price.Ratio.MA
  c1 <- x$Price.Ratio.SD
  
  z <- (a1-b1)/c1
  
  colnames(z)<- 'Z.Score'
  z
  
}

Z.Score <- ZScore(x=merge(Price.Ratio,Price.Ratio.MA,Price.Ratio.SD))
dev.new()
plot(main = "Z-Score Time Series", xlab = "Date" , ylab = "Z-Score",Z.Score, type = "l" )
abline(h = 2, col = 2, lwd = 3 ,lty = 2)
abline(h = -2, col = 3, lwd = 3 ,lty = 2)

# b) Augmented Dickey Fuller


ft2<-function(x){
  adf.test(x)$p.value
}

Pval <- function(x){

  Augmented.df <- rollapply(x, width = N.ADF, ft2)
  colnames(Augmented.df) <- "P.Value"
  Augmented.df
}

P.Value <- Pval(Price.Ratio)


add.indicator(strategy = qs.strategy, name = "ZScore", arguments = 
                list(x=merge(Price.Ratio,Price.Ratio.MA,Price.Ratio.SD)))

add.indicator(strategy = qs.strategy, name = "Pval", arguments = 
                list(x=quote(Price.Ratio)))

summary(get.strategy(qs.strategy))


# 8) Add signals

add.signal(qs.strategy, name="sigThreshold",arguments=list(column="Z.Score", threshold=buyThresh, 
         relationship="lt", cross=FALSE),label="longEntryZ")

add.signal(qs.strategy, name="sigThreshold",arguments=list(column="P.Value", threshold= alpha, 
          relationship="lt", cross=FALSE),label="PEntry")

add.signal(qs.strategy, name="sigAND",
           arguments=list(columns=c("longEntryZ", "PEntry"), cross=FALSE),
           label="longEntry")

add.signal(qs.strategy, name="sigThreshold",arguments=list(column="Z.Score", threshold= exitlong,
         relationship="gt", cross=FALSE),label="longExit")

add.signal(qs.strategy, name="sigThreshold",arguments=list(column="Z.Score", threshold=sellThresh,
         relationship="gt", cross=FALSE),label="shortEntryZ")


add.signal(qs.strategy, name="sigAND", arguments=list(columns=c("shortEntryZ", "PEntry"), cross=FALSE),
           label="shortEntry")

add.signal(qs.strategy, name="sigThreshold",arguments=list(column="Z.Score", threshold= exitshort,
         relationship="lt", cross=FALSE),label="shortExit")

summary(get.strategy(qs.strategy))




addPosLimit( portfolio = qs.strategy, # add position limit rules
             symbol = 'spread',
             timestamp = initDate,
             maxpos = 3000,
             longlevels = 1,
             minpos = -3000)

add.rule(qs.strategy, name='ruleSignal',arguments = list(sigcol="longEntry", 
        sigval=TRUE, orderqty=3000,  osFUN = osMaxPos, replace = FALSE, ordertype='market', 
        orderside='long', prefer = "open"), type='enter' ) 

add.rule(qs.strategy, name='ruleSignal', arguments = list(sigcol="shortEntry",
        sigval=TRUE, orderqty=-3000,  osFUN = osMaxPos, replace = FALSE,ordertype='market',
        orderside='short', prefer = "open"), type='enter')

add.rule(qs.strategy, name='ruleSignal', arguments = list(sigcol="longExit",
        sigval=TRUE, orderqty= 'all', ordertype='market', orderside='short', prefer = "open"), type='exit') 

add.rule(qs.strategy, name='ruleSignal', arguments = list(sigcol="shortExit", 
        sigval=TRUE, orderqty= 'all' , ordertype='market', orderside='long', prefer = "open"), type='exit')

summary(get.strategy(qs.strategy))


# 10) Apply strategy 

applyStrategy(strategy = qs.strategy, portfolios = qs.strategy, mktdata = spread)

tns <-getTxns(Portfolio=qs.strategy, Symbol= symbols)

# 11) Update portfolio, account, equity

updatePortf(qs.strategy)
#dateRange <- time(getPortfolio(qs.strategy)$summary)[-1]
updateAcct(qs.strategy)
updateEndEq(qs.strategy)  




# 12) Plot the results

chart.P2 = function (Portfolio, Symbol, Dates = NULL, ..., TA = NULL) 
{
  pname <- Portfolio
  Portfolio <- getPortfolio(pname)
  if (missing(Symbol)) 
    Symbol <- ls(Portfolio$symbols)[[1]]
  else Symbol <- Symbol[1]
  Prices = get(Symbol)
  if (!is.OHLC(Prices)) {
    if (hasArg(prefer)) 
      prefer = eval(match.call(expand.dots = TRUE)$prefer)
    else prefer = NULL
    Prices = getPrice(Prices, prefer = prefer)
  }
  freq = periodicity(Prices)
  switch(freq$scale, seconds = {
    mult = 1
  }, minute = {
    mult = 60
  }, hourly = {
    mult = 3600
  }, daily = {
    mult = 86400
  }, {
    mult = 86400
  })
  if (!isTRUE(freq$frequency * mult == round(freq$frequency, 
                                             0) * mult)) {
    n = round((freq$frequency/mult), 0) * mult
  }
  else {
    n = mult
  }
  tzero = xts(0, order.by = index(Prices[1, ]))
  if (is.null(Dates)) 
    Dates <- paste(first(index(Prices)), last(index(Prices)), 
                   sep = "::")
  Portfolio$symbols[[Symbol]]$txn <- Portfolio$symbols[[Symbol]]$txn[Dates]
  Portfolio$symbols[[Symbol]]$posPL <- Portfolio$symbols[[Symbol]]$posPL[Dates]
  Trades = Portfolio$symbols[[Symbol]]$txn$Txn.Qty
  Buys = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades > 
                                                           0)]
  Sells = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades < 
                                                            0)]
  Position = Portfolio$symbols[[Symbol]]$txn$Pos.Qty
  if (nrow(Position) < 1) 
    stop("no transactions/positions to chart")
  if (as.POSIXct(first(index(Prices))) < as.POSIXct(first(index(Position)))) 
    Position <- rbind(xts(0, order.by = first(index(Prices) - 
                                                1)), Position)
  Positionfill = na.locf(merge(Position, index(Prices)))
  CumPL = cumsum(Portfolio$symbols[[Symbol]]$posPL$Net.Trading.PL)
  if (length(CumPL) > 1) 
    CumPL = na.omit(na.locf(merge(CumPL, index(Prices))))
  else CumPL = NULL
  if (!is.null(CumPL)) {
    CumMax <- cummax(CumPL)
    Drawdown <- -(CumMax - CumPL)
    Drawdown <- rbind(xts(-max(CumPL), order.by = first(index(Drawdown) - 
                                                          1)), Drawdown)
  }
  else {
    Drawdown <- NULL
  }
  if (!is.null(Dates)) 
    Prices = Prices[Dates]
  chart_Series(Prices, name = Symbol, TA = TA, ...)
  if (!is.null(nrow(Buys)) && nrow(Buys) >= 1) 
    (add_TA(Buys, pch = 2, type = "p", col = "green", on = 1))
  if (!is.null(nrow(Sells)) && nrow(Sells) >= 1) 
    (add_TA(Sells, pch = 6, type = "p", col = "red", on = 1))
  if (nrow(Position) >= 1) {
    (add_TA(Positionfill, type = "h", col = "blue", lwd = 2))
    (add_TA(Position, type = "p", col = "orange", lwd = 2, 
            on = 2))
  }
  if (!is.null(CumPL)) 
    (add_TA(CumPL, col = "darkgreen", lwd = 2))
  if (!is.null(Drawdown)) 
    (add_TA(Drawdown, col = "darkred", lwd = 2, yaxis = c(0, 
                                                          -max(CumMax))))
  plot(current.chob())
}

dev.new()
chart.P2(qs.strategy, "spread", prefer = "close")

returns <- PortfReturns(qs.strategy)

dev.new()
charts.PerformanceSummary(returns, geometric=FALSE, wealth.index=TRUE, main = "Pair Strategy Returns")

# 13) Get statistics

tStats <- tradeStats(qs.strategy, use="trades", inclZeroDays=FALSE)

tStats[,4:ncol(tStats)] <- round(tStats[,4:ncol(tStats)], 4)

tStats <- print(data.frame(t(tStats[,-c(1,2)])))


# 14) Get the order book

orderBook <- getOrderBook(qs.strategy)

