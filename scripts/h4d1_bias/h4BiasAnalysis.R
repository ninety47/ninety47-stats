# ----------------------------------------------------------------------
#
# This library and source code are released under the GNU General Public
# License v3.
# 
# A copy of the license can be found in the my GitHub repository here
# https://github.com/ninety47/ninety47-stats/blob/master/LICENSE
# 
# Trading is a risky business and in using this code you accept ALL
# responsibility for ensuring it works as intended. Simply put I provide
# this code as-is with no warranty or guarantee that it works or gives
# accurate, logical or useful results.
#
# I am  not liable for any losses you incur through incorrect logic, bugs 
# in the software, bugs in the R platform itself or implemented in this 
# script/library or ANY of the scripts/libraries used within.
# 
# All that said, feel free to chip me a couple of bones if you
# appreciate work I've done here and it helps you turn a profit.
#
# ----------------------------------------------------------------------
# 
# Copyright (C) 2012 Michael O'Keeffe (a.k.a. ninety47)
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/gpl.txt>.
#
# ----------------------------------------------------------------------

loadMT4Export <- function(filename) {
  data <- read.csv(filename, header=F, stringsAsFactors=F)
  names(data) <- c('date', 'time', 'open', 'high', 'low', 'close', 'volume')
  data
}


h4BiasAnlaysis <- function(bars, symbol) {  
  result <- t(sapply(split(bars, bars$date), function(D1) {
      if ( nrow(D1) != 6 ) {
        row <- rep(NA, 5)
      } else {
        row <- c(D1$open[1], rev(range(D1[, c('high', 'low')])), D1$close[6], D1$close[1] < D1$open[1])
      }
      names(row) <- c('open', 'high', 'low', 'close', 'h4.bias')
      row
  }))
  result <- cbind(result, d1.bias=result[, 'open'] < result[, 'close'])
  result <- cbind(result, occurred=result[, 'h4.bias'] + result[, 'd1.bias'])
  dates <- as.Date(rownames(result), "%Y.%m.%d")  
  days <- ordered(weekdays(dates), c('Monday', 'Tuesday', 'Wednesday','Thursday', 'Friday'))
  result <- cbind(date=dates, day=I(days), as.data.frame(result))
  
  if (missing(symbol)) symbol <- "(Symbol not supplied)"
  attr(result, 'symbol') <- symbol
  
  class(result) <- c('h4BiasAnalysis', 'data.frame')
  result
}

summary.h4BiasAnalysis <- function(result) {

  overallTable <- table(result$occurred)
  dimnames <- attr(overallTable, "dimnames")
  names(dimnames) <- "Bias"
  dimnames$Bias <- c("Bearish", "None", "Bullish")
  attr(overallTable, "dimnames") <- dimnames

  dailyTable <- table(result[, c('day', 'occurred')])
  dimnames <- attr(dailyTable, 'dimnames')
  names(dimnames) <- c("Day", "Bias")
  dimnames$Bias <- c("Bearish", "None", "Bullish")
  attr(dailyTable, 'dimnames') <- dimnames
  
  object <- list(
      nObs=nrow(result),
      nValidObs= nrow(result) - sum(is.na(result$close)),
      symbol=attr(result, "symbol"),
      overall=overallTable,
      daily=dailyTable
  )
  class(object) <- 'summary.h4BiasAnalysis'
  object
}

print.summary.h4BiasAnalysis <- function(x) {
  cat(x$symbol, "H4 to D1 bias\n")
  cat(rep("=", nchar(x$symbol)+1), "=============\n", sep="")
  cat("Overall:\n")
  cat("Total observations:    ", x$nObs, '\n')
  cat("Invalid D1 bars (NAs): ", x$nObs - x$nValidObs, '\n')
  cat("Successful bias bars:  ", (x$overall['Bearish'] + x$overall['Bullish']), "(", sprintf("%3.2f%%", 100*(x$overall['Bearish'] + x$overall['Bullish'])/x$nValidObs), ")\n")
  cat("Failed bias bars:      ", x$overall['None'], "(", sprintf("%3.2f%%", 100*x$overall['None']/x$nValidObs), ")\n")
  cat("\nBreak down by day of the week (as % or number of days e.g. Mondays): \n")
  x$daily <- 100*x$daily/apply(x$daily, 1, function(x) sum(x))
  digits <- options()$digits
  options(digits=3)  
  print(x$daily)
  options(digits=digits)
}


plot.h4BiasAnalysis <- function(x, ...) {
  tab <- table(x[, c('day', 'occurred')])
  tab <- tab/apply(tab, 1, function(x) sum(x))
  dimnames <- attr(tab, 'dimnames')
  names(dimnames) <- c("Day", "Bias")
  dimnames$Bias <- c("Bearish", "None", "Bullish")
  attr(tab, 'dimnames') <- dimnames       
  plot(tab, xlab="Day of the week", 
       main=paste(attr(x, 'symbol'), "Bullish/Bearish H4-D1 bias breakdown"), ...)  
  text(x=seq(.2,1,.2)-.18, y=-0.02, labels=sprintf("%2.2f%%", 100.0*tab[,'Bullish']), cex=0.75, c(0,0))
  text(x=seq(.2,1,.2)-.18, y=tab[,'Bullish'], labels=sprintf("%2.2f%%", 100.0*tab[,'None']), cex=0.75, c(0,0))
  text(x=seq(.2,1,.2)-.18, y=apply(tab[,c('None', 'Bullish')], 1, sum), 
        labels=sprintf("%2.2f%%", 100.0*tab[,'Bearish']), cex=0.75, c(0,0))
}

#
# Assumption:
# - Input filename structure is <SYMBOL>240.csv (as exported from the MT4 history centre).
#
# Instructions:
# - Export data from your MT4 history centre and save in  <location>
# - Set "data.dir" to <location>
# - Set the "output.dir" variable to the location you want save the results.
#
# Method:
# - The H4 days are split into days
# - For each day the 
#   - 1st H4 bar (H4.1) is classified as bullish or bearish.
#   - The daily bar (D1) is classified as bullush or bearish.
# - The H4 to D1 bias exists if both H4.1 and D1 are bearish or bullish.
# - Otherwise the bias is classified as "None".
#
# Table of outcomes:
# +---------+-------------------+
# |         |  D1               |
# |         +---------+---------+
# | H4.1    | Bearish | Bullish |
# +---------+---------+---------+
# | Bearish | Bearish | None    |
# +---------+---------+---------+
# | Bullish | None    | Bullish |
# +---------+---------+---------+
#

data.dir <- "/data/ninety47/data/fx/"
output.dir <- "~/tmp/d1h4_bias/"
timeStamp <- format(Sys.time(), "%Y%m%dT%H%M")
image.width <- 866
image.height <- 512
errorHandler <- function() {
  if (names(dev.cur()) != "null device") dev.off()  
}
cat("Results will be logged to:", paste(output.dir, "/result_", timeStamp, ".txt", sep=""), "\n")
con <- file(paste(output.dir, "/result_", timeStamp, ".txt", sep=""))
sink(con, append=TRUE)
tmp <- sapply(
  list.files(data.dir, "*.240\\.csv", full.names=T),
  function(h4.file) {
    on.exit(errorHandler())
    symbol <- gsub(".*\\/(.*)240\\.csv", "\\1", h4.file)    
    bars <- loadMT4Export(h4.file)
    result <- h4BiasAnlaysis(bars, symbol)
    print(summary(result))
    cat("\n\n")
    png(paste(output.dir, "h4d1bias_", symbol, "_", timeStamp, ".png", sep=""), width=image.width, height=image.height, units="px")
    plot(result)
    dev.off()
  })
sink(type="message")
sink()
flush(con)
close(con)
