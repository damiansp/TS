#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R/frapo')

library(FRAPO)
data(StockIndexAdj)

# 4. Classes, Methods, and Functions
# Create a class
setClass('PortWgt', representation(Weights='numeric', 
                                   Name='character', 
                                   Date='character', 
                                   Leveraged='logical', 
                                   LongOnly='logical'))
showClass('PortWgt')
P1 <- new('PortWgt', 
          Weights=rep(0.2, 5), 
          Name='Equal Weighted', 
          Date='2001-03-31', 
          LongOnly=T, 
          Leveraged=F)

# Or use a constructor function
PortWgt <- function(Weights, Name, Date=NULL, LongOnly=T, Leveraged=F) {
  Weights <- as.numeric(Weights)
  Name <- as.character(Name)
  if (is.null(Date)) { Date <- as.character(Sys.Date()) }
  ans <- new('PortWgt', 
             Weights=Weights, 
             Name=Name, 
             Date=Date, 
             LongOnly=LongOnly, 
             Leveraged=Leveraged)
  ans
}

P2 <- PortWgt(Weights=rep(0.2, 5), Name='Equal Weighted')


validPortWgt <- function(object) {
  if (object@LongOnly) {
  	if (any(object@Weights < 0)) { return ('\nNegative weights for long-only') }
  }
  if (!object@Leveraged) {
  	if (sum(abs(object@Weights)) > 1) { 
  		return ('\nAbsolute sum of weights > 1') 
  	}
  }
  T
}

setValidity('PortWgt', validPortWgt)

PortWgt(Weights=rep(-0.2, 5), Name='Equal Weighted', LongOnly=T)
PortWgt(Weights=rep(0.3, 5), Name='Equal Weighted', Leveraged=F)


setMethod('show', signature='PortWgt', function(object) {
  if (is.null(names(object@Weights))) { 
  	N <- length(object@Weights) 
    names(object@Weights)	 <- paste('Asset', 1:N)
  }
  cat('Portfolio: ', object@Name, 
      '\n Long-Only: ', object@LongOnly, 
      '\n Leveraged: ', object@Leveraged, 
      '\n Weights:\n', sep='')
  print(object@Weights)
  cat('\n')
})

P2


setMethod('summary', 'PortWgt', function(object, ...) {
  summary(object@Weights, ...)
})

summary(P2)


setMethod('length', 'PortWgt', function(x) { length(x@Weights) })
length(P2)


setGeneric('weights', function(object) { standardGeneric('weights') })


setMethod('weights', 'PortWgt', function(object) { object@Weights })
weights(P2)

setGeneric('weights<-', function(x, ..., value) { 
  standardGeneric('weights<-')
}) 

setReplaceMethod('weights', 'PortWgt', function(x, ..., value) {
  x@Weights <- value
  x
})

weights(P2) <- rep(0.25, 4)
P2

setAs(from='PortWgt', to='data.frame', function(from){
  anames <- names(from@Weights)
  if (is.null(anames)) {
    N <- length(from)
    anames <- paste('Asset', 1:N)
  }
  ans <- data.frame(from@Date, t(weights(from)))
  colnames(ans) <- c('Date', anames)
  ans
})

as(P2, 'data.frame')


# As a reference class (RC)
PortWgtRC <- setRefClass('PortWgtRC', fields=list(Weights='numeric',
                                                  Name='character',
                                                  Date='character',
                                                  Leveraged='logical', 
                                                  LongOnly='logical'))
P3 <- PortWgtRC$new()
P3

P3$Weights <- rep(0.2, 5)
P3$Name <- 'Equal Weighted'
P3$Date <- '2001-03-31'
P3$LongOnly <- T
P3$Leveraged <- F

P4RC <- P3
P4RC$LongOnly <- F
P3$LongOnly

P4S4 <- P1
P4S4@LongOnly <- F
P1@LongOnly

P3$LongOnly <- T
PortWgtRC$methods()
P4RC <- P3$copy()
P4RC$LongOnly <- F
P3$LongOnly


PortWgtRC$methods(show=function() {
  if (is.null(names(Weights))) {
    N <- length(Weights)
    names(Weights) <<- paste('Asset', 1:N) # <<- non-local assignment
  }
  cat('Portfolio: ', Name, 
      '\n Long-Only: ', LongOnly, 
      '\n Leveraged: ', Leveraged, 
      '\n Weights:\n', sep='')
  print(Weights)
  cat('\n')
})

P4 <- PortWgtRC$new(Weights=rep(0.2, 5), 
                    Name='Equal Weigthed', 
                    Date='2001-03-31', 
                    LongOnly=T, 
                    Leveraged=F)
P4



# 5. The Accompanying Package FRAPO
R <- returnseries(StockIndexAdj, method='discrete', trim=T)
head(R) # returns
P <- PGMV(R, optctrl=ctrl(trace=F))
str(P)
showClass('PortSol')
showMethods(classes='PortSol', inherited=F)
P

showMethods('Weights', inherited=F)
selectMethod(f='Weights', signature='PortSol')
Weights(P)
slot(P, 'weights')
P@weights
