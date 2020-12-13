
# ts.0 <- ts <- 939
# b <- '7,13,x,x,59,x,31,19'
l <- readLines('data/day13.txt', warn = FALSE)
ts.0 <- ts <- as.numeric(l[1])
b <- l[2]
b <- strsplit(b, ',')[[1]]
b <- as.numeric( b[ b != 'x'] )

repeat{
  d <- ts %% b
  if (any(d == 0)){
    print(b[d == 0] * (ts - ts.0))
    break()
  }
  ts <- ts + 1
}

# part two
common_ts <- function(b, id, ts, m){
  
  init <- k <- 0
  repeat{
    d <- (ts + id) %% b
    if (all(d[m] == 0)){
      
      if (init == 0)
        init <- ts
      else
        return(list(init = init, k = ts - init))
      
    }
    ts <- ts + b[1]
  }
}

find_ts <- function(b, m = 1){
  
  b <- strsplit(b, ',')[[1]]
  id <- {w <- which(b != 'x')} - 1
  b <- as.numeric(b[w])

  o <- common_ts(b, id, b[1], m)
  ts <- o$init
  
  repeat{
    d <- (ts + id) %% b
    if (all(d == 0))
      return(ts)
    ts <- ts + o$k
  }
}


find_ts('7,13,x,x,59,x,31,19') == 1068781
find_ts('17,x,13,19') == 3417
find_ts('67,7,59,61') == 754018
find_ts('67,x,7,59,61') == 779210
find_ts('67,7,x,59,61') == 1261476
find_ts('1789,37,47,1889') == 1202161486

options(scipen = 999)

l <- readLines('data/day13.txt', warn = FALSE)
b <- l[2]
print( find_ts(l[2], 1:5) )
#939490236001473

