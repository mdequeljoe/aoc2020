

.set <- function(v){
  v <- strsplit(v, '')[[1]]
  v[v != ' ']
}

.solve <- function(v){
  res <- character()
  i <- 1
  .op <- list('+' = `+`, '*' = `*`)
  repeat{
    if (v[i] %in% c('+', '*')){
      res[length(res)] <- 
        .op[[ v[i] ]](as.numeric(res[length(res)]), 
                      as.numeric(v[i + 1]))
      i <- i + 2
    } else {
      res[length(res) + 1] <- v[i]
      i <- i + 1
    }
    if (i > length(v))
      break()
  }
  as.numeric(res)
}

.f <- function(v){
  v <- .set(v)
  res <- list()
  k <- character()
  for (i in seq_along(v)){
    
    if (v[i] == "("){
      res[[length(res) + 1]] <- k
      k <- character()
      next()
    }
    if (v[i] == ")"){
      s <- .solve(k)
      k <- c(res[[length(res)]], s)
      res <- res[-length(res)]
      next()
    }
    k[length(k) + 1] <- v[i]
  }
  .solve(k)
}

.f('1 + 2 * 3 + 4 * 5 + 6') == 71
.f('1 + (2 * 3) + (4 * (5 + 6))') == 51
.f('2 * 3 + (4 * 5)') == 26
.f('5 + (8 * 3 + 9 + 3 * 4 * 3)') == 437
.f('5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))') == 12240
.f('((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2') == 13632

l <- readLines('data/day18.txt', warn = FALSE)
print( sum( vapply(l, .f, numeric(1)) ) )
#69490582260

# part two
.solve <- function(v){
  res <- character()
  i <- 1
  repeat{
    if (v[i] == '+'){
      res[length(res)] <- 
        as.numeric(res[length(res)]) + 
        as.numeric(v[i + 1])
      i <- i + 2
    } else {
      res[length(res) + 1] <- v[i]
      i <- i + 1
    }
    if (i > length(v))
      break()
  }
  Reduce(`*`, as.numeric(res[res != '*']))
}

.f('1 + (2 * 3) + (4 * (5 + 6))') == 51
.f('1 + 2 * 3 + 4 * 5 + 6') == 231
.f('1 + (2 * 3) + (4 * (5 + 6))') == 51
.f('2 * 3 + (4 * 5)') == 46
.f('5 + (8 * 3 + 9 + 3 * 4 * 3)') == 1445
.f('5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))') == 669060
.f('((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2') == 23340

options(scipen = 999)
l <- readLines('data/day18.txt', warn = FALSE)
print( sum( vapply(l, .f, numeric(1)) ) )
#362464596624526
