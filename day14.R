

bit_vec <- function(n)
  c(0, 0, 0, 0, as.integer(rev(intToBits(n))))

# from stack overflow
# https://stackoverflow.com/questions/25411380/convert-binary-vector-to-decimal
as_int <- function(b)
  Reduce(function(x, y)
    x * 2 + y, as.integer(b))

apply_mask <- function(mask, n) {
  b <- bit_vec(n)
  b <- as.character(b)
  mask <- strsplit(mask, '')[[1]]
  for (i in seq_along(mask)) {
    if (mask[i] == 'X')
      next()
    b[i] <- mask[i]
  }
  as_int(b)
}

.f <- function(p) {
  res <- list()
  i <- 0
  rx <- 'mem\\[([0-9]+)\\]\\s=\\s([0-9]+)'
  for (l in p) {
    if (grepl('mask', l)) {
      mask <- gsub('mask = ', '', l)
      next()
    }
    address <- as.numeric(gsub(rx, '\\1', l))
    val <- as.numeric(gsub(rx, '\\2', l))
    res[[address]] <- apply_mask(mask, val)
  }
  res
}

# p <- "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
# mem[8] = 11
# mem[7] = 101
# mem[8] = 0"
# p <- strsplit(p, '\n')[[1]]
# res <- .f(p)
# sum(unlist(res))

options(scipen = 999)
p <- readLines('data/day14.txt', warn = FALSE)
res <- .f(p)
print( sum(unlist(res)) )

# part two

float <- function(n) {
  l <- lapply(1:n, function(x)
    0:1)
  expand.grid(l)
}

apply_mask <- function(mask, n) {
  b <- bit_vec(n)
  b <- as.character(b)
  mask <- strsplit(mask, '')[[1]]
  for (i in seq_along(mask))
    if (mask[i] != 0)
      b[i] <- mask[i]
  
  g <- float(length(b[b == 'X']))
  lapply(1:nrow(g), function(i, v) {
    v[v == "X"] <- g[i,]
    as_int(v)
  }, v = b)
  
}
# 
# apply_mask('000000000000000000000000000000X1001X', 42) 
# apply_mask('00000000000000000000000000000000X0XX', 26) 

.f <- function(p) {
  res <- list()
  i <- 0
  rx <- 'mem\\[([0-9]+)\\]\\s=\\s([0-9]+)'
  for (l in p) {
    if (grepl('mask', l)) {
      mask <- gsub('mask = ', '', l)
      next()
    }
    address <- as.numeric(gsub(rx, '\\1', l))
    val <- as.numeric(gsub(rx, '\\2', l))
    m <- apply_mask(mask, address)
    for (a in m)
      res[[as.character(a)]] <- val
  }
  res
}

# p <- "mask = 000000000000000000000000000000X1001X
# mem[42] = 100
# mask = 00000000000000000000000000000000X0XX
# mem[26] = 1"
# p <- strsplit(p, '\n')[[1]]
# sum(unlist(.f(p))) 

p <- readLines('data/day14.txt', warn = FALSE)
res <- .f(p)
print( sum(unlist(res)) )