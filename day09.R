
any_valid <- function(x, n)
  any((n - x) %in% x)

# any_valid(1:25, 26)
# any_valid(1:25, 49)
# any_valid(1:25, 100)

.f <- function(l, p = 5){
  for (i in (p + 1):length(l))
    if (!any_valid( l[(i - p):(i - 1)] ,l[i]))
      return(l[i])
}

# l <- "35
# 20
# 15
# 25
# 47
# 40
# 62
# 55
# 65
# 95
# 102
# 117
# 150
# 182
# 127
# 219
# 299
# 277
# 309
# 576"
# l <- strsplit(l, '\n')[[1]]
# l <- as.numeric(l)
# (res <- .f(l, 5))

l <- readLines('data/day09.txt', warn = FALSE)
l <- as.numeric(l)
(res <- .f(l, 25))

# part two
i <- j <- 1
s <- 0
set <- numeric(0)
n <- length(l)
repeat{
  
  if (s == res || i == n)
    break
  
  s <- s + l[i]
  set[length(set) + 1] <- l[i]

  if (s > res){
    s <- 0
    set <- numeric(0)
    j <- j + 1
    i <- j
  }
  i <- i + 1
}
( min(set) + max(set) )