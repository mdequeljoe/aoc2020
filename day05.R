
.f <- function(s){
  s <- strsplit(s, '')[[1]]
  r <- 0:127
  k <- 0:7
  rlen <- function() length(r)
  klen <- function() length(k)
  o <- list(
    F = function() r <<- r[1:(rlen() / 2)],
    B = function() r <<- r[(rlen() / 2 + 1):rlen()],
    L = function() k <<- k[1:(klen() / 2)],
    R = function() k <<- k[(klen() / 2 + 1):klen()]
  )
  for (l in s)
    o[[l]]()
  
  r * 8 + k
}

.f("FBFBBFFRLR") == 357
.f('BFFFBBFRRR') == 567
.f('FFFBBBFRRR') == 119
.f('BBFFBBFRLL') == 820

# part one
l <- readLines('data/day05.txt', warn = FALSE)
max(id <- vapply(l, .f, numeric(1)))

# part two
g <- expand.grid(r = (0:127 * 8), k = 0:7)
g <- rowSums(g)
g <- sort(g[!g %in% id])
g[diff(g) != 1][-1]

#part one method 2
l <- readLines('data/day05.txt', warn = FALSE)
l <- gsub('F|L', '0', l)
l <- gsub('B|R', '1', l)
max(strtoi(substr(l, 1, 7), base = 2) * 8 +
      strtoi(substr(l, 8, 11), base = 2))
