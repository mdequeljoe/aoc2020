l <- readLines('data/day02.txt', warn = FALSE)
l <- strsplit(l, "-|\\s|:")
v <- vapply(l, function(x){
  g <- gregexpr(x[3], x[5])[[1]]
  lim <- as.integer( x[1]:x[2] )
  if (length(g[g > 0]) %in% lim) 1L else 0L
}, integer(1))
sum(v)

#part two
v <- vapply(l, function(x){
  g <- gregexpr(x[3], x[5])[[1]]
  lim <- as.integer(c(x[1], x[2]))
  if (length(g[g %in% lim]) == 1) 1L else 0L
}, integer(1))
sum(v)
