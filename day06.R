
# l <- "abc
# 
# a
# b
# c
# 
# ab
# ac
# 
# a
# a
# a
# a
# 
# b" 
# l <- strsplit(l, '\n')[[1]]

l <- readLines('data/day06.txt', warn = FALSE)
res <- g <- list()

for (i in seq_along(l)){
  if (l[i] == ''){
    res[[length(res) + 1]] <- g
    g <- list()
    next()
  }
  g[[length(g) + 1]] <- strsplit(l[i], '')[[1]]
}
res[[length(res) + 1]] <- g

sum(lengths(lapply(res, function(x)
  Reduce(union, x))))

#part two
sum(lengths(lapply(res, function(x)
  Reduce(intersect, x))))


