
.f <- function(l, o = 0) {
  l <- sort(l)
  d <- c(l[1] - o, diff(l), 3)
  table(d)
}

l <- "16
10
15
5
1
11
7
19
6
12
4"
l <- strsplit(l, '\n')[[1]]
l <- as.numeric(l)
(.f(l))

l <- "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"
l <- strsplit(l, '\n')[[1]]
l <- as.numeric(l)

(.f(l))

l <- readLines('data/day10.txt', warn = FALSE)
l <- as.numeric(l)
res <- .f(l)
print(res[[1]] * res[[2]])

# part two

nlines <- function(l) {
  l <- sort(l)
  l <- c(0, l, l[length(l)] + 3)
  res <- list()
  for (i in seq_along(l)) {
    x <- l[i]
    s <- l[l > x & l <= (x + 3)]
    n <- length(s)
    if (n < 2)
      next()
    res[[length(res) + 1]] <- s
  }
  res

  link_lines <- function(res) {
    o <- vector('list', max(unlist(res)))
    res <- rev(res)
    
    for (i in seq_along(res)) {
      if (i == length(res))
        break
      
      tail <- res[[i]]
      prev <- res[[i + 1]]
      
      for (p in prev) {
        if (p %in% tail)
          next()
        if (is.null(o[[p]]))
          o[[p]] <- tail
      }
      
    }
    o
  }
  l <- link_lines(res)
  
  seen <- list()
  .h <- function(v) paste0('v_', paste(v, collapse = '-'))
  
  expand_lines <- function(x, o) {
    
    res <- 0
    v <- seen[[.h(x)]]
    if (!is.null(v))
      return(v)
    
    for (l in x) {
      if (is.null(o[[l]]))
        res <- res + 1
      else
        res <- res + expand_lines(o[[l]], o)
    }
    seen[[.h(x)]] <<- res
    res
  }

  o <- lapply(rev(res), function(k){
    expand_lines(k, l)
  })

  o[[length(o)]]
}


l <- "16
10
15
5
1
11
7
19
6
12
4"
l <- strsplit(l, '\n')[[1]]
l <- as.numeric(l)
nlines(l)


l <- "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"
l <- strsplit(l, '\n')[[1]]
l <- as.numeric(l)
nlines(l)

options(scipen = 999)
l <- readLines('data/day10.txt', warn = FALSE)
l <- as.numeric(l)
print( nlines(l) )

# part 2 dynamic programming approach
nlines <- function(l) {
  
  l <- sort(l)
  l <- c(0, l, out <- l[length(l)] + 3)
  v <- integer(out)
  
  v[1] <- 1
  if (.one <- 1 %in% l)
    v[2] <- 1
  if (2 %in% l)
    v[3] <- if (.one) 2 else 1

  for (n in 3:out){
    
    if (!n %in% l)
      next()

    v[n + 1] <- v[n] + v[n - 1] + v[n - 2]
    
  }
  v[out + 1]
}
nlines(l) 

