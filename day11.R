

adj_cells <- function(m, r, k){
  nr <- nrow(m)
  nk <- ncol(m)
  o <- list(
    c(r - 1, k),
    c(r + 1, k),
    c(r, k - 1),
    c(r, k + 1),
    c(r - 1, k - 1),
    c(r - 1, k + 1),
    c(r + 1, k - 1),
    c(r + 1, k + 1)
  )
  o <- lapply(o, function(x){
    if (x[1] < 1 || x[1] > nr || x[2] < 1 || x[2] > nk)
      return()
    m[x[1], x[2]]
  })
  unlist(o[lengths(o) > 0])
}

run_map <- function(m){
  m_ <- m
  for (r in 1:nrow(m)){
    for (k in 1:ncol(m)){
      
      v <- m[r, k]
      
      if (v == '.')
        next()
      
      s <- adj_cells(m, r, k)
      .occ <- length(s[s == "#"])
      if (v == 'L' && .occ == 0){
        m_[r, k] <- "#"
        next()
      }
      if (v == "#" && .occ >= 4)
        m_[r, k] <- "L"
    }
  }
  m_
}

g <- "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"

g <- strsplit(g, '\n')[[1]]
g <- strsplit(g, '')
m <- matrix(unlist(g), length(g), lengths(g)[1], byrow = TRUE)



g <- readLines('data/day11.txt', warn = FALSE)
g <- strsplit(g, '')
m <- matrix(unlist(g), length(g), lengths(g)[1], byrow = TRUE)

i <- 0
repeat{
  i <- i + 1
  m_ <- run_map(m)
  if (identical(m_, m))
    break
  m <- m_
}

print( length(m_[m_ == "#"]) )

# part two

adj_cells2 <- function(m, r, k){
  nr <- nrow(m)
  nk <- ncol(m)
  o <- list(
    c( -1, 0),
    c( +1, 0),
    c( 0,  -1),
    c( 0,  +1),
    c( -1,  -1),
    c( -1,  +1),
    c( +1,  -1),
    c( +1,  +1)
  )
  res <- list()
  for (i in seq_along(o)){
    x <- o[[i]]
    x.r <- r + x[1]
    x.k <- k + x[2]
    repeat{
      
      if (x.r < 1 || x.r > nr || x.k < 1 || x.k > nk)
        break()
      
      if (m[x.r, x.k] %in% c("L", "#")){
        res[[i]] <- m[x.r, x.k]
        break()
      }
      x.r <- x.r + x[1]
      x.k <- x.k + x[2]
      
    }
  }
  unlist(res[lengths(res) > 0])
}


run_map2 <- function(m){
  m_ <- m
  for (r in 1:nrow(m)){
    for (k in 1:ncol(m)){
      
      v <- m[r, k]
      
      if (v == '.')
        next()
      
      s <- adj_cells2(m, r, k)
      .occ <- length(s[s == "#"])
      if (v == 'L' && .occ == 0){
        m_[r, k] <- "#"
        next()
      }
      if (v == "#" && .occ >= 5)
        m_[r, k] <- "L"
    }
  }
  m_
}

g <- readLines('data/day11.txt', warn = FALSE)
g <- strsplit(g, '')
m <- matrix(unlist(g), length(g), lengths(g)[1], byrow = TRUE)

i <- 0
repeat{
  i <- i + 1
  m_ <- run_map2(m)
  if (identical(m_, m))
    break
  m <- m_
}

print( length(m_[m_ == "#"]) )
