
set <- function(l) {
  o <- k <- list()
  for (i in seq_along(l)) {
    if (grepl("Tile", l[i])) {
      nm <- gsub(' |:', '', l[i])
      next()
    }
    if (l[i] == '') {
      o[[nm]] <- do.call(rbind, k)
      k <- list()
      next()
    }
    k[[length(k) + 1]] <- strsplit(l[i], '')[[1]]
  }
  o
}

rotate <- function(m, n = 1){
  for (i in 1:n){
    m_ <- m
    nk <- ncol(m)
    for (i in 1:ncol(m))
      m_[nk - i + 1, ] <- m[, i]
    m <- m_
  }
  m
}

flip_rows <- function(m) {
  for (r in 1:nrow(m))
    m[r,] <- rev(m[r,])
  m
}

tile_pos <- function(m){
  o <- list(
    m,
    rotate(m, 1),
    rotate(m, 2),
    rotate(m, 3)
  )
  res <- lapply(o, function(x){
    list(
      m = x,
      m_r = flip_rows(x)
    )
  })
  unique(unlist(res, FALSE))
}

match_pair <- function(l, a, b){
  m.a <- tile_pos(l[[a]])
  m.b <- tile_pos(l[[b]])
  find_edges(m.a, m.b, a, b)
}

find_edges <- function(m.a, m.b, name.a, name.b){
  res <- list()
  for (i in seq_along(m.a)){
    x <- m.a[[i]]
    for (j in seq_along(m.b)){
      y <- m.b[[j]]
      .d <- identical(x[nrow(x), ], y[1, ])
      
      if (.d)
        res[[length(res) + 1]] <- 
          setNames(list(x, y), c(name.a, name.b))
    }
  }
  unique(res)
}

tile_pairs <- function(l) {
  g <- combn(names(l), 2, simplify = FALSE)
  pairs <- res <- list()

  for (i in seq_along(g)){
    a <- g[[i]][1]
    b <- g[[i]][2]
    k <- match_pair(l, a, b)
    if (length(k)){
      pairs[[a]] <- c(pairs[[a]], b)
      pairs[[b]] <- c(pairs[[b]], a)
      res[[length(res) + 1]] <- k
    }
  }
  list(pairs = pairs, res = unlist(res, FALSE))
}

options(scipen = 999)
# l <- c(readLines('data/day20_example.txt'), '')
l <- c(readLines('data/day20.txt'), '')
l <- set(l)
p <- tile_pairs(l)
corners <- p$pairs[lengths(p$pairs) == 2]
print(
  Reduce(`*`, as.numeric(gsub('Tile', '', names(corners))))
)

# part two

link_corners <- function(corners, pairs, n){
  pairs <- pairs[lengths(pairs) < 4]
  res <- list()
  k <- c(names(corners)[1], corners[[1]][1])
  seen <- character()
  repeat{
    if (length(res) == 4 && all(lengths(res) == n))
      break()
    x <- k[length(k)]
    p <- pairs[[x]]
    p <- p[!p %in% c(k, seen) & p %in% names(pairs)]
    k <- c(k, p)
    if (p %in% names(corners)){
      res[[length(res) + 1]] <- k
      seen <- k
      k <- p
    }
  }
  res
}

set_sides <- function(sides){
  next_side <- function(tail, l)
    Filter(function(x) x[1] == tail, l)[[1]]
  .d <- length(sides[[1]])
  m <- matrix('', nrow = .d, ncol = .d)
  n <- nrow(m)
  m[1, ] <- sides[[1]]
  m[, n] <- next_side(m[1, n], sides)
  m[n, ] <- rev(next_side(m[n, n], sides))
  m[, 1] <- rev(next_side(m[n, 1], sides))
  m
}

fill_img <- function(m, l){
  l <- l[lengths(l) > 3]
  n <- nrow(m) - 1
  repeat{
    if (all(m != ''))
      break()
    
    for (i in 2:n){
      for (j in 2:n){
        
        if (m[i, j] != '')
          next()
        
        links <- c(m[i - 1, j], 
                   m[i + 1, j],
                   m[i, j - 1],
                   m[i, j + 1])
        links <- links[links != '']
        if (!length(links))
          next()
        
        next_tile <- Filter(function(x) all(links %in% x), l)
        next_tile <- next_tile[!names(next_tile) %in% m]
        
        if (length(next_tile) > 1 || !length(next_tile))
          next()
        
        l <- l[names(l) != names(next_tile)]
        m[i, j] <- names(next_tile)
      }
    }
  }
  m
}

filter_ <- function(a, b, l, turn_right = FALSE){
  o <- Filter(function(x) all(names(x) %in% c(a, b)), l)
  o <- lapply(o, function(x) {
    if (names(x)[1] == a) 
      return(x)
    rev(lapply(x, function(x_) flip_rows(rotate(x_, 2))))
  })
  if (turn_right)
    o <- lapply(o, function(x) lapply(x, rotate))
  o
}

match_id <- function(a, b){
  for (i in seq_along(a))
    for (j in seq_along(b))
      if (identical(a[[i]], b[[j]]))
        return(c(i, j))
}

arrange_three <- function(x, x.d, x.r, l){
  d_ <- filter_(x, x.d, l)
  r_ <- filter_(x, x.r, l, TRUE)

  dm <- lapply(d_, `[[`, x) 
  rm <- lapply(r_, `[[`, x)
  id <- match_id(dm, rm)

  setNames(
    list(
      d_[[ id[1] ]][[x]],
      d_[[ id[1] ]][[x.d]],
      r_[[ id[2] ]][[x.r]]
    ),
    c(x, x.d, x.r)  
  )
}

arrange_next <- function(m, x, y, l, turn_right = FALSE){
  o <- filter_(x, y, l, turn_right)
  id <- match_id(list(m), lapply(o, `[[`, x))
  o[[ id[2] ]][y]
}

arrange <- function(m, l){
  a <- arrange_three(m[1, 1], m[2, 1], m[1, 2], l)
  for (k in 2:(ncol(m) - 1)){
    tile <- m[1, k]
    mtile <- m[1, k + 1]
    a_ <- arrange_next(a[[tile]], tile, mtile, l, TRUE)
    a <- c(a, a_)
  }
  for (r in 1:(nrow(m) - 1)){
    for (k in 1:ncol(m)){
      tile <- m[r, k]
      mtile <- m[r + 1, k]
      if (mtile %in% names(a))
        next()
      a_ <- arrange_next(a[[tile]], tile, mtile, l)
      a <- c(a, a_)
    }
  }
  a
}

set_map <- function(m, a){
  r <- lapply(1:nrow(m), function(i){
    x <- a[m[i, ]]
    x <- lapply(x, function(x_){
      x_[2:(nrow(x_) - 1), 2:(ncol(x_) - 1)]
    })
    do.call(cbind, x)
  })
  o <- do.call(rbind, r)
  o
}

key <- function() {
  s <- strsplit(
    '                  # 
#    ##    ##    ###
 #  #  #  #  #  #   '
, '\n'
  )[[1]]
  s <- strsplit(s, '')
  do.call(rbind, s)
}

check_grid <- function(grid, coords){
  res <- list(found = FALSE, grid = grid)
  for (i in 1:nrow(coords)){
    r <- coords[i, 'row']
    k <- coords[i, 'col']
    if (grid[r, k] != '#')
      return(res)
    grid[r, k] <- 'O'
  }
  list(found = TRUE, grid = grid)
}

find_key <- function(map, key){
  .d <- dim(key)
  nr <- nrow(map)
  nk <- ncol(map)
  coords <- which(key == "#", arr.ind = TRUE)
  val <- key()
  val[val == '#'] <- 'O'
  map_ <- map
  for (r in 1:nr){
    if ((r + .d[1]) > nr)
      break()
    for (k in 1:nk){
      if ((k + .d[2]) > nk)
        next()
      r_ <- r:(r + .d[1] - 1)
      k_ <- k:(k + .d[2] - 1)
      v <- check_grid(map[r_, k_], coords)
      if (v$found)
        map_[r_, k_] <- v$grid
    }
  }
  if (identical(map, map_)) NULL else map_
}

catm <- function(m){
  cat(apply(m, 1, paste, collapse = ''),'\n', sep = '\n')
}

sides <- link_corners(corners, p$pairs, n = sqrt(length(l)))
m <- fill_img(set_sides(sides), p$pairs)
a <- arrange(m, p$res)
map <- set_map(m, a)
l <- lapply(tile_pos(map), find_key, key = key())
res <- l[lengths(l) > 0][[1]]
print( length(res[res == "#"]) )
