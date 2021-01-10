


hex <- function(point, coord) {
  diff <- list(
    nw = list(d = c(1, -1), link = 'n'),
    n  = list(d = c(1, 1), link = 'ne'),
    ne = list(d = c(0, 1), link = 'se'),
    se = list(d = c(-1, 1), link = 's'),
    s  = list(d = c(-1, -1), link = 'sw'),
    sw = list(d = c(0, -1), link = 'nw')
  )
  res <- setNames(list(coord), point)
  for (i_ in seq_along(diff)) {
    x <- diff[[point]]
    coord <- coord + x$d
    point <- x$link
    res[[point]] <- coord
  }
  list(
    w  = res[c('sw', 'nw')],
    nw = res[c('nw', 'n')],
    ne = res[c('n', 'ne')],
    e  = res[c('se', 'ne')],
    se = res[c('s', 'se')],
    sw = res[c('sw', 's')]
  )
}

hex_links <- function()
  list(
    w = 'e',
    e = 'w',
    nw = 'se',
    se = 'nw',
    ne = 'sw',
    sw = 'ne'
  )

hex_path <- function(x) {
  i <- 1
  links <- hex_links()
  sides <- names(links)
  res <- list(h <- hex('nw', c(0, 0)))
  repeat {
    if (i > length(x))
      break()
    p <- x[i]
    if (!p %in% sides) {
      p <- paste0(p, x[i + 1])
      i <- i + 1
    }
    
    side <- h[[p]]
    adj_points <- names(h[[links[[p]]]])
    adj_side <- setNames(side, adj_points)
    
    h <- hex(names(adj_side)[1], adj_side[[1]])
    res[[length(res) + 1]] <- h
    i <- i + 1
  }
  res
}

# l <- readLines('data/day24_example.txt', warn = FALSE)
# l <- strsplit(l, '')

l <- readLines('data/day24.txt', warn = FALSE)
l <- strsplit(l, '')

md5 <- digest::getVDigest()

tiles <- vapply(l, function(x) {
  p <- hex_path(x)
  md5(p[length(p)])
}, character(1))

f <- table(tiles)
print(length(f[f == 1]))

# part two
.h <- function(x)
  paste0('hex_', md5(x))

set_names <- function(tiles)
  vapply(tiles, function(x)
    .h(list(x)), character(1))

is_white <- function(n)
  n %% 2 == 0
is_black <- function(n)
  ! is_white(n)

adj_hex <- function(x) {
  links <- hex_links()
  o <- lapply(seq_along(x), function(i) {
    side <- names(x)[i]
    adj_points <- names(x[[links[[side]]]])
    adj_side <- setNames(x[[i]], adj_points)
    hex(names(adj_side)[1], adj_side[[1]])
  })
  names(o) <- set_names(o)
  o
}

update_tiles <- function(o) {
  black_tiles <- o$black_tiles
  adj <- o$adj
  if (is.null(adj))
    adj <- lapply(black_tiles, adj_hex)
  nms <- names(black_tiles)
  res <- res_adj <- white_tiles <- list()
  for (i in seq_along(black_tiles)) {
    x <- nms[i]
    x.adj <- names(a <- adj[[x]])
    n.b <- length(x.adj[x.adj %in% nms])
    a.w <- x.adj[!x.adj %in% nms]
    white_tiles[a.w] <- a[a.w]
    if (n.b <= 2 && n.b != 0) {
      res[[x]] <- black_tiles[[x]]
      res_adj[[x]] <- a
    }
  }
  
  white_tiles <- white_tiles[unique(names(white_tiles))]
  for (i in seq_along(white_tiles)) {
    x.tile <- white_tiles[[i]]
    x <- names(white_tiles)[i]
    x.adj <- names(a <- adj_hex(x.tile))
    n.b <- length(x.adj[x.adj %in% nms])
    if (n.b == 2) {
      res[[x]] <- x.tile
      res_adj[[x]] <- a
    }
  }
  list(black_tiles = res, adj = res_adj)
}

run_days <- function(tiles, n = 100) {
  colors <- table(names(tiles))
  black <- colors[is_black(colors)]
  tiles <- tiles[names(black)]
  o <- list(black_tiles = tiles, adj = NULL)
  for (i_ in 1:n) {
    o <- update_tiles(o)
    cat(i_, ':', length(o[[1]]), '\n')
  }
  invisible(o[[1]])
}

# l <- readLines('data/day24_example.txt', warn = FALSE)
# l <- strsplit(l, '')
l <- readLines('data/day24.txt', warn = FALSE)
l <- strsplit(l, '')

tiles <- lapply(l, function(x) {
  p <- hex_path(x)
  p[[length(p)]]
})
names(tiles) <- set_names(tiles)

res <- run_days(tiles, n = 100)
print(length(res))
# 3937
