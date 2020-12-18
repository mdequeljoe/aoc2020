

diffs <- function(...) {
  g <- expand.grid(...)
  g <- g[rowSums(abs(g)) != 0,]
  lapply(1:nrow(g), function(i)
    unlist(g[i,]))
}

.h <- function(coord) 
  paste(paste0(names(coord), coord), collapse = '')

set_active <- function(l) {
  active <- character()
  for (i in seq_along(l))
    for (j in seq_along(l[[i]]))
      if (l[[i]][j] == "#")
        active[length(active) + 1] <-
          .h( c(x = i, y = j, z = 1) - 1 )
      active
}

area <- function(coord, d) 
  vapply(d, function(x) .h(x + coord), character(1))

# l <- strsplit(".#.\n..#\n###", '\n')[[1]]
# l <- strsplit(l, '')

l <- readLines('data/day17.txt', warn = FALSE)
l <- strsplit(l, '')

nr <- length(l)
nk <- lengths(l)[1]
active <- set_active(l)
r0 <- k0 <- z <- cycles <- 0
end <- 6
d <- diffs(x = -1:1, y = -1:1, z = -1:1)
regions <- list()

repeat {
  if (cycles == end)
    break()
  print(cycles)
  z <- c(z[1] - 1, z, z[length(z)] + 1)
  active_ <- character()
  for (z_ in z) {
    for (r in r0:nr) {
      for (k in k0:nk) {
        
        coord <- c(x = r, y = k, z = z_)
        coord_ <- .h(coord)
        
        a <- regions[[coord_]]
        if (is.null(a)){
          a <- area(coord, d)
          regions[[coord_]] <- a
        }
        
        n <- length(a[a %in% active])
        
        if (!n %in% c(2, 3))
          next()
        
        if (coord_ %in% active) {
          if (n %in% c(2, 3))
            active_[length(active_) + 1] <- coord_
          next()
        }
        
        if (n == 3)
          active_[length(active_) + 1] <- coord_
        
        
      }
    }
  }
  
  cycles <- cycles + 1
  active <- active_
  r0 <- r0 - 1
  nr <- nr + 1
  k0 <- k0 - 1
  nk <- nk + 1
  
}

print(length(active))

# part two

set_active <- function(l) {
  active <- character()
  for (i in seq_along(l))
    for (j in seq_along(l[[i]]))
      if (l[[i]][j] == "#")
        active[length(active) + 1] <-
          .h( c(x = i, y = j, z = 1, w = 1) - 1 )
      active
}


# l <- strsplit(".#.\n..#\n###", '\n')[[1]]
# l <- strsplit(l, '')
l <- readLines('data/day17.txt', warn = FALSE)
l <- strsplit(l, '')

nr <- length(l)
nk <- lengths(l)[1]
active <- set_active(l)
r0 <- k0 <- z <- w <- cycles <- 0
end <- 6
d <- diffs(x = -1:1, y = -1:1, z = -1:1, w = -1:1)

.h <- function(coord) 
  paste(paste0(names(coord), coord), collapse = '')
regions <- list()

repeat {
  
  if (cycles == end)
    break()
  print(cycles)
  
  z <- c(z[1] - 1, z, z[length(z)] + 1)
  w <- c(w[1] - 1, w, w[length(w)] + 1)
  active_ <- character()
  
  for (w_ in w) {
    for (z_ in z) {
      for (r in r0:nr) {
        for (k in k0:nk) {
          
          coord <- c(x = r, y = k, z = z_, w = w_)
          coord_ <- .h(coord)
          
          a <- regions[[coord_]]
          if (is.null(a)){
            a <- area(coord, d)
            regions[[coord_]] <- a
          }
          
          n <- length(a[a %in% active])
          
          if (!n %in% c(2, 3))
            next()
          
          if (coord_ %in% active) {
            if (n %in% c(2, 3))
              active_[length(active_) + 1] <- coord_
            next()
          }
          
          if (n == 3)
            active_[length(active_) + 1] <- coord_
          
        }
      }
    }
  }
  
  cycles <- cycles + 1
  active <- active_
  
  r0 <- r0 - 1
  nr <- nr + 1
  k0 <- k0 - 1
  nk <- nk + 1
  
  cat(r0, nr, k0, nk, z, w, '\n')
}

print(length(active))

