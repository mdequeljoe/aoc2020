

set <- function(l) {
  rx <- '(.+)\\s\\(contains (.+)\\)$'
  ing <- strsplit(gsub(rx, '\\1', l), ' ')
  alg <- strsplit(gsub(rx, '\\2', l), ', ')
  o <- list(
    food = lapply(seq_along(l), function(i) {
      c(alg = alg[i], ing = ing[i])
    }),
    ing = unique(unlist(ing)),
    alg = unique(unlist(alg))
  )
  o
}

consolidate <- function(f) {
  s <- seq_along(f)
  for (i in s) {
    alg.i <- sort(f[[i]]$alg)
    for (j in s[-i]) {
      alg.j <- sort(f[[j]]$alg)
      if (identical(alg.i, alg.j))
        f[[i]]$ing <- intersect(f[[i]]$ing, f[[j]]$ing)
    }
  }
  unique(f)
}



cross <- function(f) {
  s <- seq_along(f)
  for (i in s) {
    alg.i <- sort(f[[i]]$alg)
    for (j in s[-i]) {
      alg.j <- sort(f[[j]]$alg)
      if (all(alg.i %in% alg.j))
        f[[i]]$ing <- intersect(f[[i]]$ing, f[[j]]$ing)
    }
  }
  f
}

rm_alg <- function(f) {
  s <- seq_along(f)
  for (i in s) {
    x <- f[[i]]
    if (!all(lengths(x) == 1))
      next()
    for (j in s[-i]) {
      alg.j <- f[[j]]$alg
      ing.j <- f[[j]]$ing
      if (x$alg %in% alg.j || x$ing %in% ing.j) {
        f[[j]]$alg <- alg.j[alg.j != x$alg]
        f[[j]]$ing <- ing.j[ing.j != x$ing]
      }
    }
  }
  f
}

filter_ <- function(f) {
  repeat {
    f_ <- rm_alg(cross(consolidate(f)))
    if (identical(f_, f))
      break()
    f <- f_
  }
  f_
}

l <- readLines('data/day21.txt', warn = FALSE)
l <- set(l)
f <- filter_(l$food)
ing <- unlist(lapply(f, `[[`, 'ing'))
o <- unlist(lapply(l$food, `[[`, 'ing'))
print(length(o[!o %in% ing]))

# part two
alg <- unlist(lapply(f, `[[`, 'alg'))
print(paste(ing[order(alg)], collapse = ','))
