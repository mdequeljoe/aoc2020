
get_rules <- function(l) {
  rules <- list()
  for (i in seq_along(l)) {
    if (!nzchar(l[i]))
      break()
    field <- gsub('([a-z]+)\\:.+', '\\1', l[i])
    val <- gsub('[^0-9|-]', ' ', l[i])
    val <- strsplit(trimws(val), '\\s+')[[1]]
    val <- lapply(val, function(x) {
      x <- as.numeric(strsplit(x, '-')[[1]])
      x[1]:x[2]
    })
    rules[[field]] <- val
  }
  rules
}

get_tickets <- function(l) {
  i <- which(l == 'nearby tickets:') + 1
  lapply(strsplit(l[i:length(l)], ","), as.numeric)
}

sum_invalid <- function(l) {
  tickets <- get_tickets(l)
  rules <- get_rules(l)
  rules <- unlist(rules, recursive = TRUE)
  res <- lapply(tickets, function(x) {
    x[!x %in% rules]
  })
  sum(unlist(res))
}
# 
# l <- "class: 0-1 or 4-19
# row: 0-5 or 8-19
# seat: 0-13 or 16-19
# 
# your ticket:
# 11,12,13
# 
# nearby tickets:
# 3,9,18
# 15,1,5
# 5,14,9"
# l <- strsplit(l, '\n')[[1]]

l <- readLines('data/day16.txt', warn = FALSE)
print( sum_invalid(l) )

# part two

rm_invalid <- function(tickets, rules) {
  rules <- unlist(rules, TRUE)
  o <- vapply(tickets, function(x)
    all(x %in% rules), logical(1))
  tickets[o]
}

rake <- function(tickets, rules) {
  
  tickets <- rm_invalid(tickets, rules)
  rules   <- lapply(rules, unlist)
  
  k <- do.call(rbind, tickets)
  k <- apply(k, 2, function(x) {
    unlist(lapply(rules, function(r) {
      if (all(x %in% r))
        1
      else
        0
    }))
  })
  colnames(k) <- paste0('field_', 1:ncol(k))
  fields <- list()
  
  rake_cols <- function(k) {
    ks <- colSums(k)
    for (j in seq_along(ks)) {
      if (ks[j] == 1) {
        m <- k[, j] == 1
        fields[[colnames(k)[j]]] <<- row.names(k)[m]
        k <- k[, -j, drop = FALSE]
        k <- k[!m, , drop = FALSE]
      }
    }
    k
  }
  rake_rows <- function(k) {
    rs <- rowSums(k)
    for (j in seq_along(rs)) {
      if (rs[j] == 1) {
        m <- k[j,] == 1
        fields[[colnames(k)[m]]] <<- row.names(k)[j]
        k <- k[-j, , drop = FALSE]
        k <- k[, !m, drop = FALSE]
      }
    }
    k
  }
  repeat {
    if (length(k) == 0)
      break()
    k <- rake_cols(k)
    k <- rake_rows(k)
  }
  fields
}

l <- readLines('data/day16.txt', warn = FALSE)
tickets <- get_tickets(l)
rules   <- get_rules(l)

res <- rake(tickets, rules)

o <- l[which(l == 'your ticket:') + 1]
o <- as.numeric(strsplit(o, ',')[[1]])
res <- {res <- unlist(res)}[grepl('departure', res)]
i <- as.integer(gsub("field_", '', names(res)))

print( Reduce(`*`, o[i]) )
