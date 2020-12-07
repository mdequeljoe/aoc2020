

set_list <- function(l){
  l <- gsub('bags', 'bag', l)
  l <- strsplit(l, '\\scontain\\s')
  o <- list()
  for (x in l){
    k <- strsplit(x[2], ',\\s')[[1]]
    qty <- as.integer(gsub('[^0-9+]', '', k))
    qty[is.na(qty)] <- 0
    nms <- gsub('[^a-z]', '', k)
    x[1] <- gsub(' ', '', x[1])
    o[[x[1]]] <- as.list( setNames(qty, nms) )
  }
  o
}

link_bags <- function(o){
  out <- list()
  link_ <- function(o, bag){
    x <- o[[bag]]
    if (is.null(x))
      return()
    res <- character()
    for (b in names(x)){
      res <- if (is.null(out[[b]]))
        c(res, b, link_(o, b))
      else 
        c(res, b, out[[b]])
    }
    unique(res)
  }
  for (b in names(o)){
    out[[b]] <- link_(o, b)
  }
  out
}

l <- "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."
l <- strsplit(l, '\n')[[1]]
o <- set_list(l)
b <- link_bags(o)
length(Filter(function(x) 'shinygoldbag' %in% x, b))


l <- readLines('data/day07.txt', warn = FALSE)
o <- set_list(l)
b <- link_bags(o)
length(Filter(function(x) 'shinygoldbag' %in% x, b))

# part two

# l <- "shiny gold bags contain 2 dark red bags.
# dark red bags contain 2 dark orange bags.
# dark orange bags contain 2 dark yellow bags.
# dark yellow bags contain 2 dark green bags.
# dark green bags contain 2 dark blue bags.
# dark blue bags contain 2 dark violet bags.
# dark violet bags contain no other bags."
# l <- set_list(l)

sum_bag <- function(o, bag){
  
  x <- o[[bag]]
  res <- 0
  if (is.null(x))
    return(res)
  
  for (i in seq_along(x))
    res <- res + x[[i]] + (x[[i]] * sum_bag(o, names(x)[i]))
  
  res
}

l <- readLines('data/day07.txt', warn = FALSE)
o <- set_list(l)
sum_bag(o, 'shinygoldbag')
