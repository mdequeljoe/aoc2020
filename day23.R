

shift <- function(v, offset)
  v[(seq_along(v) - offset - 1) %% length(v) + 1]

sim <- function(s, times = 100){
  k <- as.numeric( strsplit(s, '')[[1]] )
  n <- length(k)
  pos <- 1
  for (i in 1:times){
    
    cup <- k[pos]
    a <- pos + 1
    b <- pos + 3
    id <- (a:b - 1) %% n + 1
    pick <- k[id]
    next_pos <- pos %% n + 1
    p <- if (k[pos] == 1) n else k[pos] - 1
    repeat{
      if (!p %in% pick)
        break()
      p <- if (p == 1) n else p - 1
    }
    k <- k[-id]
    k <- append(k, pick, which(k == p))
    cup_pos <- which(k == cup)
    k <- shift(k, pos - cup_pos)
    pos <- next_pos
  }
  k
}

s <- '362981754'
o <- sim(s, 100)
res <- shift(o, 1 - which(o == 1))
res <- res[-1]
print( paste( res, collapse = ''))

# part two

node <- function(value, prev_, next_){
  self <- new.env(parent = emptyenv())
  self$value <- value
  self$prev_ <- prev_
  self$next_ <- next_
  self
}

game <- function(s, m = NULL){
  self <- new.env(parent = emptyenv())
  
  k <- as.numeric( strsplit(s, '')[[1]] )
  if (!is.null(m))
    k <- c(k, seq(length(k) + 1, m, 1))
  
  self$n <- length(k)
  self$current <- prev_ <- k[1]
  
  nodes <- vector('list', self$n)
  nodes[[prev_]] <- node(prev_, NULL, NULL)
  for (cup in k[-1]){
    if (cup %% 10e3 == 0)
      cat(cup, '\n')
    nodes[[cup]] <- node(cup, nodes[[prev_]], NULL)
    nodes[[prev_]]$next_ <- nodes[[cup]]
    prev_ <- cup
  }
  nodes[[cup]]$next_ <- nodes[[self$current]]
  nodes[[self$current]]$prev_ <- nodes[[cup]]
  self$nodes <- nodes
  
  self$turn <- function(){
    nodes <- self$nodes
    
    #pick three after current
    nd_current <- nodes[[self$current]]
    nd_1 <- nd_current$next_
    nd_2 <- nd_1$next_
    nd_3 <- nd_2$next_
  
    pick <- c(nd_1$value, nd_2$value, nd_3$value)

    nd_rem <- nd_3$next_
    nd_rem$prev_ <- nd_current
    nd_current$next_ <- nd_rem

    # destination p
    p <- if (nd_current$value == 1) self$n else nd_current$value - 1
    repeat{
      if (!p %in% pick)
        break()
      p <- if (p == 1) self$n else p - 1
    }

    # link picks to dest
    dest <- nodes[[p]]
    dest_rem <- dest$next_
    dest$next_ <- nd_1
    nd_3$next_ <- dest_rem
    dest_rem$prev_ <- nd_3
    
    self$current <- nodes[[self$current]]$next_$value
  }
  self$play <- function(n_turns = 10){
    for (i in 1:n_turns){
      if (i %% 1e6 == 0)
        cat(i, '\n')
      self$turn()
    }
  }
  self$show <- function(n = NULL){
    if (is.null(n))
      n <- self$n
    nd <- self$nodes[[1]]
    res <- nd$value
    for (i in 2:n){
      nd <- nd$next_
      res <- c(res, nd$value)
    }
    res
  }
  
  self
}

# s <- '389125467'
# g <- game(s, 1e6)
# g$play(10e6)

s <- '362981754'
g <- game(s, 1e6)
g$play(10e6)
res <- g$show(3)

print(res[2] * res[3])

