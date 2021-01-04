


set <- function(l) {
  w <- which(l == "Player 2:")
  lapply(list(l[2:(w - 2)],
              l[(w + 1):length(l)]), as.numeric)
}

play <- function(a, b) {
  repeat {
    if (!length(a) || !length(b))
      break()
    
    l.a <- length(a)
    l.b <- length(b)
    if (a[1] > b[1]) {
      a[(l.a + 1):(l.a + 2)] <- c(a[1], b[1])
    } else {
      b[(l.b + 1):(l.b + 2)] <- c(b[1], a[1])
    }
    b <- b[-1]
    a <- a[-1]
    
  }
  if (length(a))
    a
  else
    b
}

l <- set(readLines('data/day22.txt', warn = FALSE))
res <- play(l[[1]], l[[2]])
print(sum(res * rev(seq_along(res))))

# part two
play <- function(a, b) {
  .h <- function(a, b) {
    a <- paste(a, collapse = "_")
    b <- paste(b, collapse = "_")
    paste0('a:', a, 'b:', b)
  }
  
  play_ <- function(a, b) {
    seen <- list()
    res <- list(winner = '', deck = numeric())
    
    repeat {
      h_ <- .h(a, b)
      s <- seen[[h_]]
      if (isTRUE(s)) {
        res$winner <- 'a'
        res$deck <- a
        return(res)
      }
      seen[[h_]] <- TRUE
      
      l.a <- length(a)
      l.b <- length(b)
      
      if (!l.a) {
        res$winner <- 'b'
        res$deck <- b
        return(res)
      }
      
      if (!l.b) {
        res$winner <- 'a'
        res$deck <- a
        return(res)
      }
      
      if ((l.a - 1) >= a[1] && (l.b - 1) >= b[1]) {
        next.a <- a[2:(2 + a[1] - 1)]
        next.b <- b[2:(2 + b[1] - 1)]
        sub <- play_(next.a, next.b)
        if (sub$winner == 'a')
          a <- c(a, a[1], b[1])
        else
          b <- c(b, b[1], a[1])
        b <- b[-1]
        a <- a[-1]
        
        next()
      }
      
      if (a[1] > b[1]) {
        a <- c(a, a[1], b[1])
      } else
        b <- c(b, b[1], a[1])
      b <- b[-1]
      a <- a[-1]
    }
  }
  
  play_(a, b)
}

l <- set(readLines('data/day22.txt', warn = FALSE))
res <- play(l[[1]], l[[2]])
print(sum(res$deck * rev(seq_along(res$deck))))
