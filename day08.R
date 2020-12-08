
set_ins <- function(l){
  l <- strsplit(l, '\\s')
  setNames(
    lapply(l, function(x) as.numeric(x[2])),
    lapply(l, `[[`, 1)
  )
}

run_loop <- function(l){
  s <- 0
  i <- 1
  n <- length(l)
  ran <- integer(0)
  finished <- FALSE
  repeat{
    if (i > n){
      finished <- TRUE
      break
    }
    if (i %in% ran)
      break
    ran <- c(ran, i)
    k <- names(l)[i]
    v <- l[[i]]
    if (k == 'acc'){
      s <- s + v
      i <- i + 1
      next()
    }
    if (k == 'jmp'){
      i <- i + v
      next()
    }
    if (k == 'nop')
      i <- i + 1
    
  }
  list(s = s, finished = finished)
}

# l <- "nop +0
# acc +1
# jmp +4
# acc +3
# jmp -3
# acc -99
# acc +1
# jmp -4
# acc +6"
# l <- strsplit(l, '\n')[[1]]
# l <- set_ins(l)
# run_loop(l)

l <- readLines('data/day08.txt', warn = FALSE)
l <- set_ins(l)
run_loop(l)$s

#part two
id <- which(names(l) %in% c('jmp', 'nop'))
for (i in id){
  v <- names(l)[i]
  l_ <- l
  names(l_)[i] <- if (v == 'jmp') 'nop' else 'jmp'
  o <- run_loop(l_)
  if (o$finished)
    print(o$s)
}


