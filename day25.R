
.f <- function(x, n)
  (x * n) %% 20201227

transform <- function(subject, l){
  res <- 1
  for (i in 1:l)
    res <- .f(res, subject)
  res
}

loop_size <- function(key, subject = 7){
  i <- x <- 1
  repeat{
    x <- .f(x, subject)
    if (x == key)
      break()
    i <- i + 1
  }
  i
}

# card <- 5764801
# door <- 17807724
# l <- loop_size(card)
# transform(door, l)

card <- 17115212
door <- 3667832
l <- loop_size(card)
print( transform(door, l) )
