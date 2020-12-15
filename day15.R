

# .f <- function(v, n = 2020){
#   o <- numeric(n)
#   vlen <- length(v)
#   o[1:vlen] <- v
#   for (i in (vlen + 1):n){
#     
#     x <- o[i - 1]
#     w <- which(o[1:(i - 1)] == x)
#     if (length(w) > 1)
#       o[i] <- w[length(w)] - w[length(w) - 1]
#     else
#       o[i] <- 0
#     
#   }
#   o[n]
# }

.f <- function(v, n = 2020){
  
  vlen <- length(v)
  seen <- integer(0)
  for (i in seq_along(v))
    seen[ v[i] + 1 ] <- i
  
  x <- d <- 0
  
  for (i in (vlen + 1):n){
    
    if (i %% 1e6 == 0)
      print(i / n * 100)
    
    x <- if (d != 0) d else 0
    d <- if (is.na(seen[x + 1])) 0 else i - seen[x + 1]
    seen[x + 1] <- i
    
  }
  x
}

# .f(c(0,3,6)) == 436
# .f(c(1,3,2)) == 1
# .f(c(1,2 ,3)) == 27
# .f(c(2, 3, 1)) == 78
# .f(c(3, 2, 1)) == 438
# .f(c(3, 1, 2)) == 1836
print( .f(c(18,11,9,0,5,1)) )

#part two
m <- 30000000
# .f(c(0,3,6), m)
# .f(c(1,3,2), m)
# .f(c(1,2 ,3), m)
# .f(c(2, 3, 1), m)
# .f(c(3, 2, 1), m)
# .f(c(3, 1, 2), m)

print( .f(c(18,11,9,0,5,1), m) )
# 116590
