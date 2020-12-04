l <- scan('data/day01.txt', integer(), quiet = TRUE)
.f <- function(l, m, n = 2020)
  Reduce(`*`, {k <- combn(l, m)}[, colSums(k) == n])
.f(l, 2)
.f(l, 3) # part two


