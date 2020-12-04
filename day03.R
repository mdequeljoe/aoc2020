
# m <- '..##.......
# #...#...#..
# .#....#..#.
# ..#.#...#.#
# .#...##..#.
# ..#.##.....
# .#.#.#....#
# .#........#
# #.##...#...
# #...##....#
# .#..#...#.#'
# m <- strsplit(m, '\n')[[1]]
# m <- strsplit(m, '')
# map <-
#   matrix(unlist(m),
#          nrow = length(m),
#          ncol = length(m[[1]]),
#          byrow = TRUE)

m <- readLines('data/day03.txt', warn = FALSE)
m <- strsplit(m, '')
map <-
  matrix(unlist(m),
         nrow = length(m),
         ncol = length(m[[1]]),
         byrow = TRUE)

nr <- nrow(map)
nk <- ncol(map)
r <- k <- 1
trees <- 0
down <- 1
right <- 3
repeat{
  if (r > nr)
    break
  
  if (map[r, k] == '#')
    trees <- trees + 1
  
  r <- r + down
  k <- (k + right - 1) %% nk + 1
}
trees

# part two
dir <- list(
  c(1, 1),
  c(3, 1),
  c(5, 1),
  c(7, 1),
  c(1, 2)
)
res <- list()
nr <- nrow(map)
nk <- ncol(map)
for (i in seq_along(dir)){
  r <- k <- 1
  right <- dir[[i]][1]
  down <- dir[[i]][2]
  trees <- 0
  repeat{
    if (r > nr)
      break
    
    if (map[r, k] == '#')
      trees <- trees + 1
    
    r <- r + down
    k <- (k + right - 1) %% nk + 1
  }
  res[[i]] <- trees
}
Reduce(`*`, res)
