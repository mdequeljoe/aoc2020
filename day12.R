
# Action N means to move north by the given value.
# Action S means to move south by the given value.
# Action E means to move east by the given value.
# Action W means to move west by the given value.
# Action L means to turn left the given number of degrees.
# Action R means to turn right the given number of degrees.
# Action F means to move forward by the given value in the direction the ship is 
# currently facing.

change_dir <- function(current, dir, degrees = 90){
  degrees <- degrees / 90
  directions <- c('E', 'S', 'W', 'N')
  i <- which(directions == current)
  o <- if (dir == 'R') 1 else 3
  directions[ (i + (o * degrees) - 1) %% 4 + 1 ]
}

advance <- function(k, val, direction){
  list(
    N = c(0, val),
    S = c(0, -val),
    E = c(val, 0),
    W = c(-val, 0)
  )[[direction]] + k
}

# l <- "F10
# N3
# F7
# R90
# F11"
# l <- strsplit(l, '\n')[[1]]

l <- readLines('data/day12.txt', warn = FALSE)
action <- gsub('[0-9]+', '', l)
v <- as.integer(gsub('[A-Z]', '', l))
face <- 'E'
k <- c(0, 0)

for (i in seq_along(l)){
  act <- action[i]
  if (act == 'F'){
    k <- advance(k, v[i], face)
    next()
  }
  if (act %in% c("L", 'R')){
    face <- change_dir(face, act, v[i])
    next()
  }
  k <- advance(k, v[i], act)
}
print( sum(abs(k))  )

# part two
# Action N means to move the waypoint north by the given value.
# Action S means to move the waypoint south by the given value.
# Action E means to move the waypoint east by the given value.
# Action W means to move the waypoint west by the given value.
# Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
# Action R means to rotate the waypoint around the ship right (clockwise) 
# the given number of degrees.
# Action F means to move forward to the waypoint a number of times equal 
# to the given value.

# l <- "F10
# N3
# F7
# R90
# F11"
# l <- strsplit(l, '\n')[[1]]

l <- readLines('data/day12.txt', warn = FALSE)
action <- gsub('[0-9]+', '', l)
v <- as.integer(gsub('[A-Z]', '', l))
k <- c(0, 0)
face <- 'E'
waypoint <- wd <- c(10, 1)

rotate_point <- function(d, dir, degrees){
  n <- degrees / 90
  for (i in 1:n)
    d <- if (dir == 'R')
      c(d[2], -d[1])
    else
      c(-d[2], d[1])
  d
}

for (i in seq_along(l)){
  
  act <- action[i]
  if (act == 'F'){
    k <- k + wd * v[i]
    waypoint <- k + wd
    next()
  }
  if (act %in% c("L", 'R')){
    wd <- rotate_point(wd, act, v[i])
    waypoint <- k + wd
    next()
  }
  waypoint <- advance(waypoint, v[i], act)
  wd <- waypoint - k
  
}
print( sum(abs(k)) )

