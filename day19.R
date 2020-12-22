
.set <- function(l){
  rx <- '([0-9]+): (.+)'
  nm <- gsub(rx, '\\1', l)
  val <- gsub(rx, '\\2', l)
  val <- gsub("\"", '', val)
  setNames(as.list(val), nm)
}

is_key <- function(v)
  grepl('^[a-z]$', v)

has_sub <- function(x)
  "|" %in% x

pairs <- function(x){
  if (length(x) == 3)
    return(list(x[1], x[3]))
  list(
    x[c(1, 2)],
    x[c(4, 5)]
  )
}

valid_rules <- function(l, root = '0'){
  seen <- list()
  .f <- function(l, nm, res = character()){
    
    x <- l[[nm]]
    if (is_key(x))
      return(x)
    
    if (!is.null(seen[[x]]))
      return(seen[[x]])
    .x <- x
    x <- strsplit(l[[nm]], " ")
    
    if (has_sub( x[[1]] ))
      x <- pairs( x[[1]] )
    
    for (i in seq_along(x)){
      k <- character()
      for (j in seq_along(x[[i]])){
        
        x_ <- x[[i]][j]
        v <- l[[x_]]
        
        if (is_key(v)){
          k <- paste0(k, v)
          next()
        }
        
        if (!length(k))
          k <- paste0(k, .f(l, x_))
        else 
          k <- unlist(lapply(k, function(k_) paste0(k_, .f(l, x_))))
      }
      res <- c(res, k)
    }
    
    seen[[.x]] <<- res
    res
  }
  .f(l, root)
}

# 
# l <- strsplit('0: 4 1 5
# 1: 2 3 | 3 2
# 2: 4 4 | 5 5
# 3: 4 5 | 5 4
# 4: "a"
# 5: "b"
# 
# ababbb
# bababa
# abbbab
# aaabbb
# aaaabbb', '\n')[[1]]
# 
# w <- which(l == '')
# msg <- l[(w + 1):length(l)]
# l <- l[1:(w - 1)]
# l <- .set(l)
# rules <- .f(l, '0')
# msg[msg %in% rules]

l <- readLines('data/day19.txt', warn = FALSE)
w <- which(l == '')
msg <- l[(w + 1):length(l)]
l <- l[1:(w - 1)]
l <- .set(l)
rules <- valid_rules(l, '0')
print( length(msg[msg %in% rules]) )
# 142

# part two

# l <- strsplit('42: 9 14 | 10 1
# 9: 14 27 | 1 26
# 10: 23 14 | 28 1
# 1: "a"
# 11: 42 31
# 5: 1 14 | 15 1
# 19: 14 1 | 14 14
# 12: 24 14 | 19 1
# 16: 15 1 | 14 14
# 31: 14 17 | 1 13
# 6: 14 14 | 1 14
# 2: 1 24 | 14 4
# 0: 8 11
# 13: 14 3 | 1 12
# 15: 1 | 14
# 17: 14 2 | 1 7
# 23: 25 1 | 22 14
# 28: 16 1
# 4: 1 1
# 20: 14 14 | 1 15
# 3: 5 14 | 16 1
# 27: 1 6 | 14 18
# 14: "b"
# 21: 14 1 | 1 14
# 25: 1 1 | 1 14
# 22: 14 14
# 8: 42
# 26: 14 22 | 1 20
# 18: 15 15
# 7: 14 5 | 1 21
# 24: 14 1
# 
# abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
# bbabbbbaabaabba
# babbbbaabbbbbabbbbbbaabaaabaaa
# aaabbbbbbaaaabaababaabababbabaaabbababababaaa
# bbbbbbbaaaabbbbaaabbabaaa
# bbbababbbbaaaaaaaabbababaaababaabab
# ababaaaaaabaaab
# ababaaaaabbbaba
# baabbaaaabbaaaababbaababb
# abbbbabbbbaaaababbbbbbaaaababb
# aaaaabbaabaaaaababaa
# aaaabbaaaabbaaa
# aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
# babaaabbbaaabaababbaabababaaab
# aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba', '\n')[[1]]
# 

# l <- readLines('data/day19.txt', warn = FALSE)
# w <- which(l == '')
# msg <- l[(w + 1):length(l)]
# l <- l[1:(w - 1)]
# l <- .set(l)
# rules <- valid_rules(l)
# print( length(msg[msg %in% rules]) )


filter_msg <- function(msg, a, b){
  rx <- sprintf("(.{%d})", nchar(a[1]))
  m <- strsplit(gsub(rx, "\\1 ", msg), ' ')
  res <- list()
  for (i in seq_along(msg)){
    
    k <- character()
    
    for (v in m[[i]]){
      
      if (v %in% a)
        k <- c(k, '42')
      else if (v %in% b)
        k <- c(k, '31')
      else {
        k <- character()
        break()
      }
    }
    res[[length(res) + 1]] <- k
  }
  unlist( lapply(res, paste, collapse = ' ') )
}

'8 : 42 | 42 8
-> 42 | 42 42 | 42 42 42 |... 

11: 42 31 | 42 11 31
-> 42 31 | 42 42 31 31 | 42 42 42 31 31 31 |...

->
42 then 42 31
42 then 42 42 31 31
42 then 42 42 42 31 31 31 
...
42 42 then 42 31
42 42 then 42 42 31 31
...
'

a <- valid_rules(l, '42')
b <- valid_rules(l, '31')

m <- filter_msg(msg, a, b)
n <- max(nchar(msg)) / nchar(a[1])
a_ <- vapply(1:n, function(k) trimws(strrep('42 ', k)), character(1))
b_ <- vapply(1:n, function(k){
  paste(trimws(strrep('42 ', k)), trimws(strrep('31 ', k)))
}, character(1))
g <- expand.grid(a_, b_, stringsAsFactors = FALSE)
g <- apply(g, 1, paste, collapse = " ")
print( length( m[m %in% g] ) )

