
# l <- "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
# byr:1937 iyr:2017 cid:147 hgt:183cm
# 
# iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
# hcl:#cfa07d byr:1929
# 
# hcl:#ae17e1 iyr:2013
# eyr:2024
# ecl:brn pid:760753108 byr:1931
# hgt:179cm
# 
# hcl:#cfa07d eyr:2025 pid:166559648
# iyr:2011 ecl:brn hgt:59in" 
# l <- strsplit(l, '\n')[[1]]
# 
# 

l <- readLines('data/day04.txt', warn = FALSE)
o <- passport <- list()
.required <- 
  c('byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid')

for (i in seq_along(l)){
  if (l[i] == ''){
    o[[length(o) + 1]] <- passport
    passport <- list()
    next
  }
  
  p <- strsplit(l[i], ' ')[[1]]
  for (x in p){
    x <- strsplit(x, ':')[[1]]
    passport[[ x[1] ]] <- x[2]
  }
}
o[[length(o) + 1]] <- passport

valid <- lapply(o, function(x) all(.required %in% names(x)))
length( valid[unlist(valid)] )


#part two
o <- o[unlist(valid)]
lapply(o, function(x){
  
  c(
    byear = as.integer(x$byr) %in% 1920:2002,
    iyear = as.integer(x$iyr) %in% 2010:2020,
    eyear = as.integer(x$eyr) %in% 2020:2030,
    hgt = grepl('^[0-9]+cm|in$', x$hgt),
    hgt2 = if (grepl('cm', x$hgt))
      as.numeric(gsub('cm', '', x$hgt)) %in% 150:193
    else if (grepl('in', x$hgt))
      as.numeric(gsub('in', '', x$hgt)) %in% 59:76
    else
      FALSE,
    hcolor = grepl("^#[a-f0-9]{6}$", x$hcl),
    ecolor = x$ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
    pid = grepl("^[0-9]{9}$", x$pid)
  ) -> v
  
  all(v)
  
}) -> valid
length( valid[unlist(valid)] )






