library(stringr)

day4 = readLines("../data/day4.txt")

ticket = day4 |>
  str_extract("\\:.*\\|") |>
  str_extract_all("\\d+") |>
  lapply(as.integer)

winners = day4 |>
  str_extract("\\|.*") |>
  str_extract_all("\\d+") |>
  lapply(as.integer)

check_winners = function(ticket, winners){
  x = sum(ticket %in% winners)
  if(x == 0){
    return(0)
  }
  ## This is a way to get the exponential sequence
  return(2^(x-1))
}


sum(mapply(ticket, winners, FUN=check_winners))

## Part 2 
results = rep(1, length(day4))

for(i in seq_along(results)){
  if(i == 0) next 
  n = results[i]
  wins = sum(winners[[i]] %in% ticket[[i]])
  results[seq_len(wins) + i] = results[seq_len(wins) + i] + n
}
sum(results)
