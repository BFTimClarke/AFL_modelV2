Home.Win.tally = sum(games$Home.Win)
Home.Venue.advantage = Home.Win.tally/ngames
Home.Venue.advantage <- 0.5 + Ground.mod * (Home.Venue.advantage-0.5)

count = 1
for(val in x){

wfactor$Ground.home[count] <- round(0.5/(Home.Venue.advantage*2),digits <- dig)
wfactor$Ground.away[count] <- 1-wfactor$Ground.home[count]
count = count + 1
}