source("afl_read.r")
x <- 1:nrow(afl_odds)
dig <- 3

#STRENGTH OF FORM (Higher = more prevalent)
elapsed_exp <- 3
Ground.mod <- 1
Rest.mod <- 1
Elapsed.mod <- 1

#CAN ONLY BE APPLIED HISTORICALLY!!!
Margin.mod <- 1





source("afl_init.r")
source("afl_ground.r")
source("afl_margin.r")
source("afl_rest.r")
source("afl_elapsed.r")
source("afl_mods.r")
source("afl_4col.r")

games_btd <- btdata(games_4col)
games_fit <- btfit(games_btd,1)
games_btprob <- btprob(games_fit)

source("odds_rest.r")

count <-1
for(val in o){
odds$Home.Prob[count] <- Home.Venue.advantage
odds$Away.Prob[count] <- 1 - odds$Home.Prob[count]
odds$Home.prop[count] <- (odds$Home.Prob[count]/odds$Away.Prob[count]) * (as.double(odds$Home.Rest[count])/as.double(odds$Away.Rest[count]))
count = count+1
}

count = 1
for(val in o){
hometeam<- odds$Home.Team[count]
awayteam<- odds$Away.Team[count]
if(hometeam=="Brisbane"){
hometeam <- "Brisbane Lions"
}
if(awayteam=="Brisbane"){
awayteam <- "Brisbane Lions"
}
if(hometeam=="Western Bulldogs"){
hometeam <- "Footscray"
}
if(awayteam=="Western Bulldogs"){
awayteam <- "Footscray"
}
if(hometeam=="GWS Giants"){
hometeam <- "GWS"
}
if(awayteam=="GWS Giants"){
awayteam <- "GWS"
}

odds$Home.prop[count] <- odds$Home.prop[count] * games_btprob[hometeam,awayteam]/(1-games_btprob[hometeam,awayteam])
odds$Home.p[count] <- odds$Home.prop[count]/(odds$Home.prop[count]+1)
odds$Away.p[count] <- 1-odds$Home.p[count]
count <- count+1
}


bets <- data.frame(
Date <- odds$Date
)
bets$Home.Team <- odds$Home.Team
bets$Away.Team <- odds$Away.Team

bets$Home.Win <- as.integer(afl_odds$Home.Score>afl_odds$Away.Score)
bets$Away.Win <- as.integer(afl_odds$Away.Score>afl_odds$Home.Score)

count <- 1
for(val in o){
if(bets$Home.Win[count] == bets$Away.Win[count]){
bets$Home.Win[count] <- 0.5
bets$Away.Win[count] <- 0.5
}
count <- count+1
}

bets$Home.BOdds <- odds$Home.Odds
bets$Away.BOdds <- odds$Away.Odds
bets$Home.MOdds <- 1/odds$Home.p
bets$Away.MOdds <- 1/odds$Away.p
bets$Home.bet <- 0
bets$Away.bet <- 0
bets$PL <- 0




b <- 1:209

stakeprof = 0
PandL = 0
turnover = 0
stake = 1
R <- 0

count = 1
for (val in b){
if(bets$Home.Team[count] != "Carlton" && bets$Away.Team[count] != "Carlton"){

if(bets$Home.BOdds[count] > bets$Home.MOdds[count]){
bets$Home.bet[count] <- 1
turnover <- turnover + stake
bets$PL[count] <-bets$Home.bet[count]*bets$Home.Win[count]*bets$Home.BOdds[count] - stake
}
if(bets$Away.BOdds[count] > bets$Away.MOdds[count]){
bets$Away.bet[count] <- 1
turnover <- turnover + stake
bets$PL[count] <- bets$Away.bet[count]*bets$Away.Win[count]*bets$Away.BOdds[count] - stake
}


PandL <- PandL+bets$PL[count]
count <- count+1


}
}






