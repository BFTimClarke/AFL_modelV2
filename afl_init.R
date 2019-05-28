ngames <- nrow(afl_results_data)
games <- data.frame(
Game <-afl_results_data[,1]
)
colnames(games) <- "Game"

x <- 1:nrow(afl_results_data)

games$Days.Since<- abs(as.integer(difftime(as.Date(afl_results_data[,2],format='%d/%m/%Y'),as.Date(afl_results_data[ngames,2],format='%d/%m/%Y'),units = "days")))
games$Home.Team<- afl_results_data[,4]
games$Away.Team<- afl_results_data[,8]
games$Home.Win<-as.integer(afl_results_data[,13]>0) 
games$Away.Win<-as.integer(afl_results_data[,13]<0)
games$Draw.Win<-as.integer(afl_results_data[,13]==0)
games$Margin<-(afl_results_data[,13])
games$Home.Rest <-365
games$Away.Rest <-365

max_elapsed <- max(games$Days.Since)



wfactor <- data.frame(
Game <- afl_results_data[,1]
)
colnames(wfactor) <- "Game"
wfactor$Ground.home <- 0.5
wfactor$Ground.away <- 1-wfactor$Ground.home
wfactor$Margin.home <- 0.5
wfactor$Margin.away <- 1-wfactor$Margin.home
wfactor$Rest.home <- 0.5
wfactor$Rest.away <- 1- wfactor$Rest.home

wfactor$Elapsed <- 1

mods <- data.frame(
Game <-afl_results_data[,1]
)
colnames(mods) <- "Game"
mods$Ground.mod <- 1
mods$Rest.mod <- 1
mods$Elapsed.mod <- 1

odds <- data.frame(
Date <-afl_odds[,1]
)
colnames(odds) <- "Date"
odds$Days.Since <- 365
odds$Home.Team <- afl_odds$Home.Team
odds$Away.Team <- afl_odds$Away.Team
odds$Home.Prob <- 0.5
odds$Away.Prob <- 0.5
odds$Home.Rest <- 365
odds$Away.Rest <- 365
odds$Home.prop <- 1
odds$Away.prop <- 1
odds$Home.p <- 0.5
odds$Away.p <- 0.5 

odds$Home.Odds <- afl_odds$Home.Odds
odds$Away.Odds <- afl_odds$Away.Odds



