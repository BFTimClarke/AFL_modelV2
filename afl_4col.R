games_4col <- data.frame(games$Home.Team,games$Away.Team)
games_4col$item1wins <- 0.5
games_4col$item2wins <- 0.5

count <- 1

for (val in x){

games_4col$item1wins[count] <-games$Home.Win[count]
games_4col$item2wins[count] <-games$Away.Win[count]

if(games_4col$item1wins[count] == games_4col$item2wins[count]){
games_4col$item1wins[count] <- 0.5
games_4col$item2wins[count] <- 0.5
}
winshare_prop <- round((wfactor$Ground.home[count]/wfactor$Ground.away[count])*(wfactor$Margin.home[count]/wfactor$Margin.away[count])*(wfactor$Rest.home[count]/wfactor$Rest.away[count]),digits=dig)
lossshare_prop<- round((wfactor$Ground.away[count]/wfactor$Ground.home[count])*(wfactor$Margin.away[count]/wfactor$Margin.home[count])*(wfactor$Rest.away[count]/wfactor$Rest.home[count]),digits=dig)
if(winshare_prop == Inf){
games_4col$item1wins[count]<-1
}else
if(lossshare_prop == Inf){
games_4col$item2wins[count]<-1
}else

{
games_4col$item1wins[count]<- round(winshare_prop/(winshare_prop+1),digits <- dig)
games_4col$item2wins[count]<- round(lossshare_prop/(lossshare_prop+1), digits <- dig)
}
count =count+1
}

count <-1
for (val in x){
games_4col$item1wins[count] <- round(games_4col$item1wins[count]*wfactor$Elapsed[count],digits<-3)
games_4col$item2wins[count] <- round(games_4col$item2wins[count]*wfactor$Elapsed[count],digits<-3)
count <- count+1
}



