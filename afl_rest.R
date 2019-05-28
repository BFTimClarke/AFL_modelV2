x <- 1:nrow(afl_results_data)
y <- 1:100

count <- 1
for (val in x){
count1 <-1
for (val in y){
if(afl_results_data$Home.Team[max(count-count1,1)] == afl_results_data$Home.Team[min(ngames,count)]){
games$Home.Rest[count] <- count1
break
} else
if(afl_results_data$Away.Team[max(count-count1,1)] == afl_results_data$Home.Team[min(ngames,count)]){
games$Home.Rest[count] <- count1
break
}
else{
count1 = count1+1}
}
count = count+1
}

count <- 1
for (val in x){
count1 <-1
for (val in y){
if(afl_results_data$Home.Team[max(count-count1,1)] == afl_results_data$Away.Team[min(ngames,count)]){
games$Away.Rest[count] <- count1
break
} else
if(afl_results_data$Away.Team[max(count-count1,1)] == afl_results_data$Away.Team[min(ngames,count)]){
games$Away.Rest[count] <- count1
break
}
else{
count1 = count1+1}
}
count = count+1
}

count = 1
for(val in x){

if (games$Home.Rest[count] == 365){
games$Home.Rest[count]<-NA
}
if (games$Away.Rest[count] == 365){
games$Away.Rest[count]<-NA
}

count = count+1
}

games$Home.Rest[1] <- NA
games$Away.Rest[1] <- NA

#difference between
count <-1
for(val in x){ 
games$Home.Rest[count] <- games$Days.Since[count-games$Home.Rest[count]]-games$Days.Since[count]
games$Away.Rest[count] <- games$Days.Since[count-games$Away.Rest[count]]-games$Days.Since[count]
count =count +1
}

rest_3col <- data.frame(games$Home.Rest, games$Away.Rest,games$Home.Win)

count <- 1
for (val in x){

if(is.na(rest_3col$games.Home.Rest[count])){
rest_3col$games.Home.Rest[count] <- '16'
}

if(is.na(rest_3col$games.Away.Rest[count])){
rest_3col$games.Away.Rest[count] <- '16'
}

if(as.integer(rest_3col$games.Home.Rest[count])>15){
rest_3col$games.Home.Rest[count]<-15
} else
if(as.integer(rest_3col$games.Home.Rest[count])>12){
rest_3col$games.Home.Rest[count]<-12
}else
if(as.integer(rest_3col$games.Home.Rest[count])<5){
rest_3col$games.Home.Rest[count]<-5
}





if(as.integer(rest_3col$games.Away.Rest[count])>15){
rest_3col$games.Away.Rest[count]<-15
} else
if(as.integer(rest_3col$games.Away.Rest[count])>12){
rest_3col$games.Away.Rest[count]<-12
}else
if(as.integer(rest_3col$games.Away.Rest[count])<5){
rest_3col$games.Away.Rest[count]<-5
}


if(games$Home.Win[count] ==1){
rest_3col$games.Home.Win[count] <- 'W1'
}else
if(games$Home.Win[count] ==games$Away.Win[count]){
rest_3col$games.Home.Win[count] <- 'D'
}else
if(games$Home.Win[count] ==0){
rest_3col$games.Home.Win[count] <- 'W2'
}
else{
rest_3col$games.Home.Win[count] <- 'W2'
}


count = count+1
}





#15 only comes start of season (not a factor as both teams will be 15), bye rounds and finals (no longer bye rounds), finals not included in model predictions
rest_4col <- codes_to_counts(rest_3col,c("W1","W2","D"))
rest_4col$games.Home.Rest <- as.factor(rest_4col$games.Home.Rest)
rest_4col$games.Away.Rest <- as.factor(rest_4col$games.Away.Rest)

rest_btd <- btdata(rest_4col,return_graph = TRUE)
rest_btfit <- btfit(rest_btd,1)
rest_btfit$pi
rest_btp <- btprob(rest_btfit)



count <- 1
for (val in x){

if(is.na(games$Home.Rest[count])){
games$Home.Rest[count]<-15
} else
if(games$Home.Rest[count]>15){
games$Home.Rest[count]<-15
} else
if(games$Home.Rest[count]>12){
games$Home.Rest[count]<-12
} else
if(games$Home.Rest[count]<5){
games$Home.Rest[count] <- 5}


if(is.na(games$Away.Rest[count])){
games$Away.Rest[count]<-15
} else
if(games$Away.Rest[count]>15){
games$Away.Rest[count]<-15
} else
if(games$Away.Rest[count]>12){
games$Away.Rest[count]<-12
} else
if(games$Away.Rest[count]<5){
games$Away.Rest[count] <- 5}
count = count +1
}

z <- 1:nrow(rest_btp)
count = 1
for (val in z){
rest_btp[count,count] <- 0.5
count = count+1
}


count = 1
for (val in x){
wfactor$Rest.home[count] <- rest_btp[as.character(games$Home.Rest[count]),as.character(games$Away.Rest[count])]
wfactor$Rest.away[count] <- rest_btp[as.character(games$Away.Rest[count]),as.character(games$Home.Rest[count])]


count = count +1
}

wfactor$Rest.home <- 0.5 + Rest.mod *(wfactor$Rest.home-0.5)
wfactor$Rest.home <- round(0.5 / (wfactor$Rest.home*2),digits <- dig)
wfactor$Rest.away <- 1-wfactor$Rest.home



