o<- 1:nrow(afl_odds)
y<- 1:100




count <-1
for(val in o){
odds$Days.Since[count]<-abs(as.integer(difftime(as.Date(odds$Date[count]),as.Date(odds$Date[1]))))
count = count+1
}


count <- 1
for(val in o){
count1<- 1
for(val in y){
if(afl_odds$Home.Team[min(nrow(afl_odds),count+count1)] == afl_odds$Home.Team[count])
{
odds$Home.Rest[count] <- count1
break
} else
if(afl_odds$Away.Team[min(nrow(afl_odds),count+count1)] == afl_odds$Home.Team[count])
{
odds$Home.Rest[count] <- count1
break
} 
count1 = count1+1
}
count = count+1
}

count <- 1
for(val in o){
count1<- 1
for(val in y){
if(afl_odds$Home.Team[min(nrow(afl_odds),count+count1)] == afl_odds$Away.Team[count])
{
odds$Away.Rest[count] <- count1
break
} else
if(afl_odds$Away.Team[min(nrow(afl_odds),count+count1)] == afl_odds$Away.Team[count])
{
odds$Away.Rest[count] <- count1
break
} 
count1 = count1+1
}
count = count+1
}
count <- 1
for(val in o)
{
odds$Home.Rest[count] <- odds$Days.Since[count+odds$Home.Rest[count]] - odds$Days.Since[count]
odds$Away.Rest[count] <- odds$Days.Since[count+odds$Away.Rest[count]] - odds$Days.Since[count]
count = count+1
}

count <-1
for(val in o){
if(is.na(odds$Home.Rest[count])){
odds$Home.Rest[count] <- '16'
}
if(is.na(odds$Away.Rest[count])){
odds$Away.Rest[count] <- '16'
}

if(as.integer(odds$Home.Rest[count])>15){
odds$Home.Rest[count] <- 15
} else
if(as.integer(odds$Home.Rest[count])>12){
odds$Home.Rest[count] <- 12
} else
if(as.integer(odds$Home.Rest[count])<5){
odds$Home.Rest[count] <- 5
}

if(as.integer(odds$Away.Rest[count])>15){
odds$Away.Rest[count] <- 15
} else
if(as.integer(odds$Away.Rest[count])>12){
odds$Away.Rest[count] <- 12
} else
if(as.integer(odds$Away.Rest[count])<5){
odds$Away.Rest[count] <- 5
}
count = count+1
}
count <-1
for(val in o){

odds$Home.Rest[count] <- round(as.double(rest_btp[as.character(odds$Home.Rest[count]),as.character(odds$Away.Rest[count])]),digits <- dig)
odds$Away.Rest[count] <- 1 - as.double(odds$Home.Rest[count])
count = count+1
}








