count = 1
for(val in x){

margin <- min(abs(games$Margin[count]),90)
if (games$Margin[count]>0){
wfactor$Margin.home[count] <- round(0.5+0.5* sin(margin/180*pi),digits = dig)
wfactor$Margin.away[count] <- 1 - wfactor$Margin.home[count]} else
if (games$Margin[count]<0){
wfactor$Margin.away[count] <- round(0.5+0.5*sin(margin/180*pi),digits = dig)
wfactor$Margin.home[count] <- 1 - wfactor$Margin.away[count]} 

wfactor$Margin.home[count] <- 0.5 + Margin.mod *(wfactor$Margin.home[count]-0.5)
wfactor$Margin.home[count] <- min(wfactor$Margin.home[count],1)
wfactor$Margin.home[count] <- max(wfactor$Margin.home[count],0)

wfactor$Margin.home[count]<- round(wfactor$Margin.home[count],digits <- dig)
wfactor$Margin.away[count] <- 1- wfactor$Margin.home[count]


count = count +1
}

