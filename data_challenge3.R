library(readxl)
# Finding impact of 17 yr olds in voting block sizes
adult_population_new <- read_excel("Election Data/adult_population_new.xlsx")
child_population_new <- read_excel("Election Data/child_population_new.xlsx")

# Making data sets match
adult_population_max <- adult_population_new[c(adult_population_new$Year >= 1990),]
adult_population_rev <- adult_population_max[rev(1:nrow(adult_population_max)),]
child_population_even <- child_population_new[c(child_population_new$Year %% 2 == 0),]

# Plotting age block sizes over time
plot(child_population_even$Year,child_population_even$Pop15to17,
     main="US Population by Age Group over Time",
     xlab="Year",ylab="US Population by Age Group",type="o",
     col="#000249",pch=16,lwd=2,
     ylim= c(min(child_population_even$Pop15to17),max(adult_population_rev$VAP25to44)))

points(adult_population_rev$Year,adult_population_rev$VAP18to24,col="#0F4392",pch=16)
lines(adult_population_rev$Year,adult_population_rev$VAP18to24,col="#0F4392",lwd=2)

points(adult_population_rev$Year,adult_population_rev$VAP25to44,col="#FF4949",pch=16)
lines(adult_population_rev$Year,adult_population_rev$VAP25to44,col="#FF4949",lwd=2)

points(adult_population_rev$Year,adult_population_rev$VAP45to64,col="#DD1717",pch=16)
lines(adult_population_rev$Year,adult_population_rev$VAP45to64,col="#DD1717",lwd=2)

points(adult_population_rev$Year,adult_population_rev$VAP65plus,col="#FFCD00",pch=16)
lines(adult_population_rev$Year,adult_population_rev$VAP65plus,col="#FFCD00",lwd=2)

legend(1990,7.6e+7,legend=c("15-17","18-24","25-44","45-64","65+"),
       col=c("#000249","#0F4392","#FF4949","#DD1717","#FFCD00"),pch=16,lty=1,ncol=1,lwd=2)

# Plot youth age block proportions when 15-17 year olds are and are not included over time
without_proportion_youth = adult_population_rev$VAP18to24/adult_population_rev$VotingAgePop
with_proportion_youth = (adult_population_rev$VAP18to24 + child_population_even$Pop15to17)/(adult_population_rev$VotingAgePop + child_population_even$Pop15to17)
plot(adult_population_rev$Year,with_proportion_youth,
     main="Youth as Proportion of Adult Population over Time",
     xlab="Year",ylab="Youth Proportion of Adult Population",type="o",
     col="#0F4392",pch=16,lwd=2,
     ylim= c(min(without_proportion),max(with_proportion)))
points(adult_population_rev$Year,without_proportion_youth,col="#DD1717",pch=16)
lines(adult_population_rev$Year,without_proportion_youth,col="#DD1717",lwd=2)

legend(1990,0.17,c("w/ 15-17","w/out 15-17"),fill=c("#0F4392","#DD1717"))
# Plot all age block proportions when 15-17 year olds are and are not included for 2018
adult_2018 <- adult_population_max[which(adult_population_max==2018),2:6]
child_2018 <- child_population_even[which(child_population_even==2018),2]

without_proportion_all_2018 <- adult_2018[2:5]/adult_2018[[1]]
with_proportion_all_2018 <- (adult_2018[2:5] + child_2018[[1]])/(adult_2018[[1]] + child_2018[[1]])

matrix_2018 <- matrix(unlist(c(without_proportion_all_2018,with_proportion_all_2018)),
                      nrow = 2,ncol = 4,byrow = TRUE)

barplot(matrix_2018,main="Age Groups as Proportion of Adult Population 2018",
        xlab="Age Group",ylab ="Proportion of Adult Population",col=c("#000249","#FFCD00"),
        names.arg=c("18-24","25-44","45-64","65+"),beside=TRUE)

legend(1,0.35,c("w/out 15-17","w 15-17"),fill=c("#000249","#FFCD00"))

# Plot voting rate over time for general elections with different lines for each age group for general elections
general_voting_new <- read_excel("Election Data/general_voting_new.xlsx")
general_voting_new <- data.frame(general_voting_new)

plot(general_voting_new$Year,general_voting_new$VAPTurnoutRate18to24.,
     main="General Election Turnout by Age Group Over Time",
     xlab="Year",ylab="Voter Turnout",type="o",
     col="#000249",pch=16,lwd=2,
     ylim= c(10,80))

points(general_voting_new$Year,general_voting_new$VAPTurnoutRate25to44.,col="#0F4392",pch=16)
lines(general_voting_new$Year,general_voting_new$VAPTurnoutRate25to44.,col="#0F4392",lwd=2)

points(general_voting_new$Year,general_voting_new$VAPTurnoutRate45to64.,col="#DD1717",pch=16)
lines(general_voting_new$Year,general_voting_new$VAPTurnoutRate45to64.,col="#DD1717",lwd=2)

points(general_voting_new$Year,general_voting_new$VAPTurnoutRate65plus.,col="#FFCD00",pch=16)
lines(general_voting_new$Year,general_voting_new$VAPTurnoutRate65plus.,col="#FFCD00",lwd=2)

legend(1963,80,legend=c("18-24","25-44","45-64","65+"),
       col=c("#000249","#0F4392","#DD1717","#FFCD00"),pch=16,lty=1,ncol=1,lwd=2)

# Plot voting rate against time
primary_voting_2000_new <- read_excel("Election Data/primary_voting_2000_new.xlsx")
primary_voting_2004_new <- read_excel("Election Data/primary_voting_2004_new.xlsx")
primary_voting_2008_new <- read_excel("Election Data/primary_voting_2008_new.xlsx")
primary_voting_2012_new <- read_excel("Election Data/primary_voting_2012_new.xlsx")
primary_voting_2016_new <- read_excel("Election Data/primary_voting_2016_new.xlsx")
primary_voting_2020_new <- read_excel("Election Data/primary_voting_2020_new.xlsx")

# Calculate mean turnout rates for primary elections by year and graph
mean_turnout_2000 <- mean(as.numeric(primary_voting_2000_new$VAPTurnoutRate),na.rm = TRUE)
mean_turnout_2004 <- mean(as.numeric(primary_voting_2004_new$VAPTurnoutRate),na.rm = TRUE)
mean_turnout_2008 <- mean(as.numeric(primary_voting_2008_new$VAPTurnoutRate),na.rm = TRUE)
mean_turnout_2012 <- mean(as.numeric(primary_voting_2012_new$VAPTurnoutRate),na.rm = TRUE)
mean_turnout_2016 <- mean(as.numeric(primary_voting_2016_new$VAPTurnoutRate),na.rm = TRUE)
mean_turnout_2020 <- mean(as.numeric(primary_voting_2000_new$VAPTurnoutRate),na.rm = TRUE)

barplot(c(mean_turnout_2000,mean_turnout_2004,mean_turnout_2008,mean_turnout_2012,
          mean_turnout_2016,mean_turnout_2020),main="Primary Election Turnout by Year",
        xlab="Year",ylab="Voter Turnout",names.arg=c(2000,2004,2008,2012,2016,2020),col="#FF4949")