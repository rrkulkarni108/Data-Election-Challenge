
#PRIMARY VOTING 2000

df <- primary_voting_2000_new
turnoutRate <- df$VAPTurnoutRate
state <- df$State
votingAgePop <- df$VotingAgePop

na.omit(df)
#any(is.na(df))

df$VAPTurnoutRate <- suppressWarnings(as.numeric(gsub("\\.", "", df$VAPTurnoutRate)))
df$VotingAgePop <- suppressWarnings(as.numeric(gsub("\\.", "", df$VotingAgePop)))

#complete.cases(df)
#df[complete.cases(df), ]
#df[!complete.cases(df), ]

str(df)
newdf<- df[complete.cases(df), ]
newdf$VAPTurnoutRate<- suppressWarnings(as.numeric(gsub("\\.", "", newdf$VAPTurnoutRate)))
#any(is.na(newdf))

nums <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)
for (i in nums){
  newState[i] = state[i]
}
newState
#any(is.na(newState))

library(ggplot2)
newState <-as.factor(newState)

plot(newState, newdf$VAPTurnoutRate, main = "Primary 2000 VAP Turnout Rate by State",
     xlab = "State", ylab = "VAP Turnout Rate",
     pch = 19)
plot(newState, newdf$VotingAgePop, main = "Primary 2000 Voting Age Population by State",
     xlab = "State", ylab = "Voting Age Population",
     pch = 19)

mean(newdf$VAPTurnoutRate)



#PRIMARY VOTING 2020:
df <- primary_voting_2020_new
turnoutRate <- df$VAPTurnoutRate
state <- df$State
votingAgePop <- df$VotingAgePop
mean(turnoutRate)


#Are group means equal across the groups? (18-24, 25-44, etc)
#GENERAL VOTING:
gendf <- general_voting_new
year <- gendf$Year
generalVAPTurnoutRate <- gendf$`VAPTurnoutRate
`
# we will compare each group to general turnout rate
group_18_to_24_rate <- gendf$`VAPTurnoutRate18to24
`
group_25_to_44_rate <- gendf$`VAPTurnoutRate25to44
`
group_45_to_64_rate <- gendf$`VAPTurnoutRate45to64
`
group_65plus_rate <- gendf$`VAPTurnoutRate65plus
`
#general pop graphs and correlation
plot(year, generalVAPTurnoutRate, main = " VAP Turnout Rate Over Time (1964-2018)",
      xlab = "Year", ylab = "VAP Turnout Rate",
      pch = 16)
reg<-lm(generalVAPTurnoutRate ~ year, data = general_voting_new)
abline(reg, col = "red")
#r^2
summary(reg)$r.squared 

#18-24 yr olds graphs and correlation
reg2<-lm(group_18_to_24_rate ~ year, data = general_voting_new)
plot(year, group_18_to_24_rate, main = "VAP Turnout Rate Over Time of 18-24 year olds (1964-2018)",
     xlab = "Year", ylab = "VAP Turnout Rate",
     pch = 17)
abline(reg2, col = "red")
#r^2
summary(reg2)$r.squared

#25-44 year olds graphs and correlation
reg3<-lm(group_25_to_44_rate ~ year, data = general_voting_new)
plot(year, group_25_to_44_rate, main = "VAP Turnout Rate Over Time of 25-44 year olds (1964-2018)",
     xlab = "Year", ylab = "VAP Turnout Rate",
     pch = 18)
abline(reg3, col = "red")
#r^2
summary(reg3)$r.squared

#45-64 year olds graphs and correlation
reg4<-lm(group_45_to_64_rate ~ year, data = general_voting_new)
plot(year, group_45_to_64_rate, main = "VAP Turnout Rate Over Time of 45-64 year olds (1964-2018)",
     xlab = "Year", ylab = "VAP Turnout Rate",
     pch = 19)
abline(reg4, col = "red")
#r^2
summary(reg4)$r.squared

#65 plus year olds graphs and correlation
reg5<-lm(group_65plus_rate ~ year, data = general_voting_new)
plot(year, group_65plus_rate, main = "VAP Turnout Rate Over Time of 65+ year olds (1964-2018)",
     xlab = "Year", ylab = "VAP Turnout Rate",
     pch = 20)
abline(reg5, col = "red")
#r^2
summary(reg5)$r.squared
