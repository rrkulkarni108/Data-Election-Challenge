## PART 1

## Import Data
library("readxl")
AgeDistribution <- read_excel("~/Desktop/age_distribution_by_party_new.xlsx")

# Piechart 1a
## Pie Chart for Age Distribution of Republicans in California
Y = AgeDistribution$Age
X = AgeDistribution$Republican

pct <- round(X/sum(X)*100)
lbls <- paste(pct,"%",sep="") # ad % to labels
colors <- c("#000249","#0F4392","#FF4949","#FFCD00")

pie(X, labels = lbls, col = colors, radius = 1, main = "Age Distribution of Republicans in California (2014)")
legend("topright", Y, fill = colors)

# Piechart 1b
## Pie Chart for Age Distribution of Democrats in California 
Y = AgeDistribution$Age
X = AgeDistribution$Democrat

pct <- round(X/sum(X)*100)
lbls <- paste(pct,"%",sep="") # ad % to labels
colors <- c("#000249","#0F4392","#FF4949","#FFCD00")

pie(X, labels = lbls, col = colors, radius = 1, main = "Age Distribution of Democrats in California (2014)")
legend("topright", Y, fill = colors)

# Piechart 1c
## Pie Chart for Age Distribution of 3rd Party Voters in California
Y = AgeDistribution$Age
X = AgeDistribution$Other

pct <- round(X/sum(X)*100)
lbls <- paste(pct,"%",sep="") # ad % to labels
colors <- c("#000249","#0F4392","#FF4949","#FFCD00")

pie(X, labels = lbls, col = colors, radius = 1, main = "Age Distribution of 3rd Party Voters in California (2014)")
legend("topright", Y, fill = colors)

## PART 2
## Import Data
library("readxl")
PartyAffiliation <- read_excel("~/Desktop/party_affiliation_new.xlsx")

# Piechart 2a
## Pie Chart for Party Affiliation Amongst 18 to 29 year olds in California
Y = PartyAffiliation$Party
X = PartyAffiliation$Age18to29

pct <- round(X/sum(X)*100)
lbls <- paste(pct,"%",sep="") # ad % to labels
colors <- c("#000249","#0F4392","#FF4949")

pie(X, labels = lbls, col = colors, radius = 1, main = "Party Affiliation Amongst 18 to 29 year olds in California (2014)")
legend("topright", Y, fill = colors)

# Piechart 2b
## Pie Chart for Party Affiliation Amongst 30 to 49 year olds in California
Y = PartyAffiliation$Party
X = PartyAffiliation$Age30to49

pct <- round(X/sum(X)*100)
lbls <- paste(pct,"%",sep="") # ad % to labels
colors <- c("#000249","#0F4392","#FF4949")

pie(X, labels = lbls, col = colors, radius = 1, main = "Party Affiliation Amongst 30 to 49 year olds in California (2014)")
legend("topright", Y, fill = colors)

# Piechart 2c
## Pie Chart for Party Affiliation Amongst 50 to 64 year olds in California
Y = PartyAffiliation$Party
X = PartyAffiliation$Age50to64

pct <- round(X/sum(X)*100)
lbls <- paste(pct,"%",sep="") # ad % to labels
colors <- c("#000249","#0F4392","#FF4949")

pie(X, labels = lbls, col = colors, radius = 1, main = "Party Affiliation Amongst 50 to 64 year olds in California (2014)")
legend("topright", Y, fill = colors)

# Piechart 2d
## Pie Chart for Party Affiliation Amongst 65+ year olds in California
Y = PartyAffiliation$Party
X = PartyAffiliation$Age65plus

pct <- round(X/sum(X)*100)
lbls <- paste(pct,"%",sep="") # ad % to labels
colors <- c("#000249","#0F4392","#FF4949")

pie(X, labels = lbls, col = colors, radius = 1, main = "Party Affiliation Amongst 65+ year olds in California (2014)")
legend("topright", Y, fill = colors)

## PART 3

# Graph for General VAP vs Voting vs Registration for all age groups
df <- data.frame(
  Values = rep(c("Voting Age Population", "Amount Registered", "Amount Voted"), 2),
  Year = c(rep("2000", 3), rep("2002", 3), rep("2004", 3), rep("2006", 3), rep("2008", 3), rep("2010", 3), rep("2012", 3), rep("2014", 3), rep("2016", 3), rep("2018", 3)),
  Number_of_Occurrences = c(202609000, 129467151, 110827123, 210421000, 128146389, 89008083, 215694000, 142142346, 125749602, 220603000, 135891448, 96182908, 225499000, 146348851, 131240418, 229690000, 137354620, 96010420, 235248000, 153146448, 132915120, 239874000, 142245282, 92351490, 245502000, 157612284, 137481120, 249748000, 153095524, 122376520)
)

library(lattice)
options(scipen = 5)
colors <- c("#000249","#0F4392","#FF4949")
barchart(Number_of_Occurrences~Year, col = colors, xlab = "Year", ylab = "Total People", main = "General VAP vs. Voting vs. Registration for All Age Groups", data=df, groups=Values, scales=list(x=list(rot=90,cex=0.8))) 

# Graph for General VAP vs Voting vs Registration for 18-24 year olds
df <- data.frame(
  Values = rep(c("Voting Age Population", "Amount Registered", "Amount Voted"), 2),
  Year = c(rep("2000", 3), rep("2002", 3), rep("2004", 3), rep("2006", 3), rep("2008", 3), rep("2010", 3), rep("2012", 3), rep("2014", 3), rep("2016", 3), rep("2018", 3)),
  Number_of_Occurrences = c(26712000, 12127248,	8627976, 27377000, 10458014,	4708844, 27808000, 14321120,	11651552, 27774000,	11553984,	5527026, 28263000,	15092442,	12520509, 28924000,	12090232,	5669104, 29878000,	14759732,	11353640, 29658000,	11596278,	4715622, 29320000,	14894560,	11552080, 28993000,	13249801,	8726893)
)

library(lattice)
options(scipen = 5)
colors <- c("#000249","#0F4392","#FF4949")
barchart(Number_of_Occurrences~Year, col = colors, xlab = "Year", ylab = "Total 18 to 24 Year Olds", main = "General VAP vs. Voting vs. Registration for 18-24 Year Olds", data=df, groups=Values, scales=list(x=list(rot=90,cex=0.8)))