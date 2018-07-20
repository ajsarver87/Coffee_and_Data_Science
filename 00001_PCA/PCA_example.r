#import libraries used and the mtcars data set
library(corrplot)
library(ggfortify)
library(plot3D)
library(plot3Drgl)
df <- mtcars

#Looking at a Correlation Table for our data set
corrplot(cor(df))

#Create the Principle Components(note we are only looking at the numerical data)
df.pca <- prcomp(~ mpg + cyl + disp + hp + drat + wt + qsec + gear + carb,
                 data = df,
                 center = TRUE,
                 scale. = TRUE)
#Will print the standard deviations and the loadings(coefficents) of each PC
print(df.pca)

#Will display the standard deviations, proportion of variance, and cumulative proportion of each 
#Pc
summary(df.pca)

#Scree Plot, Looks Like I can use 3 or 4 PC's to decribe the data, instead of all 9 variable
plot(df.pca, type = "l")

#plot the data according to the first two PCs, explains abotu 85% of the data
autoplot(df.pca, label = TRUE, scale = 0)

#Apply the same ideas to the Credit Card Fraud data Set
credit <- read.csv(file = "00001_PCA/creditcard.csv",
                   header = TRUE)

credit.pca <- prcomp( ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28,
                      data = credit)
#This double checks to see that the scores are unchanged from the orginal data frame
#and the prcomp object (They are numberically the same, just some of the characteristics are
#different)
all.equal(credit[,2:29],as.data.frame(credit.pca$x))

print(credit.pca)
summary(credit.pca)

screeplot(credit.pca, npcs = 28, type = c("lines"))

autoplot(credit.pca, scale=0, data = credit, colour = 'Class')

scatter3D(credit.pca$x[,1], credit.pca$x[,2], credit.pca$x[,3], colvar = credit$Class )
plotrgl()
