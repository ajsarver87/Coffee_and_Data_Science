#import libraries used and the mtcars data set
library(corrplot)
library(ggfortify)
df <- mtcars

#Looking at a Correlation Table for our data set
corrplot(cor(df))

#Create the Principle Components(note we are only looking at the numerical data)
df.pca <- prcomp(~ mpg + cyl + disp + hp + drat + wt + qsec + gear + carb,
                 data = df,
                 center = TRUE,
                 scale. = TRUE)
print(df.pca)
summary(df.pca)

#Scree Plot, Looks Like I can use 3 or 4 PC's to decribe the data, instead of all 9 variable
plot(df.pca, type = "l")

#plot the data according to the first two PCs, explains abotu 85% of the data
autoplot(df.pca, label = TRUE, scale = 0)