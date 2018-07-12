
#Model #1: Multiple Linear Regression modeling MPG using all the numerical covariates
model.1 <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + gear + carb, 
              data = df)
summary(model.1)

#Looking at the Residual Plot for Model #1
plot(model.1)

#Model #2: Simple Linear Regression modeling MPG using the weight of the car
model.2 <- lm(mpg ~ wt, data = df)
summary(model.2)

#Looking at the Residual Plot for Model #2
plot(model.2)



#Create the Principle Components
df.pca <- prcomp(~cyl + disp + hp + drat + wt + qsec + gear + carb,
                 data = df,
                 center = TRUE,
                 scale. = TRUE)
print(df.pca)
summary(df.pca)

#Scree Plot, Looks Like I can use 3 or 4 PC's to decribe the data
plot(df.pca, type = "l")

#Model #3 - Model MPG using the first 3 PC's
model.3 <- lm (df$mpg ~ df.pca$x[,1:3])
summary(model.3)
plot(model.3)

pca.df <- cbind(df$mpg, df.pca$x[,1:3])
corrplot(cor(pca.df))

#Model #4 - Model MPG using only the first PC
model.4 <- lm(df$mpg ~ df.pca$x[,1])
summary(model.4)
plot(model.4)