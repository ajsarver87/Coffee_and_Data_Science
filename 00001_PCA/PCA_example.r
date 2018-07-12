df <- mtcars
head(df)

cor(df)

model.1 <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, 
              data = df)

summary(model.1)

df.pca <- prcomp(~cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb,
                 data = df,
                 center = TRUE,
                 scale. = TRUE)


print(df.pca)

plot(df.pca, type = "l")

summary(df.pca)

df.pca$x[,1:5]

model.2 <- lm (df$mpg ~ df.pca$x[,1:5])
summary(model.2)

model.3 <- lm (df$mpg ~ df.pca$x[,1])
summary(model.3)
