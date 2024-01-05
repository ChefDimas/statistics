library(readxl)
data <- read_excel("./data/Moving.xlsx")
model <- lm(Hours ~ Feet, data = data)
print(summary(model))


plot(data$Hours, data$Feet)
abline(model, col = "red")
plot(model)


new_data <- data.frame(Feet = 800)
predictions <- predict(model, new_data)
print(predictions)
