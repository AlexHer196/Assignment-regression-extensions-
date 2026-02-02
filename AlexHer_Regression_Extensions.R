#Load necessary libraries
library("readxl")
library("ggplot2")
library("tidyverse")


#Load the dataset
wages <- read_excel("C:/Users/alexh/OneDrive/School/Hamline courses/Business analytics/Advance_Bussiness_Analytics/assignment 2 regression extensions/wages.xlsx")
AnnArbor <- read_excel("C:/Users/alexh/OneDrive/School/Hamline courses/Business analytics/Advance_Bussiness_Analytics/assignment 2 regression extensions/AnnArbor.xlsx")

#View data sets
View(wages)
View(AnnArbor)

#View Initial Summary Statistics
summary(wages)
summary(AnnArbor)

# Plot Wage against Age
# and evaluate whether a linear or quadratic 
# model would better capture the relationship. 

ggplot(wages, aes(x = Age, y = Wage)) +
  geom_point(size=2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  labs(
    title = "Wage vs Age",
    subtitle = "Blue = Linear, Red = Quadratic",
    x = "Age",
    y = "Hourly Wage ($)" )

# The Quadratic Model better captures the relationship between Wages & Age

# Estimate a multiple regression model of Wage using Age and Education as independent 
# (X) variables; assume a standard linear relationship between Wage and Age.

Linear_Model <- lm(Wage ~ Age + Educ, data = wages)
summary(Linear_Model)

# Estimate another multiple regression model of Wage using Age and Education as independent (X) variables; 
# this time fit Age using a quadratic relationship. Verify your choice from part a. 
# by comparing the distribution of residuals and the goodness of fit between the models in parts b and c.

Quadratic_Model <- lm(Wage ~ Age + I(Age^2) + Educ, data = wages)
summary(Quadratic_Model)

par(mfrow = c(1, 2))
hist(residuals(Linear_Model), main = "Linear Model Residuals", xlab = "Residuals")
hist(residuals(Quadratic_Model), main = "Quadratic Model Residuals", xlab = "Residuals")
par(mfrow = c(1, 1))

summary(Linear_Model)$adj.r.squared
summary(Quadratic_Model)$adj.r.squared

AIC(Linear_Model, Quadratic_Model)

# The Quadratic Model is the better fitted model for the relationship between Wages ~ Ages + Education

# Use the appropriate model to predict hourly wages for someone with 16 years 
# of education and age equal 30, 50, or 70.

Ages_model <- data.frame(
  Age = c(30, 50, 70),
  Educ = 16)

summary(Ages_model)
view(Ages_model)

# According to the model, 
# at what age will someone with 16 years of education attain the highest wages?

predict(Quadratic_Model, newdata = Ages_model, interval = "confidence")

beta1 <- coef(Quadratic_Model)["Age"]
beta2 <- coef(Quadratic_Model)["I(Age^2)"]

max_wage_age <- -beta1 / (2 * beta2)
max_wage_age

### According to the quadratic model, wages peak at approximately 50.7 years of age

### AnnArbor
# Plot Rent against each of the three predictor variables and evaluate whether the relationship 
# is best captured by a line or a curve. Identify variables that may benefit from a log-transformation.

ggplot(AnnArbor, aes(x = Beds, y = Rent)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Rent vs Beds")

ggplot(AnnArbor, aes(x = Baths, y = Rent)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Rent vs Baths")

ggplot(AnnArbor, aes(x = Sqft, y = Rent)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Rent vs Square Footage")

AnnArbor$logRent <- log(AnnArbor$Rent) #log Rent
AnnArbor$logSqft <- log(AnnArbor$Sqft) #log Sqft

New_Rent_Model <- lm(logRent ~ logSqft + Beds + Baths, data = AnnArbor)
summary(New_Rent_Model)

# Estimate a multiple regression model (with any appropriate log-transformations) 
# to predict rent for a 1,600-square-foot rental with 3 bedrooms and 2 bathrooms.

house_rental <- data.frame(
  logSqft = log(1600),
  Beds = 3,
  Baths = 2)

predicted_log_rent <- predict(New_Rent_Model, newdata = house_rental)
predicted_rent <- exp(predicted_log_rent)

predicted_rent # $1486.24

