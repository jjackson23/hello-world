#This lecture Oct 18 

data("CO2")
#tell us the information about the variable
str(CO2)

fit <- lm(uptake ~ conc, data = CO2)
summary(fit)
str(fit)

fitted(fit)
residuals(fit)