library(tidyverse)
library(lme4)

setwd("")
crpp = read.csv("crpp.csv", header = TRUE, stringsAsFactors = TRUE)
crpp = as_tibble(crpp)

# Remove exclusions and select only the relevant columns.
crpp = dplyr::filter(crpp, crpp$Excluded..Y.N. == "N")
crpp_s = select(crpp, Subject.ID, Chosen.rule, Sex, Decimal.Age)

# Simplify to J/B
crpp_s$Chosen.rule = dplyr::recode(crpp_s$Chosen.rule, "Beth/Ben" = "B", "Jane/Jim" = "J")

# Logistic regression
crpp.glm = glm(Chosen.rule ~ 1+Decimal.Age, crpp_s, family = "binomial")
summary(crpp.glm)

# Plot the data points and prediction curve.
crpp_s$Chosen.rule = dplyr::recode(crpp_s$Chosen.rule, "B" = 0, "J" = 1)
plot(crpp_s$Decimal.Age, crpp_s$Chosen.rule, xlab="Age", ylab="Probability of choosing J rule")
curve(predict(crpp.glm,data.frame(Decimal.Age=x), type="resp"),add=TRUE)
