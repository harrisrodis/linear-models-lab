td <- data.frame(read.csv("test1_data.csv", sep = ",", header = T))

# Q1
# Get names of all possible pairs of td variables
n.pairs <- ncol(combn(colnames(td), 2))
td.names_of_pairs <- vector(length = n.pairs)
for (i in 1:n.pairs) {
  td.names_of_pairs[i] <- paste(combn(colnames(td), 2)[1,i], combn(colnames(td), 2)[2,i], sep = "~")
}

# Calculate correlation coefficients and their p-values for all possible pairs of td variables
td.cor <- data.frame(matrix(nrow = n.pairs, ncol = 2, dimnames = list(td.names_of_pairs, c("cor", "p-value"))))
for (i in 1:n.pairs) {
  td.cor[i, 1] <- cor(td[,combn(1:4, 2)[1,i]], td[,combn(1:4, 2)[2,i]])
  td.cor[i, 2] <- cor.test(td[,combn(1:4, 2)[1,i]], td[,combn(1:4, 2)[2,i]])$p.value
}


# Q2
# Linear model for expected value of cholesterol given glukos level
td.model1 <- lm(cholesterol ~ glukos, td)
summary(td.model1)
anova(td.model1)


# Q3
# Scatterplot of cholesterol and glukos levels
plot(td$cholesterol ~ td$glukos, 
     pch = 16,
     bty = "n",
     col = "#3F6797",
     xlab = "Επίπεδο Γλυκόζης",
     ylab = "Επίπεδο Χοληστερόλης",
     main = "Διάγραμμα διασποράς"
)


# Q4
# C.I. for slope and intercept of model1
confint(td.model1, level = 0.95)


# Q5 - Q6
# C.I. and P.I. for expected value of cholesterol, given glukos level at 110
predict(td.model1, data.frame(glukos = 110), interval = "confidence", level = 0.95)
predict(td.model1, data.frame(glukos = 110), interval = "prediction", level = 0.95)


# Q7
# Linear model for expected value of cholesterol given glukos level and age
td.model2 <- lm(cholesterol ~ glukos + age, td)
summary(td.model2)
anova(td.model2)


# Q8
# C.I. for slope and intercept of model2
confint(td.model2, level = 0.95)


# Q9 - Q10
# C.I. and P.I. for expected value of cholesterol, given glukos level at 110 and age 60
predict(td.model2, data.frame(glukos = 110, age = 60), interval = "confidence", level = 0.95)
predict(td.model2, data.frame(glukos = 110, age = 60), interval = "prediction", level = 0.95)


# Q11
# Linear model for expected value of cholesterol given glukos level, age and bmi
td.model3 <- lm(cholesterol ~ glukos + age + bmi, td)
summary(td.model3)
anova(td.model3)


# Q12
# C.I. for slope and intercept of model3
confint(td.model3, level = 0.95)


# Q13 - Q14
# C.I. and P.I. for expected value of cholesterol, given glukos level at 110, age 60 and bmi 23
predict(td.model3, data.frame(glukos = 110, age = 60, bmi = 23), interval = "confidence", level = 0.95)
predict(td.model3, data.frame(glukos = 110, age = 60, bmi = 23), interval = "prediction", level = 0.95)
