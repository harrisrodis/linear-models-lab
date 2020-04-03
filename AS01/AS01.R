#Load data
india_foot_height <- read.table("india_foot_height.dat")
names(india_foot_height)[1:2] <- c("Foot", "Height")
attach(india_foot_height)

#Convert cm to inch
india_foot_height$Foot_inch <- Foot * 0.393701
india_foot_height$Height_inch <- Height * 0.393701

#Convert foot to ln(foot) and height to height^2
india_foot_height$ln_Foot <- log(Foot)
india_foot_height$Height_squared <- Height^2

detach(india_foot_height)
attach(india_foot_height)

#Plot data
plot(Foot, Height, pch = 16, bty = "n", col = "#f6b26b")

#Plot and compare all possible combinations of units
par(mfrow=c(2,2), pch = 16, bty = "n", col = "#f6b26b")
plot(Foot, Height, main = paste("r =", round(cor(Foot, Height), 6)))
plot(Foot, Height_inch, main = paste("r =", round(cor(Foot, Height_inch), 6)))
plot(Foot_inch, Height, main = paste("r =", round(cor(Foot_inch, Height), 6)))
plot(Foot_inch, Height_inch, main = paste("r =", round(cor(Foot_inch, Height_inch), 6)))

#Invert axes of scatterplot and compare r
par(mfrow=c(1,2), pch = 16, bty = "n", col = "#f6b26b")
plot(Foot, Height, main = paste("r =", round(cor(Foot, Height), 6)))
plot(Height, Foot, main = paste("r =", round(cor(Height, Foot), 6)))

#Plot and compare all combinations of non-linear transformations
par(mfrow=c(2,2), pch = 16, bty = "n", col = "#f6b26b")
plot(Foot, Height, main = paste("r =", round(cor(Foot, Height), 6)))
plot(Foot, Height_squared, main = paste("r =", round(cor(Foot, Height_squared), 6)))
plot(ln_Foot, Height, main = paste("r =", round(cor(ln_Foot, Height), 6)))
plot(ln_Foot, Height_squared, main = paste("r =", round(cor(ln_Foot, Height_squared), 6)))

#Calculate r and evaluate its statistical significance
cor.test(Foot, Height)
