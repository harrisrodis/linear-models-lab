# Dependencies
library(faraway)
library(car)
library(ggplot2)

simple.graph <- theme(
    panel.background = element_rect(fill = "#ffffff"),
    panel.grid.major = element_line(colour = "#e9e9e9", size = 0.2),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = .4, colour = "#222222"),
    axis.title.x = element_text(face = "bold", margin = margin(15, 0, 0, 0)),
    axis.title.y = element_text(face = "bold", margin = margin(0, 15, 0, 0)),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Load dataset
dt <- savings

# Simulate random linear dataset
set.seed(101)
x <- runif(50, 0, 100)
y <- runif(1, 0, 10) * x + rnorm(50, 0, 10)
ls <- data.frame(x = x, y = y)

# 1. Linearity
graph.1.1 <- ggplot(dt, aes(dpi, sr)) +
  geom_point() +
  labs(
    x = "Κατά κεφαλήν διαθέσιμο εισόδημα ($)", 
    y = "Λόγος αποταμιεύσεων προς διαθέσιμο εισόδημα"
  ) + simple.graph

graph.1.2 <- graph.1.1 + stat_function(fun = function(x) 10 * sin(0.0008 * x) + 4, col = "#e69138")

graph.1.3 <- graph.1.1 + geom_smooth(method = "lm", se = F, col = "#e08e79", size = .5)

graph.1.4 <- ggplot(ls, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, col = "#c5e0dc", size = .5) +
  labs(
    x = "Ανεξάρτητη μεταβλητή Χ", 
    y = "Εξαρτημένη μεταβλητή Υ"
  ) + simple.graph

# 3. Homoscedasticity
sr.model <- lm(sr ~ dpi, dt)
sr.model.dt <- data.frame(res = sr.model$residuals, fit = sr.model$fitted.values)
sr.model.dt$qnt <- cut(
  sr.model.dt$fit,
  unique(quantile(sr.model.dt$fit)),
  labels = c("1o τεταρτημόριο", "2o τεταρτημόριο", "3o τεταρτημόριο", "4o τεταρτημόριο"),
  include.lowest = T
)

ls.model <- lm(y ~ x, ls)
ls.model.dt <- data.frame(res = ls.model$residuals, fit = ls.model$fitted.values)

graph.3.1 <- ggplot(sr.model.dt, aes(fit, res)) +
  geom_hline(yintercept = 0, col = "grey", size = .4) +
  geom_point() +
  labs(
    x = "Προβλέψεις μοντέλου", 
    y = "Κατάλοιπα μοντέλου"
  ) + simple.graph

graph.3.2 <- graph.3.1 + 
  geom_vline(xintercept = quantile(sr.model.dt$fit)[2:4], col = "grey", linetype = "dashed") +
  geom_point(aes(col = qnt), size = 2) +
  scale_color_manual(values=c("#f6b26b", "#76a5af", "#c27ba0", "#e08e79"))

graph.3.3 <- ggplot(ls.model.dt, aes(fit, res)) +
  geom_hline(yintercept = 0, col = "grey", size = .4) +
  geom_point() +
  labs(
    x = "Προβλέψεις μοντέλου", 
    y = "Κατάλοιπα μοντέλου"
  ) + simple.graph

leveneTest(sr.model$residuals, sr.model.dt$qnt, center = median)

# 4. Normality
stud.res <- data.frame(res = sort(rstudent(sr.model)))
t.params <- list(df = sr.model$df.residual)

# simulate random samples of size n from t distribution with 48df
n <- length(sr.model$residuals)
M <- 1000
t.sim <- NULL
set.seed(1001)
for (i in 1:M) {
  t.sim <- cbind(t.sim, sort(rt(n, t.params[[1]])))
}

# construct pointwise 95% CI of simulated samples
t.upper <- apply(t.sim, 1, (function(x) quantile(x, prob = 0.975)))
t.lower <- apply(t.sim, 1, (function(x) quantile(x, prob = 0.025)))
stud.res <- cbind(stud.res, lower = t.lower, upper = t.upper)

# compute quantile of each residual
fti <- (1:n - 0.5) / n
t.scores <- qt(fti, df = t.params[[1]])
stud.res <- cbind(stud.res, tscores = t.scores)


graph.4.1 <- ggplot(sr.model.dt, aes(sample = res)) +
  geom_qq(distribution = qnorm) + 
  geom_qq_line(col = "#e08e79") +
  labs(
    x = "Ποσοστιαία σημεία κανονικής κατανομής [Ν(0,1)]", 
    y = "Ποσοστιαία σημεία καταλοίπων μοντέλου"
  ) + simple.graph

graph.4.2 <- ggplot(stud.res, aes(sample = res)) +
  geom_ribbon(aes(x = tscores, ymin = lower, ymax = upper), fill = "#c5e0dc", alpha = .5) +
  stat_qq_line(distribution = qt, dparams = t.params, col = "#76a5af") +
  stat_qq(distribution = qt, dparams = t.params) +
  labs(
    x = "Θεωρητική κατανομή t με 48 βαθμούς ελευθερίας", 
    y = "Τυποποιημένα κατά Student διαγραμμένα κατάλοιπα"
  ) + simple.graph


# 5. Infuential points
sr.cookd <- data.frame(cd = round(cooks.distance(sr.model), 4))
sr.cookd$countries <- rownames(sr.cookd)

graph.5.1 <- ggplot(sr.cookd, aes(x = countries, y = cd)) +
  geom_col(width = 0.2, fill = "#c5e0dc") + 
  geom_point(col = "#76a5af", size = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(
    x = "Χώρες", 
    y = "Απόσταση του Cook"
  ) + simple.graph
