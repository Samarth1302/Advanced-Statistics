getwd()
setwd("F:/USERS/DOWNLOADS")
data=read.csv("Mat502 - Dataset.csv")
data

library(ggplot2)
##########################################################################################################################
#Chi-square test between category of activities and grade relation

x <- seq(0, 30, length.out = 1000)

# Set significance level
alpha <- 0.05

#table 
Chisq_table <- table(data$Type, data$Academic_Performance)
print(Chisq_table)

#chi-square test
chisqTest <- chisq.test(Chisq_table)
print(chisqTest)

#degree of freedom
df <- chisqTest$parameter

critical_value <- qchisq(1 - alpha, df)
cat("Critical value: ",critical_value)

y <- dchisq(x, df)
table <- table(data$Type, data$Academic_Performance)
barplot(table, beside = TRUE, xlab="Performance",ylab="Frequency", legend.text = TRUE, col = c("lightblue", "pink", "lightgreen"))

#chi-square Distribution
ggplot(data.frame(x, y), aes(x = x, y = y)) +
  geom_line(size = 1, color = "blue") +
  scale_x_continuous(limits = c(0, 30)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  geom_vline(xintercept = critical_value, linetype = "dashed", color = "orange") +
  geom_vline(xintercept = chisqTest$statistic, linetype = "dashed", color = "red") +
  labs(x = "Chi-square value", y = "Density", title = paste0("Chi-square Distribution with df = ", df)) +
  theme_minimal()+
  theme(legend.position = "top") +
  annotate("text", x = critical_value, y = 0.3, angle = 90, label = "Critical Value", vjust = -1.5, color = "orange") +
  annotate("text", x = chisqTest$statistic, y = 0.3,angle = 90, label = "X-squared value", vjust = 1.5, color = "red")

######################################################################################################################################################################################

# Z-test between CGPA and Hours spent on extra curricular activities

CGPA<- data$CGPA
hours_spent <- data$Hours

#histogram 
hours_hist <- hist(hours_spent,main="Hours spent on extra curricular activities", xlab = "Hours spent", ylab = "Frequency", breaks = 5,col="lightblue")
print(hours_hist$counts)

# Fit a linear regression line
fit <- lm(CGPA ~ `Hours`, data)

# Plot the data points and regression line
ggplot(data, aes(x=`Hours`, y=CGPA)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  xlab("Time spent on extracurricular activities") +
  ylab("CGPA") +
  ggtitle("Relationship between CGPA and time spent on extracurricular activities")

# Calculate Pearson correlation coefficient and p-value
cor_res <- cor.test(data$CGPA, data$Hours)
r <- cor_res$estimate
p_value <- cor_res$p.value

# Calculate z-statistic and critical value
z <- 0.5 * log((1+r)/(1-r))
critical_value <- qnorm(1 - alpha/2)


# Test the null hypothesis
if (abs(z) > critical_value) {
  print("Reject null hypothesis")
} else {
  print("Accept null hypothesis")
  print(paste("P-value: ", p_value))
}

cat("z-Critical value: ",critical_value,"\n")
cat("z-Statistic value: ",z)

curve(dnorm(x, mean = 0, sd = 1), xlim = c(-3, 3), ylim = c(0, 0.5), xlab = "Z values", ylab = "Probability Density", main = "Z-Distribution for Pearson's Correlation Coefficient")
abline(v = c(-critical_value, critical_value), lty = 2, col = "red")
abline(v = z, lty = 1, col = "blue")
legend("topright", legend = c("Z-value", "Critical Values"), lty = c(1, 2), col = c("blue", "red"))

#########################################################################################################################

