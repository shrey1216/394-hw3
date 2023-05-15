#1
#(a) Use the R script to make a scatter plot of the advertising expenses by the company sales.
# Does the data appear linear?
expenses_sales <- matrix(c(2.4, 1.6, 2.0, 2.6, 1.4, 1.6, 2.0, 2.2, 225, 184, 220, 240, 180, 184, 186, 215), nrow = 2, byrow = TRUE)
rownames(expenses_sales) <- c("Advertising expenses ($ in thousands) x", "Company sales ($ in thousands) y")
expenses_sales

x <- expenses_sales[1,]
y <- expenses_sales[2,]

plot(x, y)
# The data does appear linear, as the points on the scatterplot follow a somewhat straight line pattern.

#(b) Use the R script to find the Pearson correlation coefficient and Spearman’s ρ
# between the advertising expenses and the company sales.
#Pearson correlation coefficient
cor(x, y)
#The correlation coefficient is 0.9129053

#Spearman's ρ
cor.test(x, y, method = "spearman")
#Spearman’s ρ is 0.9575933

#(c) Use the R script to check whether the linear correlation is significantly different
#from zero at the level of 5% using the Pearson correlation coefficient and Spearman’s ρ.
#What is your conclusion?
cor.test(x, y)
cor.test(x, y, method = "spearman")
#The p-value in the Spearman's ρ test is 0.001846. The p-value is 0.001546 
#in the Pearson test. Both p-values are less than 0.05, which means that they are statistically 
#significant. They indicate strong evidence against the null hypothesis, so we reject it and 
#accept the alternative hypothesis that true correlation and true rho are not equal to 
#zero or are significantly different from zero at the level of 5%. 

#(d) Use the R script to fit a least squares regression line. Write out the fitted model
#explicitly.
lm(y ~ x)
#y = 104.06 + 50.73 * x

#(e) Use the R script to find the coefficient of determination of the regression model
summary(lm(y ~ x))$r.squared
#The coefficient of determination of the regression model is 0.8333961. This can also be found
#by squaring the Pearson correlation coefficient from earlier, which yields the same result.

#(f) Test if the coefficient of advertising expenses is zero in the regression model at
#the 5% level. What is your conclusion?
summary(lm(y ~ x))
# The summary gives a p-value of 0.001546. This p-value is much less than 0.05, which means
#we can reject the null hypothesis that the coefficient of advertising expenses is zero at
#the 5% level. 

#(g)Use the R script to make a scatter plot of the residuals versus the fitted values
#of the regression model.
lm.expenses_sales = lm(y ~ x)
plot(fitted(lm.expenses_sales), resid(lm.expenses_sales))
abline(0, 0)

#(h) Use the R script to predict the expected company sales if the company plans
#to spend $1800 on advertising.
new <- data.frame(x=c(1800))
predict(lm.expenses_sales, newdata = new)
#The expected company sales is about $91415.8

#(i) Use the R script to find the confidence intervals and the prediction intervals
#for all predicted values based on the regression model.
predict(lm.expenses_sales, interval = "confidence")
predict(lm.expenses_sales, interval = "prediction")

#(j) Use the R script to make a scatter plot of the advertising expenses by the
#company sales again with the fitted line of the regression model, the confidence bands,
#and the prediction bands on the plot.
pred.frame <- data.frame(x = 1:3)
pc <- predict(lm.expenses_sales, interval = "confidence", newdata = pred.frame)
pp <- predict(lm.expenses_sales, interval = "prediction", newdata = pred.frame)
plot(x, y, ylim = range(y, pp, na.rm = TRUE))
pred.x <- pred.frame$x
#confidence bands
matlines(pred.x, pc, lty = c(1, 2, 2), col = "black")
#prediction bands
matlines(pred.x, pp, lty = c(1, 3, 3), col = "black")

#2
#(a) Use the R script to regress the log return of Intel on the log return of Citigroup
#(with the intercept). Write out the fitted model explicitly.
stocks <- read.table("/Users/shreyanwankavala/Desktop/AMS\ 394\ /d_logret_6stocks.txt", header = TRUE)
lm(stocks$Intel ~ stocks$Citigroup)
#stocks$Intel = -0.00716 + 1.25429 * stocks$Citigroup

#(b) Use the R script to regress the log return of Intel on the log return of Citigroup
#(without the intercept). Write out the fitted model explicitly.
lm(stocks$Intel ~ 0 + stocks$Citigroup)
#stocks$Intel = 1.247 * stocks$Citigroup

#(c) Use the R script to find the Pearson correlation coefficient and Kendall’s τ of
#log returns for Intel and Citigroup.
#Pearson correlation coefficient
cor(stocks$Citigroup, stocks$Intel)
#Kendall's τ
cor.test(stocks$Citigroup, stocks$Intel, method = "kendall")

#(d) Use the R script to test if the correlation of log returns between Intel and
#Citigroup is zero at the 5% level using the Pearson correlation coefficient and Kendall’s τ .
#What is your conclusion?
cor.test(stocks$Citigroup, stocks$Intel)
cor.test(stocks$Citigroup, stocks$Intel, method = "kendall")
#The p-value in the Kendall's τ test is 1.32e-05. The p-value i s7.085e-07 
#in the Pearson test. Both p-values are less than 0.05, which means that they are statistically 
#significant. They indicate strong evidence against the null hypothesis, so we reject it and 
#accept the alternative hypothesis that true correlation and true tau are not equal to 
#zero or are significantly different from zero at the level of 5%. 


#(e) Use the R script to regress the log return of Intel on the log return of Citigroup,
#the log return of Pfizer, the log return of AmerExp, the log return of Exxon, and the log
#return of GenMotor (with the intercept). Write out the fitted model explicitly.
lm(stocks$Intel ~ stocks$Citigroup + stocks$Pfizer + stocks$AmerExp + stocks$Exxon + stocks$GenMotor)
#stocks$Intel = -0.004878 + 1.005380 * stocks$Citigroup - 0.256907 * stocks$Pfizer 
#+ 0.428527 * stocks$AmerExp - 0.512211 * stocks$Exxon + 0.346642 * stocks$GenMotor

#(f) Use the R script to find the adjusted coefficient of determination of the regression
#model in part (e).
summary(lm(stocks$Intel ~ stocks$Citigroup + stocks$Pfizer + stocks$AmerExp + stocks$Exxon + stocks$GenMotor))$r.squared
#The coefficient of determination of the regression model is 0.4389347.

#(g) Use the R script to perform a model search of the regression model in part
#(e) using the Akaike information criterion and the bidirectional selection. Write out the
#preferred model explicitly
step(lm(stocks$Intel ~ stocks$Citigroup + stocks$Pfizer + stocks$AmerExp + stocks$Exxon + stocks$GenMotor), direction = "both")
#The preferred model is the one with the minimum AIC value. After performing a model search, there is a minumum
#AIC value of -371.69 which is less than the starting AIC value of -370.4. This AIC value is reached 
#with the preferred model stocks$Intel ~ stocks$Citigroup + stocks$AmerExp + stocks$Exxon + stocks$GenMotor,
#where stocks#Pfizer is omitted. 

#(h) Use the R script to perform a model search of the regression model in part (e)
#using the Akaike information criterion and the forward selection. Write out the preferred
#model explicitly. Is the regression model obtained here the same as the one obtained in
#part (g)?
step(lm(stocks$Intel ~ stocks$Citigroup + stocks$Pfizer + stocks$AmerExp + stocks$Exxon + stocks$GenMotor), direction = "forward")
#The preferred model is the one with the minimum AIC value. After performing a model search, there is a minumum
#AIC value of -370.4. This AIC value is reached with the preferred model stocks$Intel ~ stocks$Citigroup + stocks$Pfizer + stocks$AmerExp + 
#stocks$Exxon + stocks$GenMotor. This model is different from the one acquired using bidirectional selection in that
#it still has stocks$Pfizer. The AIC value is also higher. 

