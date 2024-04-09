library(readxl)
library(car)
library(dplyr)
library(jtools)
library(xts)
library(PerformanceAnalytics)


#1 - read the data
dati <- read_excel("G:/Il mio Drive/MAGISTRALE/Finance/Lab Sessions/First_Assignment_dataset.xlsx")
dati <- dati[,1:17]
head(dati)

dati$month <- as.Date(paste(gsub("m","-",dati$month), "01", sep="-"))
str(dati)



#2 - create returns and excesses from initial data;
#    stocks have been assigned to portfolios based on their market capitalization.
#    The smallest stocks are in S1 and the largest stocks are in S10.
returns <- dati[,8:17]
head(returns)

excess <- returns-dati$rf
names(excess) <- c("XS1", "XS2", "XS3", "XS4", "XS5", "XS6", "XS7", "XS8", "XS9", "XS10")
head(excess)

r <- xts(x=returns, order.by=dati$month)
e <- xts(x=excess, order.by=dati$month)



#3 - Inspect the returns for each portfolio and comment on any exceptional features
boxplot(excess , ylab="Values of the excess returns",
        main="Boxplots of the excess returns", col="white")

cor(excess)

par(mfrow=c(5, 2))
#open up full plot window
plot(dati$month, excess$XS1, type="l", xlab = "time", ylab = "XS1")
plot(dati$month, excess$XS2, type="l", xlab = "time", ylab = "XS2")
plot(dati$month, excess$XS3, type="l", xlab = "time", ylab = "XS3")
plot(dati$month, excess$XS4, type="l", xlab = "time", ylab = "XS4")
plot(dati$month, excess$XS5, type="l", xlab = "time", ylab = "XS5")
plot(dati$month, excess$XS6, type="l", xlab = "time", ylab = "XS6")
plot(dati$month, excess$XS7, type="l", xlab = "time", ylab = "XS7")
plot(dati$month, excess$XS8, type="l", xlab = "time", ylab = "XS8")
plot(dati$month, excess$XS9, type="l", xlab = "time", ylab = "XS9")
plot(dati$month, excess$XS10, type="l", xlab = "time", ylab = "XS10")

#cumulated excess returns
attach(excess)
cbind(prod(1+XS1)-1, prod(1+XS2)-1, prod(1+XS3)-1, prod(1+XS4)-1, prod(1+XS5)-1, prod(1+XS6)-1, 
      prod(1+XS7)-1,prod(1+XS8)-1,prod(1+XS9)-1,prod(1+XS10)-1)

#Cumulative excess returns plot
par(mfrow=c(1,1))
chart.CumReturns(e, main="Cumulative excess returns", legend.loc = TRUE, lwd = 2)
title(ylab = "Excess return, %")



#4 - Compute summary statistics for the 10 excess return series.
#    Comment on the variation in statistics across the portfolios and the implications of this variation.
summary(excess)
NROW(excess)



#5 - Run a regression of the excess return on a constant term, RMRF, SMB, HML, and UMD;
#    Interpret the regression coefficients, their t-ratios or p-values;
#    Store the slope coefficients form these regressions in an Excel file;
#    Comment on the variations in risk exposures across factors and how they might be interpreted;
#    Also interpret the R2 from these regressions.
attach(dati)
cor(excess)
cor(cbind(excess, dati[,2:7]))

regxs1 <- lm(XS1 ~ rmrf + smb + hml + umd)
#regxs1 <- lm(XS1 ~ rmrf + smb)
summary(regxs1)
#summ(regxs1, confint = TRUE, digits = 4) #-> same output but with confidence intervals and maybe more graphically pleasing
avPlots(regxs1, grid=TRUE, ylab="Portfolio 1", lwd=2, cex=1.2, pch=20, main="", col="blue", col.lines="red", )
plot(y=regxs1$residuals, x=regxs1$fitted.values)
abline(h=0, col="blue")
#rmrf*** + smb*** + hml. + umd.


regxs2 <- lm(XS2 ~ rmrf + smb + hml + umd)
#regxs2 <- lm(XS2 ~ rmrf + smb)
summary(regxs2)
avPlots(regxs2, grid=FALSE)
plot(regxs2$residuals, x=regxs2$fitted.values)
abline(h=0, col="blue")
#rmrf*** + smb***


regxs3 <- lm(XS3 ~ rmrf + smb + hml + umd)
#regxs3 <- lm(XS3 ~ rmrf + smb)
summary(regxs3)
avPlots(regxs3, grid=FALSE)
plot(regxs3$residuals, x=regxs3$fitted.values)
abline(h=0, col="blue")
#rmrf*** + smb*** + hml.


regxs4 <- lm(XS4 ~ rmrf + smb + hml + umd)
summary(regxs4)
avPlots(regxs4, grid=TRUE, ylab="Portfolio 4", lwd=2, cex=1.2, pch=20, main="", col="blue", col.lines="red", )
plot(regxs4$residuals, x=regxs4$fitted.values)
abline(h=0, col="blue")
#rmrf*** + smb*** + hml** + umd*


regxs5 <- lm(XS5 ~ rmrf + smb + hml + umd)
#regxs5 <- lm(XS5 ~ rmrf + smb)
summary(regxs5)
avPlots(regxs5, grid=FALSE)
plot(regxs5$residuals, x=regxs5$fitted.values)
abline(h=0, col="blue")
#rmrf*** + smb*** + umd.


regxs6 <- lm(XS6 ~ rmrf + smb + hml + umd)
#regxs6 <- lm(XS6 ~ rmrf + smb)
summary(regxs6)
avPlots(regxs6, grid=FALSE)
plot(regxs6$residuals, x=regxs6$fitted.values)
abline(h=0, col="blue")
#rmrf*** + smb***


regxs7 <- lm(XS7 ~ rmrf + smb + hml + umd)
#regxs7 <- lm(XS7 ~ rmrf + smb)
summary(regxs7)
avPlots(regxs7, grid=FALSE)
plot(regxs7$residuals, x=regxs7$fitted.values)
abline(h=0, col="blue")
#rmrf*** + smb***


regxs8 <- lm(XS8 ~ rmrf + smb + hml + umd)
#regxs8 <- lm(XS8 ~ rmrf + smb + umd)
summary(regxs8)
avPlots(regxs8, grid=FALSE)
plot(regxs8$residuals, x=regxs8$fitted.values)
abline(h=0, col="blue")
#rmrf*** + smb*** + umd*


regxs9 <- lm(XS9 ~ rmrf + smb + hml + umd)
#regxs9 <- lm(XS9 ~ rmrf + smb + umd)
summary(regxs9)
avPlots(regxs9, grid=FALSE)
plot(regxs9$residuals, x=regxs9$fitted.values)
abline(h=0, col="blue")
#rmrf*** + smb*** + umd*


regxs10<- lm(XS10 ~ rmrf + smb + hml + umd)
#regxs10 <- lm(XS10 ~ rmrf + smb + umd)
summary(regxs10)
avPlots(regxs10, grid=TRUE, ylab="Portfolio 10", lwd=2, cex=1.2, pch=20, main="", col="blue", col.lines="red", )
plot(regxs10$residuals, x=regxs10$fitted.values)
abline(h=0, col="blue")
#rmrf*** + smb*** + umd***



#6 - Do risk exposures explain the way in which mean returns vary across portfolios?
#    Run a series of cross section regressions. The y-variable is the set of 10 mean returns on the
#    ten size-based portfolios and the x-variables are a constant and two sets of risk exposures from
#    your time series regressions; those on RMRF and one other. In turn, the other risk factor 
#    will be SMB, HML and UMD. Thus, in each regression there are 10 observations and two explanatory
#    variables plus a constant term, and interpret your results.
mean_returns <- c(mean(XS1), mean(XS2), mean(XS3), mean(XS4), mean(XS5), mean(XS6), mean(XS7), mean(XS8), mean(XS9), mean(XS10))
mean_returns

df_beta <- rbind(round(regxs1$coefficients,6),round(regxs2$coefficients,6),round(regxs3$coefficients,6),round(regxs4$coefficients,6),round(regxs5$coefficients,6),round(regxs6$coefficients,6),round(regxs7$coefficients,6),round(regxs8$coefficients,6),round(regxs9$coefficients,6),round(regxs10$coefficients,6))
df_beta <- as.data.frame(df_beta)
df_beta

#correlation matrix
cor(cbind(mean_returns, df_beta[,2:5]))

mod1 <- lm(mean_returns ~ df_beta$rmrf + df_beta$smb)
summary(mod1)
avPlots(mod1, grid=FALSE)

mod2 <- lm(mean_returns ~ df_beta$rmrf + df_beta$hml)
summary(mod2)
avPlots(mod2, grid=FALSE)

mod3 <- lm(mean_returns ~ df_beta$rmrf + df_beta$umd)
summary(mod3)
avPlots(mod3, grid=FALSE)



