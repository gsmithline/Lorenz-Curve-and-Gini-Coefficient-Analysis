library(readxl)
library(tidyverse)
library(ineq)
library(reshape2)
#original data from website 
df_1 = read_excel("lorenz.xlsx", skip = 2)
#Get get of data
head(df_1)
#get list of variables 
str(df_1)
#selection of years, for our analyssi we are doing 1980 and 2014 
year_select = c(1980, 2014)
country_select = c("Poland", "Norway")
#getting necessary data from the year 2014 and 1980 on Poland and Norway 
temp = subset(
  df_1, 
  (df_1$Country %in% country_select) & 
  (df_1$Year %in% year_select))
#Getting total incomes
print("Total incomes are:", total_income)total_income = temp[, "Mean Income"] * temp[, "Population"]  
print("Total incomes are:")
print(total_income)
# Mean Income
#1  47000972480
#2 111076271875
#3 161755660050
#4 222539813353
#These are very large number so for us it is easier to assume only one person is in each decline, meaning total income is 10 times the mean of the income 
# Pick the deciles (Columns 3 to 12) in Row 1 (Poland, 1980)
decs_p80 = unlist(temp[1, 3:12])

# The unlist function transforms temp[1, 3:12] from a 
# tibble to simple vector with data which simplifies the 
# calculations.

# Give the total income, assuming a population of 10
total_inc <- 10 * unlist(temp[1, "Mean Income"])

cum_inc_share_p80 = cumsum(decs_p80) / total_inc
print(cum_inc_share_p80)

#Decile 1 Income  Decile 2 Income  Decile 3 Income  Decile 4 Income  Decile 5 Income  Decile 6 Income  Decile 7 Income  Decile 8 Income  Decile 9 Income 
#0.02226182       0.05881433       0.10914465       0.17348748       0.25259910       0.34775730       0.46101356       0.59597531       0.76058762 
#Decile 10 Income 
#0.99998261 
# For Poland, 2014 go to Row 2 (Poland, 2014)
decs_p14 <- unlist(temp[2, 3:12])

# Give the total income, assuming a population of 10
total_inc <- 10 * unlist(temp[2, "Mean Income"]) 

cum_inc_share_p14 = cumsum(decs_p14) / total_inc  
print(cum_inc_share_p14)

#Decile 1 Income  Decile 2 Income  Decile 3 Income  Decile 4 Income  Decile 5 Income  Decile 6 Income  Decile 7 Income  Decile 8 Income  Decile 9 Income 
#0.03849711       0.09324393       0.15991214       0.23668902       0.32296416       0.41900116       0.52606243       0.64721387       0.79050173 
#Decile 10 Income 
#0.99999075 

#__________________________________________________________________________________________________________________________________________
#Norway 
# For the Norway, 1980  
# Select Row 3 (Norway, 1980)
decs_n80 <- unlist(temp[3, 3:12])

# Give the total income, assuming a population of 10
total_inc <- 10 * unlist(temp[3, "Mean Income"])

cum_inc_share_n80 = cumsum(decs_n80) / total_inc  
print(cum_inc_share_n80)

#Decile 1 Income  Decile 2 Income  Decile 3 Income  Decile 4 Income  Decile 5 Income  Decile 6 Income  Decile 7 Income  Decile 8 Income  Decile 9 Income 
#0.04391907       0.10061579       0.16762701       0.24391907       0.32929404       0.42425775       0.53030570       0.65067077       0.79353420 
#ecile 10 Income 
#0.99993402 

# For the Norway, 2014  
# Select Row 4 (Norway, 2014)
decs_n14 <- unlist(temp[4, 3:12])   

# Give the total income, assuming a population of 10 
total_inc <- 10 * unlist(temp[4, "Mean Income"])

cum_inc_share_n14 = cumsum(decs_n14) / total_inc 
print(cum_inc_share_n14)

#Decile 1 Income  Decile 2 Income  Decile 3 Income  Decile 4 Income  Decile 5 Income  Decile 6 Income  Decile 7 Income  Decile 8 Income  Decile 9 Income 
#0.01973707       0.05897217       0.11283934       0.17927267       0.25764043       0.34840362       0.45332081       0.57662626       0.72975926 
#Decile 10 Income 
#0.99996585 
#__________________________________________________________________________________________________________________________________________
#Drawing Lorenz curves for all one one graph 
plot(cum_inc_share_p80, type = "l", col = "blue", 
     lty = 2, lwd = 2, xlab = "Deciles", 
     ylab = "Cumulative income share")  

# Add the perfect equality line 
abline(a = 0, b = 0.1, col = "black", lwd = 2)  

# lty = 1 = dashed line  
lines(cum_inc_share_p14, col = "green", lty = 1, lwd = 2)

# lty = 2 = solid line  
lines(cum_inc_share_n80, col = "red", lty = 2, lwd = 2)

lines(cum_inc_share_n14, col = "orange", 
      lty = 1, lwd = 2)  

title("Lorenz curves, Poland and Norway (1980 and 2014)")  

legend("topleft", lty = 2:1, lwd = 2, cex = 1.2, legend = 
         c("Poland, 1980", "Poland, 2014",
           "Norway, 1980", "Norway, 2014"),  
       col = c("blue", "green", "red", "orange")) 
#Lorenz curve 
#Gini coefficient will be calculated below, it is the space between the line and lorenz curve
#_______________________________________________________________________________________________________________________________________________
#Gini Coefficient 
print(g_p80 = Gini(decs_p80))
#Poland 1980: .3436621
print(g_p14 = Gini(decs_p14))
#Poland 2014: .2531769
print(g_n80 = Gini(decs_n80))
#Norway 1980: .243128
print(g_n14 = Gini(decs_n14))
#Norway 2014: .352667
paste("Gini coefficients")
#Poland 
paste("Poland - 1980: ", round(g_p80, 2), 
      ", 2014: ", round(g_p14, 2))
#Norway 
paste("Norway - 1980: ", round(g_n80, 2), 
      ", 2014: ", round(g_n14, 2))
#Plot of Gini Coefficients 
plot(cum_inc_share_p80, type = "l", col = "blue", lty = 2, 
     lwd = 2, xlab = "Deciles", 
     ylab = "Cumulative income share")   

# Add the perfect equality line
abline(a = 0, b = 0.1, col = "black", lwd = 2)

# lty = 1 = dashed line
lines(cum_inc_share_p14, col = "green", lty = 1, lwd = 2)

# lty = 2 = solid line
lines(cum_inc_share_n80, col = "red", lty = 2, lwd = 2)

lines(cum_inc_share_n14, col = "orange", lty = 1, lwd = 2)

title("Lorenz curves, Poland and Norway (1980 and 2014) with Gini Coefficient")

legend("topleft", lty = 2:1, lwd = 2, cex = 1.2, legend = 
         c("Poland, 1980", "Poland, 2014", 
           "Norway, 1980", "Norway, 2014"),
       col = c("blue", "green", "red", "orange"))

text(8.5, 0.78, round(g_p80, digits = 3))
text(9.4, 0.6, round(g_p14, digits = 3))
text(5.7, 0.38, round(g_n80, digits = 3))
text(6.4, 0.3, round(g_n14, digits = 3))

#the Gini coefficient is a way of expressing income inequality as a function of some frequency distribution.
#for Poland in 1980 the Gini coefficient is .34, in 2014 it was .25.  This indicates that income inequality has lessoned over time.
#For Norway in 1980 the Gini coefficient in 1980 was .24, while in 2014 its actually .35 indicating that income inequality has risen over time.

#________________________________________________________________________________________________________________________________________________
