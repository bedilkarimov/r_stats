setwd('C:\\Studies\\2nd semester\\Advanced Econometrics\\project')

options(scipen=999)

# According to the the Moro., Rita., Val 2015 paper in the facebook dataset there are eleven variables which can be chosen as a dependant variable.
# We decided to model comments against following regressors: category, page total likes, type, post month, post hour, post weekday, paid.

# The objective is to model the number of comments for each post published on some cosmetics company's Facebook page.
# The predictors included in the analysis are: Type, Category, Post.Month, Post.Weekday, Post.Hour, Paid, Page total likes

# Category - Manual content characterization: action (special offers and contests), 
# Type - Type of content (Link, Photo, Status, Video).
# Post month - Month the post was published (January, February, ... , December)
# Post hour - Hour the post was published (0, 1, 2, 3, 4, …, 23).
# Post weekday - Weekday the post was published (Sunday, Monday, … , Saturday).
# Paid - If the company paid to Facebook for advertising (yes, no).
# Comments - Number of comments on the publication. Likes Number of “Likes” on the publication.
# Page total likes - total number of likes of the company's Facebook page

# Load the dataset
df = read.csv('dataset_Facebook.csv', sep=";")
df = df[,c(1:7,16)]


# Check the structure of the data
str(df)


# Variable transformation
df$Post.Hour = cut(df$Post.Hour , breaks = c(0,8,16,24), 
                   labels = c(" 0-8", " 8-16", " 16-24"))

df$Post.Month = cut(df$Post.Month , breaks = c(12,2,5,8,11), 
                    labels = c(" winter", " spring", " summer", " autumn"))


# Factorize the categorical variables
df$Type = as.factor(df$Type)
df$Category = as.factor(df$Category)
df$Post.Month = as.factor(df$Post.Month)
df$Post.Hour = as.factor(df$Post.Hour)
df$Post.Weekday = as.factor(df$Post.Weekday)
df$Paid = as.factor(df$Paid)


# Total interactions is a sum of comments, shares and likes, so there is a perfect linear relation between them.

# Let us check the correlations on the graph and remove the redundant variables.


# R treats categorical variables as dummy variables. 
# Categorical variables, also called indicator variables, are converted into dummy variables 
# by assigning the levels in the variable some numeric representation.
# The general rule is that if there are k categories in a factor variable, 
# the output of glm() will have k−1 categories with remaining 1 as the base category. (https://www.dataquest.io/blog/tutorial-poisson-regression-in-r/)

# Check first 5 rows of the dataset
head(df)

# Summary of the dataset
summary(df)

# Check the missing values
sum(is.na(df))
colSums(is.na(df))

# There are only 5 missing values in our dataset. Let's remove it.
df = na.omit(df)

# Number of zeros in comments
sum(df$comment==0)
round(100*sum(df$comment==0)/nrow(df),2)
# 20% of observations have zero comments


### Exploratory data analysis
# Response variable: number of comments
hist(df$comment, col = "darkred", main = "Frequency distribution for number of comments", breaks=0:max(df$comment))

# Visualize the relationship between the dependent variable and each regressor
plot(comment ~ Page.total.likes, data=df,col="darkgreen", xlab="Page total likes", ylab="Comments")
# The plot is not very informative.
# Both variables are count variables having numerous ties in the bivariate distribution and
# thus obscuring a large number of points in the display. To overcome the problem, it is useful
# to group the number of comments into a factor and produce a boxplot instead of a scatterplot. 
# Furthermore, the picture is much clearer if the dependent variable is log-transformed. 
# All count regression models which will be used in our analysis use a log link by default. 
# As there are zero counts as well, we use a convenience function
# clog() providing a continuity-corrected logarithm.

clog <- function(x) log(x + 0.5)

# For transforming a count variable to a factor (for visualization purposes only), we define
# another convenience function cfac()

cfac <- function(x, breaks = NULL) {
  if(is.null(breaks)) breaks <- unique(quantile(x, 0:10/10))
  x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
  levels(x) <- paste(breaks[-length(breaks)], ifelse(diff(breaks) > 1,
                                                     c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""),
                     sep = "")
  return(x)
}

plot(clog(comment) ~ cfac(Page.total.likes), data = df, col="darkred", xlab="Shares", ylab="Comments")
# Now we can clearly see the relationship between comments and page total likes. There is no obvious pattern.

# Let us investigate the relationship between response variable and the rest of regressors.
plot(clog(comment) ~ Post.Weekday, data = df, col="darkred",xlab="Weekday",ylab="Comments")
# Posts published in the 4th day of the week seems to have on average a higher number of comments
plot(clog(comment) ~ Post.Month, data = df, col="darkred",xlab="Month",ylab="Comments")
#plot(clog(comment) ~ Post.Hour, data = df, col="darkred",xlab="Hour",ylab="Comments")
# There is no clear relationship between month/hour and number of comments. 
plot(clog(comment) ~ Category, data = df, col="darkgreen", xlab="Category", ylab="Comment")
# The category of the post does not affect the number of comments the post receives.
plot(clog(comment) ~ cfac(Lifetime.Post.Total.Impressions), data = df,col="darkgreen", xlab="Lifetime post total impresions",ylab="Comment")
# Lifetime post total impresions has a positive relationship with number of comments.
plot(clog(comment) ~ Type, data = df,col="darkgreen", xlab="Type", ylab = "Comments")
# Posts with links get the least attention in the facebook. Posts with photos and status receive a little bit more attention than links.
# Video posts receive the most comments.
plot(clog(comment) ~ Paid, data = df,col="darkgreen",xlab="Paid",ylab="Comment")
# Sponsoring posts does not improve the popularity of posts.


### Modeling

# Check the mean and variance of the response variable
mean(df$comment)
var(df$comment)
# It seems that we have a dispersion problem: variance >> mean.

# To capture the relationship between the number of comments and all independent variables in a parametric regression model 
# we fit the Poisson regression model, negative binomial regression model and zero-inflated poisson regression model.

# As a first attempt we fit the basic Poisson regression model
fm_pois <- glm(comment ~ ., data = df, family = "poisson")

# Check the coeficient estimates along with associated partial Wald tests.
summary(fm_pois)
summary(fm_pois)$aic

# All the variables are significat. The Wald test results might be too optimistic due to 
# a misspecification of the likelihood. As the exploratory analysis suggested that over-dispersion 
# is present in this data set, we re-compute the Wald tests using sandwich standard errors.
library("sandwich") 
coeftest(fm_pois, vcov = sandwich)
# From the results above we can see that category and weekday are no longer significant.
# The standard errors for significant variables seem to be more appropriate. 
# This will also be confirmed by the following models that deal with over-dispersion and excess zeroes in a more formal way.


# Quasi-Poisson regression
fm_qpois <- glm(comment ~ ., data = df, family = "quasipoisson")
summary(fm_qpois)

# Post hour is no longer significant.
# Model above leads to an estimated dispersion of 21.38287 confirming that over-dispersion is present in the data.

# Negative binomial regression
# A more formal way to address over-dispersion in a count data regression model is to
# use a negative binomial model.
fm_nbin <- MASS::glm.nb(comment ~ ., data = df, maxit=100) 
summary(fm_nbin)
summary(fm_nbin)$aic

# Both regression coefficients and standard errors are rather similar to the
# quasi-Poisson. Thus, in terms of predicted means all three models give very similar results; the associated partial Wald tests also lead
# to the same conclusions.

# One advantage of the negative binomial model is that it is associated with a formal likelihood
# so that information criteria are readily available. Furthermore, the expected number of zeros
# can be computed from the fitted densities.

library(pscl)
# Zero-inflated regression
head(df)
# We remove the page total likes since with them ou zero inflated poisson model does not run, having the conditional singularity problem
# which may arise because of having too high numbers
df = df[,-1]
fm_zipoiss <- zeroinfl(comment ~ ., data = df, dist = "poiss")
summary(fm_zipoiss)

fm_zipoiss$aic

# Comparison of models

# cofficient comparison
fm <- list("ML-Pois" = fm_pois, "Quasi-Pois" = fm_qpois, "NB" = fm_nbin, "ZIPOISS" = fm_zipoiss)
sapply(fm, function(x) coef(x))

# The result shows that there are some small differences, especially between the
# GLMs and the zero-augmented models. However, the zero-augmented models have to be
# interpreted slightly differently: 
#   
# While the GLMs all have the same mean function , 
# the zero-augmentation also enters the mean function. 
# 
# Nevertheless, the overall impression is that the estimated mean functions are rather similar. 

# In summary, the models are not too different with respect to their fitted mean functions. The
# differences become obvious if not only the mean but the full likelihood is considered:

rbind(logLik = sapply(fm, function(x) round(logLik(x), digits = 0)),
      Df = sapply(fm, function(x) attr(logLik(x), "df")),
      AIC=sapply(fm, function(x) AIC(x,k=3))
)


# The NB model outperforms all other models.
# The quasi-Poisson model and the sandwich-adjusted Poisson model are not associated with a fitted likelihood. 
# The negative binomial already improves the fit dramatically.
# This also reflects that the over-dispersion in the data is captured better by the negative-binomial-based models than the plain Poisson
# model.

# Additionally, it is of interest how the zero counts are captured by the various models.
# Therefore, the observed zero counts are compared to the expected number of zero counts for
# the likelihood-based models:

round(c("Obs" = sum(df$comment < 1),
        "ML-Pois" = sum(dpois(0, fitted(fm_pois))),
        "NB" = sum(dnbinom(0, mu = fitted(fm_nbin), size = fm_nbin$theta)),
        "ZIPOISS" = sum(predict(fm_zipoiss, type = "prob")[,1])))

# The ML Poisson model is not appropriate whereas the negative binomial model is much better in modeling the zero counts.
# The expected number of zero counts in the zero-inflated models matches the observed number.

# In summary, negative binomial model leads to the best results (in terms of likelihood)
# on this data set. In terms of zeroes prediction zero-inflation poisson model performs the best.

# Run the dataset again
# Variable Selection
# Firstly, we see the indignificant variables, in this case are Type and Post.Hour
fm_nbin <- MASS::glm.nb(comment ~ ., data = df, maxit=100) 
summary(fm_nbin)

# We see if together they are insignificant 
fm_nbin <- MASS::glm.nb(comment ~ Type + Post.Hour, data = df, maxit=100) 
summary(fm_nbin)

# But it appears that together the Type variable is significant
# Thus we avoid the Post Hour (the more insignificant one)
df = df[,-6]
fm_nbin <- MASS::glm.nb(comment ~ ., data = df, maxit=100) 
summary(fm_nbin)

# We also get rid of Type
df = df[,-2]
fm_nbin <- MASS::glm.nb(comment ~ ., data = df, maxit=100) 
summary(fm_nbin)

# calculating the r squared
library(rsq)
rsq(fm_nbin, adj = TRUE)

# Calculating the R squared
# Here we did it manually
# 1 - (Residual deviance/Null deviance) = 1 - (500/574) = 0.13
