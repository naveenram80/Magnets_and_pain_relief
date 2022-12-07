#************************#
#******CASE STUDY ON MAGNETS AND PAIN RELIEF*****#
#************************#

library("moments")
library("ggpubr")
library("sm")
library("gplots")
library("multcomp")

#Reading data from file into the program.

my_data1 <- read.delim("https://github.com/SiddhuSiddharth/Magnets-and-Pain-Relief/blob/master/Polio1.txt")
my_data2 <- read.delim("https://github.com/SiddhuSiddharth/Magnets-and-Pain-Relief/blob/master/Polio2.txt")
my_data <- read.delim("https://github.com/SiddhuSiddharth/Magnets-and-Pain-Relief/blob/master/Polio.txt")

# Plotting boxplot.

boxplot(my_data$Score_2 ~ my_data$Active, data = my_data, xlab = "Boxplots of Treatment and Control Groups",
        ylab = "Pain rating (Score 2)", main = "pain ratings of subjects treated with active magnets vs inactive placebo devices.", col = rainbow(2))

ggline(my_data, x = "Active", y = "Score_2", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2"),
       ylab = "Pain rating (Score 2)", xlab = "Treatment and Control groups")

plotmeans(Score_2 ~ Active, data = my_data, frame = FALSE,
          xlab = "Treatment and Control Groups", ylab = "Pain rating (Score 2)",
          main="Mean Plot with 95% CI") 

summary(my_data1$Score_2)
sd(my_data1$Score_2)
skewness(my_data1$Score_2)
kurtosis(my_data1$Score_2)

summary(my_data2$Score_2)
sd(my_data2$Score_2)
skewness(my_data2$Score_2)
kurtosis(my_data2$Score_2)


#We can clearly see that the active magnetic device
#has an impact on reducing the pain of the patient, as 
#indicated by the lower median of pain score(~=4) in
#treatment group as compared to higher median of 
#pain score(~=8.5) in the control group, in the boxplots.

# Plotting histogram for control vs treatment group

#Histogram with Normal Curve for Treatment Group

hist(my_data1$Score_2, main="Pain Rating of Treatment Group",
     xlab="Pain Rating", xlim=c(-0.5,10.5), col="yellow",freq=FALSE, breaks = seq(0, 10, length.out = 11))
x <- seq(0, 10, length = 100)
f <- dnorm(x, mean = mean(my_data1$Score_2), sd = sd(my_data1$Score_2))
lines(x, f, col = "darkgreen", lwd = 2)


#Histogram with Normal Curve for Control Group

hist(my_data2$Score_2, prob = TRUE, main = "Pain Rating of Control Group", xlim=c(-0.5,10.5), xlab="Pain Rating (Score 2) ", col="yellow", breaks=8)
x <- seq(0, 10, length = 100)
f <- dnorm(x, mean = mean(my_data2$Score_2), sd = sd(my_data2$Score_2))
lines(x, f, col = "darkgreen", lwd = 2)


#From the histogram plot, the two groups are not normally distributed.

# Kernel Density Plot 1
kd1 <- density(my_data1$Score_2)
plot(kd1, main="Kernel Density Plot of Score 2 in Treatment Group Patients",  frame =FALSE)
polygon(kd1, col="red", border="blue")

# Kernel Density Plot 2
kd2 <- density(my_data2$Score_2)
plot(kd2, main="Kernel Density Plot of Score 2 in Control Group Patients", xlim = c(0,13), frame =FALSE)
polygon(kd2, col="green", border="blue")

# Comparing Kernel Density Plots 1 and 2

sm.density.compare(my_data$Score_2, group = my_data$Active, xlab = "Pain Rating (Score 2)", frame = FALSE)
title(main="Kernel Density Distributions of Treatment and Control Groups")

#Plots 1 and 2 above are a representation of the distribution 
#of Pain Rating after device application (Score 2) that use a
#kernel density estimate to show the probability density function
#of the variable. Plot 3 shows the overlap of plots 1 and 2.
#We can clearly infer that the mode for Treatment Group (~=4) 
#is lower than that for the Control Group (~=9).

# Confidence interval for Score 2 of Treatment group 
l.model1 <- lm(Score_2 ~ 1, my_data1)
confint(l.model1, level=0.95)

# Confidence interval for Score 2 of Control group 
l.model2 <- lm(Score_2 ~ 1, my_data2)
confint(l.model2, level=0.95)

#This data allows us to see that the threshold of pain in 
#the placebo group is higher than the treatment group.


#Performing one-way anova on data.


res.aov <- aov(Score_2 ~ Active, data = my_data)
summary(res.aov)

#The results of our one-way anova reveal that the p value,
#represented by Pr(>F)=3.26e-06 is lesser than the significance
#level 0.05. Thus we might be impelled to conclude that there
#are significant differences between the control and treatment group.


#Although this might very well be the case, a one way anova 
#assumes normality (- that each sample is taken from a normally
#distributed population) and variance equality (- that the variance
#of data in the different groups be the same), which is clearly not the 
#case here, standard deviation of control group(1.86) is nearly half 
#of that of treatment group(3.14) and both groups are not
#normally distributed.

#F test reveals the ratio of variances of the two groups.
res.ftest <- var.test(Score_2 ~ Active, data = my_data)
res.ftest

#p-value is 0.01779, lesser than the significance level
#alpha = 0.05. Because we see that there is significant 
#difference between the variances of the two sets of data, 
#we perform independent groups t test.


#Thus, due to heteroscedasticity, we take the approach of an
#unpaired t-test to accomplish what we require.

res <- t.test(my_data1$Score_2, my_data2$Score_2, alternative = "two.sided", var.equal = FALSE)
res


#By performing the Welch two sample test, the 
#confidence interval on the difference between means shows that
#that the effect produced by active magnetic device is huge and
#therefore of practical as well as statistical significance.

#Subjects who had an active magnet reported significantly
#less pain than did subjects who received the placebo.

#We reject null hypothesis, accept alternate hypothesis that 
#active magnets help with pain reduction with reference to post 
#polio syndrome.
