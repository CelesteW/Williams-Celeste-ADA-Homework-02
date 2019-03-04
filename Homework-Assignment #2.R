###Challenge 11.1

#Every Saturday, at the same time, a primatologist goes and sits in the forest in the morning and listens for titi monkey calls, counting the number of calls they hear in a 2 hour window from 5am to 7am. Based on previous knowledge, she believes that the mean number calls she will hear in that time is 15. Let X represent the appropriate Poisson random variable of the number of calls heard in each monitoring session.

#1a) What is the probability that she will hear more than 8 calls during any given session?
1-ppois(9, 15)

#1b) What is the probability that she will hear no calls in a session?
dpois(0,15)

#1c) What is the probability that she will hear exactly 3 calls in a session?
dpois(3,15)  

#1d. Plot the relevant Poisson mass function over the values in range 0 ≤ x ≤ 30.
x <- 0:30
l = 15
probset <- dpois(x = x, lambda = l)
barplot(probset, names.arg = x)

#1e. Simulate 104 results from this distribution (i.e., 2 years of Saturday monitoring sessions).
#Plot the simulated results using hist() and use xlim() to set the horizontal limits
#to be from 0 to 30. How does your histogram compare to the shape of the probability
#mass function you plotted above?
sample(x, 104,replace = TRUE)


#1f. Plot the simulated results using hist() and use xlim() to set the horizontal limits to be from 0 to 30. How does your histogram compare to the shape of the probability mass function you plotted above?
hist(sample(x, 104,replace = TRUE), xlim=c(0,30))
    #My histogram  doesn't follow a normal distribution like the probability mass function did. It was close to normal, except there was a larger number of observations where 0-5 calls were heard.


###Challenge 11.2
f<-"~/Desktop/zombies.csv"
d <- read.table(f, header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(d)

#2a) Calculate the population mean and standard deviation for each quantitative random variable (height, weight, age, number of zombies killed, and years of education).
summary(d)
names(d)
mean(d$height, na.rm=TRUE)
mean(d$weight, na.rm=TRUE)
mean(d$age, na.rm=TRUE)
mean(d$zombies_killed, na.rm=TRUE)
mean(d$years_of_education, na.rm=TRUE)
sqrt(sum((d$height-mean(d$height))^2)/length(d$height))
sqrt(sum((d$weight-mean(d$weight))^2)/length(d$weight))
sqrt(sum((d$age-mean(d$age))^2)/length(d$age))
sqrt(sum((d$zombies_killed-mean(d$zombies_killed))^2)/length(d$zombies_killed))
sqrt(sum((d$years_of_education-mean(d$years_of_education))^2)/length(d$years_of_education))



#2b)Use {ggplot} and make boxplots of each of these variable by gender.
ggplot(d, aes(x=gender, y=height)) + geom_boxplot()
ggplot(d, aes(x=gender, y=weight)) + geom_boxplot()
ggplot(d, aes(x=gender, y=age)) + geom_boxplot()
ggplot(d, aes(x=gender, y=years_of_education)) + geom_boxplot()
ggplot(d, aes(x=gender, y=zombies_killed)) + geom_boxplot()
ggplot(d, aes(x=gender, y=years_of_education)) + geom_boxplot()

#2c)Use {ggplot} and make scatterplots of height and weight in relation to age. Do these variables seem to be related? In what way?
ggplot(data = d, aes(x = age, y=height)) + geom_point()
ggplot(data = d, aes(x = age, y=weight)) + geom_point()
    #Height and weight both seem to have a positive correlation with age, as age increases so does height and weight. However, there appears to be a stronger relationship between height and age

#2d) Using histograms and Q-Q plots, check whether the quantitative variables seem to be drawn from a normal distribution. Which seem to be and which do not?

#Height
summary(d$height)
hist(d$height, breaks = seq(from=50, to=85, by=.5))
qqnorm(d$height, main = "QQ Plot- Height")
qqline(d$height)

#Weight
summary(d$weight)
hist(d$weight, breaks = seq(from=85, to=215, by=1))
qqnorm(d$weight, main = "QQ Plot- Weight")
qqline(d$weight)

#Age
summary(d$age)
hist(d$age, breaks = seq(from=10, to=30, by=.5))
qqnorm(d$age, main = "QQ Plot- Age")
qqline(d$age)

#Years of Education
summary(d$years_of_education)
hist(d$years_of_education, breaks = seq(from=0, to=10, by=1))
qqnorm(d$years_of_education, main = "QQ Plot- Years of Education")
qqline(d$years_of_education)

#Zombies Killed
summary(d$zombies_killed)
hist(d$zombies_killed, breaks = seq(from=0, to=11, by=1))
qqnorm(d$zombies_killed, main = "QQ Plot- Zombies Killed")
qqline(d$zombies_killed)

##Height, weight, and age seem to be drawn from a normal distribution, whereas years of education and zombies killed do not.

#2e) Now use the sample() function to sample ONE subset of 30 zombies (without replacement) from this population and calculate the mean and sample standard deviation for each variable. 
  #Also estimate the standard error for each variable and construct the 95% confidence interval for each mean. Note that for the variables that are not drawn from the normal distribution, you will need to base your estimate of the CIs on some different distribution!

Z<- d[sample(nrow(d),30),]
Z

#Height
mean(Z$height)
sd(Z$height)
se(Z$height)
qnorm(0.025, mean(Z$height), sd(Z$height)) 
qnorm(0.975,mean(Z$height),sd(Z$height))

#Weight
mean(Z$weight)
sd(Z$weight)
se(Z$weight)
qnorm(0.025, mean(Z$weight), sd(Z$weight))
qnorm(0.975,mean(Z$weight),sd(Z$weight))

#Zombies Killed
mean(Z$zombies_killed)
sd(Z$zombies_killed)
se(Z$zombies_killed)
quantile(Z$zombies_killed, c(0.025, 0.975))
summary(Z$zombies_killed)

#Years of Education
mean(Z$years_of_education)
sd(Z$years_of_education)
se(Z$years_of_education)
quantile(Z$years_of_education, c(0.025, 0.975))
summary(Z$years_of_education)

#Age
mean(Z$age)
sd(Z$age)
se(Z$age)
qnorm(0.025, mean(Z$age), sd(Z$age))
qnorm(0.975,mean(Z$age),sd(Z$age))




#2f) Now draw 99 more random samples of 30 zombies out and calculate the mean for each of the these samples.

k <- 99  
n <- 30  
s <- NULL  
for (i in 1:k) {
  s[[i]] <- d[sample(nrow(d),30),]
}
head(s)


#Height
Mean_Height<-NULL
for (i in 1:k) {
  Mean_Height[i]<- mean(s[[i]]$height)
}
Mean_Height 



#Weight
Mean_Weight<-NULL
for (i in 1:k) {
  Mean_Weight[i]<- mean(s[[i]]$weight)
}
Mean_Weight



#Age
Mean_Age<-NULL
for (i in 1:k) {
  Mean_Age[i]<- mean(s[[i]]$age)
}
Mean_Age



#Zombies Killed
Mean_Zombies_Killed<-NULL
for (i in 1:k) {
  Mean_Zombies_Killed[i]<- mean(s[[i]]$zombies_killed)
}
Mean_Zombies_Killed



#Years of Education
Mean_Years_Education<-NULL
for (i in 1:k) {
  Mean_Years_Education[i]<- mean(s[[i]]$years_of_education)
}
Mean_Years_Education





res1 <-c(mean(Z$height),mean(Z$weight), mean(Z$age),mean(Z$zombies_killed), mean(Z$years_of_education))
res1


nnsamp<- cbind(Mean_Height, Mean_Weight, Mean_Age, Mean_Zombies_Killed, Mean_Years_Education)
nnsamp
zsamp<- rbind(res1)
zsamp
zzsamp<- rbind (nnsamp, zsamp)
zzsamp
zzsamp<- as.data.frame(zzsamp)
#Height
mean(zzsamp$Mean_Height)
sd(zzsamp$Mean_Height)
se(Z$height)
hist(zzsamp$Mean_Height)


#Weight
mean(zzsamp$Mean_Weight)
sd(zzsamp$Mean_Weight)
se(Z$weight)
hist(zzsamp$Mean_Weight)


#Age
mean(zzsamp$Mean_Age)
sd(zzsamp$Mean_Age)
se(Z$age)
hist(zzsamp$Mean_Age)

#Zombies Killed
mean(zzsamp$Mean_Zombies_Killed)
sd(zzsamp$Mean_Zombies_Killed)
se(Z$zombies_killed)
hist(zzsamp$Mean_Zombies_Killed)

#Years of Education
mean(zzsamp$Mean_Years_Education)
sd(zzsamp$Mean_Years_Education)
se(Z$years_of_education)
hist(zzsamp$Mean_Years_Education)




