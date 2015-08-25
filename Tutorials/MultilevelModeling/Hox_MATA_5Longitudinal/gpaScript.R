# MATA Chapter 5: Analyzing Longitudinal Data

library(foreign)
gpa <- as.data.frame(read.spss(file="gpa2.sav"))
summary(gpa)
gpa$admitted <- factor(gpa$admitted)
gpa$sex <- factor(gpa$sex)
library(reshape2)

gpalong <- reshape(gpa, 
                   idvar='student', 
                   varying=list(gpa=c('gpa1','gpa2','gpa3','gpa4','gpa5','gpa6'),
                                job=c('job1','job2','job3','job4','job5','job6')),
                   v.names=c('gpa','job'), timevar='semester',time=0:5, direction="long")


gpalong <- gpalong[order(gpalong$student) , ]

summary(gpalong)

gpa2long <- read.spss(file="gpa2long.sav", to.data.frame=TRUE)

# plot histogram and normal curve. Figure 5.3.
h <- hist(gpalong$gpa,breaks=32, col='gray')
xfit<-seq(min(gpalong$gpa),max(gpalong$gpa),length=200) 
yfit<-dnorm(xfit,mean=mean(gpalong$gpa),sd=sd(gpalong$gpa)) 
yfit <- yfit*diff(h$mids[1:2])*length(gpalong$gpa) 
lines(xfit, yfit, col="blue", lwd=2)



# longitudinal Models -----------------------------------------------------

library(lme4)

model.1 <- lmer(gpa ~ 1 + ( 1 | student), gpalong)
summary(model.1)
model.2 <- lmer(gpa ~ 1 + semester + (1 |student), gpalong)
summary(model.2)
model.3 <- lmer(gpa ~ 1 + semester + as.numeric(job) + (1|student),gpalong)
summary(model.3)
model.4 <- lmer(gpa ~ 1 + semester + as.numeric(job) + highgpa + sex + (1|student),
                gpalong)
summary(model.4)
model.5 <- lmer(gpa ~ 1 + semester + as.numeric(job) + highgpa + sex + (1 + semester|student),
                gpalong)
summary(model.5)
model.6 <- lmer(gpa ~ 1 + semester + as.numeric(job) + highgpa + sex + semester:sex +
  (1 + semester|student), gpalong)
summary(model.6)
attach(gpalong)
model.6beta <- lmer(scale(gpa) ~ 1 + scale(semester) + scale(as.numeric(job)) + 
  scale(highgpa) + scale(as.numeric(sex),scale=F) + scale(semester):sex +
  (1 + scale(semester)|student), gpalong, REML=F)
summary(model.6beta)
detach(gpalong)
summary(as.numeric(gpalong$sex))




