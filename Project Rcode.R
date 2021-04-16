 Pima.tr <- read.csv("~/Desktop/Pima.tr.csv")
   View(Pima.tr)
 attach(Pima.tr)
# makes "type" numeric 
 ifelse(Pima.tr$type=="Yes",1,0)
 Pima.tr$Type=ifelse(Pima.tr$type=="Yes",1,0)
 
# linear model 
a=lm(formula = Type~npreg+glu+bp+skin+bmi+ped+age,data = Pima.tr)

summary(a)

# log linear model, poisson

b=glm(formula = Type~npreg+glu+bp+skin+bmi+ped+age,family=poisson(link="log"),data = Pima.tr)

summary(b)

# logit regression model
c=glm(formula = Type~npreg+glu+bp+skin+bmi+ped+age,family=binomial(link="logit"),data = Pima.tr)
summary(c)

# probit model
d=glm(formula = Type~npreg+glu+bp+skin+bmi+ped+age,family=binomial(link="probit"),data = Pima.tr)
summary(d)

# boxplot for type and npreg
ggplot(data=Pima.tr, aes(x = npreg, y = type)) + geom_boxplot(col='red', fill='blue') + ylab("Diabetic") + coord_flip() + ggtitle("Boxplots for Diabetic and number of pregnacies")

# To check variance between type and npreg
summary(aov(npreg~type,data=Pima.tr))
