###load in packages that you need###
library(dirmult)
library(DirichletReg)
library(shinystan)

#### simulate some data####
### start with one population, and then expand to multiple populations with the same environmental effects, but different q scores?
### one population analysis is straightforward and based on https://cran.r-project.org/web/packages/DirichletReg/index.html
### https://research.wu.ac.at/en/publications/dirichletreg-dirichlet-regression-for-compositional-data-in-r-3

### k=5
### simulating 1000 individuals, where each column is a q score for a specific possible parental type of increasing frequencies
q_scores<-rdirichlet(n=1000, alpha=c(1,2,3,4,5))

### check the sums of the rows and columns to demonstrate that this is true, the whole matrix sums to the number of individuals

### simulate 5 environmental variables, based on the q scores. 
### variables 1 & 2 will be unrelated to the q scores
### variables 3 and 4 will each be related to two different 'species'
### variable 5 will be related to all different q scores

en1<-rnorm(1000, 15, 8)
en2<-rnorm(1000, 0, 30)
en3<-1+5*q_scores[,1]+15*q_scores[,2]+rnorm(1000, 0, 5)
### check correlation between en3 and causal q scores, make sure it is a reasonable size for an ecological study
cor.test(en3, q_scores[,1])
cor.test(en3, q_scores[,2])
en4<-1+5*q_scores[,3]+15*q_scores[,4]+rnorm(1000, 0, 5)
cor.test(en4, q_scores[,3])
cor.test(en4, q_scores[,4])
en5<-1+5*q_scores[,1]+10*q_scores[,2]+15*q_scores[,3]+20*q_scores[,4]+rnorm(1000, 0, 5)
cor.test(en5, q_scores[,1])
cor.test(en5, q_scores[,2])
cor.test(en5, q_scores[,3])
cor.test(en5, q_scores[,4])

data<-cbind.data.frame(q_scores,en1, en2, en3, en4, en5)


###basic analysis in DirichReg
qscores_checked<-DR_data(data[,1:5], trafo = sqrt(.Machine$double.eps), base = 1)

full_model<-DirichReg(qscores_checked~en1+en2+en3+en4+en5, data=data)
full_model
coef(full_model)
summary(model)

###basically, we can compare models using ANOVA to try to work out which are the best parameters to keep in there, or we can report full models
###we could use something like random forest to try to knock down the number of parameters, or we could use PCs and then work from there. Or, we can just put then all in.
###as far as I can tell, Trevor doesn't have anything in his models to account for across population variation. 

#### given that we expect there to be environmental differences, and we want to be able to account for them, we need a hierarchical model that does this same thing, but with hyper parameters for each of the environmental variables
###let's simulate 5 populations (200 individuals each), and the only thing that we're gonna change is the relative q score distributins 

q_scores_p1<-rdirichlet(n=350, alpha=c(1,2,3,4,5))
q_scores_p2<-rdirichlet(n=350, alpha=c(5,4,3,2,1))
q_scores_p3<-rdirichlet(n=300, alpha=c(3,3,3,3,3))

q_scores_allpops<-rbind.data.frame(q_scores_p1, q_scores_p2, q_scores_p3)
names(q_scores_allpops)<-c("q1", "q2", "q3")
en1_pop1<-1+5*q_scores_p1[,1]+15*q_scores_p1[,2]+rnorm(350, 0, 5)
en2_pop1<-1+5*q_scores_p1[,2]+15*q_scores_p1[,5]+rnorm(350, 0, 5)
en3_pop1<-1+5*q_scores_p1[,3]+15*q_scores_p1[,4]+rnorm(350, 0, 5)
en4_pop1<-rnorm(350, 0, 5)
en5_pop1<-rnorm(350, 0, 5)

en1_pop2<-1+5*q_scores_p2[,1]+15*q_scores_p2[,2]+rnorm(350, 0, 5)
en2_pop2<-1+5*q_scores_p2[,2]+15*q_scores_p2[,5]+rnorm(350, 0, 5)
en3_pop2<-1+5*q_scores_p2[,3]+15*q_scores_p2[,4]+rnorm(350, 0, 5)
en4_pop2<-rnorm(350, 0, 5)
en5_pop2<-rnorm(350, 0, 5)

en1_pop3<-1+5*q_scores_p3[,1]+15*q_scores_p3[,2]+rnorm(300, 0, 5)
en2_pop3<-1+5*q_scores_p3[,2]+15*q_scores_p3[,5]+rnorm(300, 0, 5)
en3_pop3<-1+5*q_scores_p3[,3]+15*q_scores_p3[,4]+rnorm(300, 0, 5)
en4_pop3<-rnorm(300, 0, 5)
en5_pop3<-rnorm(300, 0, 5)



pop1<-cbind.data.frame(en1_pop1, en2_pop1, en3_pop1, en4_pop1, en5_pop1)
names(pop1)<-c("en1", "en2", "en3", "en4", "en5")
pop2<-cbind.data.frame(en1_pop2, en2_pop2, en3_pop2, en4_pop2, en5_pop2)
names(pop2)<-c("en1", "en2", "en3", "en4", "en5")
pop3<-cbind.data.frame(en1_pop3, en2_pop3, en3_pop3, en4_pop3, en5_pop3)
names(pop3)<-c("en1", "en2", "en3", "en4", "en5")


env_allpops<-rbind.data.frame(pop1, pop2, pop3) 
hierarchical_data<-cbind.data.frame(q_scores_allpops, env_allpops)
hierarchical_data$pop<-c(rep(1, 350), rep(2, 350), rep(3, 300))


#### The STAN PART ####
### we're going to implement this in STAN
###following this tutorial, and then changing things for dirichlet distribution, our data, etc
#https://ladyissy.github.io/bayes/
## https://ben-lambert.com/bayesian-lectures/ ->lecture 6

library(rstan)

###need to assign all of the information that's in the 'data' section of the .stan model
N<-length(hierarchical_data[,1]) ###how many observations are there
K<-5 ### how many environmental variables are there?
J<-length(unique(hierarchical_data$pop)) ###how many populations are there?
pop<-hierarchical_data$pop
x<-hierarchical_data[,4:8]
y<-hierarchical_data[,1:3]


### run the model
m_hier<-stan(file="Dirichlet_regression.stan", data=list(N=N, J=J, K=K, pop=pop, x=x, y=y))


###let's do some model checks###
### look at Rhat and n_eff to start
###rhat==1 means convergence has happened###
###n_eff is the effective sample size
m_hier

###I keep running into shiny stan, so lets play with that
launch_shinystan(m_hier)


