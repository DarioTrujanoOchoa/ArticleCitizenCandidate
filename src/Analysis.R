rm(list=ls())
#setwd("C:/Users/dtruj/Dropbox/Citizen-Candidate/ArticleCitizenCandidate")

library(pacman)
p_load(dplyr)
p_load(tidyr)
p_load(foreign)
source("src/QRE_functions_2.R")
source("src/VotingRules.R")
source("src/functions_CitCand.R")
#Import raw data              ####

## Merge data bases with the games of interest     ####
data_first <- read.csv("data/FirstTreatments.csv", sep=";", dec=",")
data_second <- read.csv("data/2x2x2.csv")
# counterbalance of ideal points
ex70 <- data_first[(data_first$num_periodo<=15 & data_first$Juego== "A")|(data_first$num_periodo>15 & data_first$Juego== "B"),]
ex70$Juego <- as.factor(rep("ex70", length(ex70$Juego)))
ex80 <- data_first[(data_first$num_periodo>15 & data_first$Juego== "A")|(data_first$num_periodo<=15 & data_first$Juego== "B"),]
ex80$Juego <- as.factor(rep("ex80", length(ex80$Juego)))
ex80$Sesion <- ex80$Sesion
data_1st_by_q <- rbind(ex70, ex80)
## set positions ####
positions1<- factor(c("Left","Center","Right","Right"),levels = c("Left","Center","Right"),ordered=T)
ideal_points1 <- c(30, 50, 70, 80)
data_1st_by_q$Position <- positions1[match(data_1st_by_q$punto_ideal,ideal_points1)]
positions2<- factor(c("Left","Center","Right"),levels = c("Left","Center","Right"),ordered=T)
ideal_points2 <- c(20, 30, 80)
data_second$Position <- positions2[match(data_second$punto_ideal,ideal_points2)]

Code <- read.csv("data/TreatmentsCode.csv")
raw_data <- rbind(data_1st_by_q,data_second) %>% 
  filter(id_tipo == "R",punto_ideal != -1,Juego %in% Code$CodeInDataBase) %>% 
  mutate(Treatment = factor(Code$ShortName[match(Juego,table = Code$CodeInDataBase)],ordered = T,
                           levels = Code$ShortName))

# General Characteritics by Sessions ####
Gral_Session <- raw_data %>% group_by(Treatment,Sesion) %>% 
  do(data.frame( No.Participants=max(.$id_usuario),
                 Bankrupcy=sum(.$balance<0))) %>% 
  data.frame()

stargazer::stargazer(Gral_Session,title = "Characteritics by Sessions",
                     header = F,summary = F,out = "results/Tables/Sessions.tex",rownames = F)

# Entry Proportions ####
Election_game <-raw_data %>% group_by(Treatment,Position) %>% 
  do(data.frame(Entry=sum(.$se_postula))) %>% 
  spread(Treatment,Entry) %>% data.frame()
parentesis_percentage <- function(x){paste("(",round(x*100,digits = 1),")",sep = "")}
Election_prop <- raw_data %>% group_by(Treatment,Position) %>% 
  do(data.frame(Prop=mean(.$se_postula))) %>% mutate(Prop=parentesis_percentage(Prop)) %>% 
  spread(Treatment, Prop) %>% data.frame() %>% mutate(Position= rep("%",3)) 

Election_table <- data.frame(matrix(nrow = 7,ncol = 7))
Election_table[c(1,3,5),] <- as.matrix(Election_game)
Election_table[c(2,4,6),] <- as.matrix(Election_prop)
Election_table[c(7),] <- c("Total",table(raw_data$Position,raw_data$Treatment)[1,])
colnames(Election_table) <- colnames(Election_game)

stargazer::stargazer(Election_table,title = "Total number of entries by position and game",
                     header = F,summary = F,out = "results/Tables/Entries.tex",rownames = F,
                     label = "tab:rawentry")

row.names(Election_game)<-Election_game$Position
Election_game$Position <- NULL
Total <- table(raw_data$Position,raw_data$Treatment)[1,]# total trials
Election_game <- t(cbind(t(Election_game),Total))
colnames(Election_game) <- c("PR_LC_q1", "PR_HC_q1", "RO_LC_q1", "RO_HC_q1","PR_LC_q2_ex70","PR_LC_q3_ex80")

#MLE Estimation ####
##parameters ####
alpha = 0.1 # cambios en la valoraci?n de la politica
cost = c(5,20) #LC and HC in the experiment
benefit = 25
D = 40 # penalty if no one enters
p= c(alpha, cost[2], benefit,D)
# what everybody does
n = 3         # numer of players
s = c(1,0,1) #rep(1,n)  # 1 means participate
q = c(20, 30, 80)  # ideal points used in the experiment (they must be put in order)
Q = rbind(q, c(30, 50, 70), c(30, 50, 80))

p_LC <- p; p_LC[2]<-5
p_HC<- p; p_HC[2]<-20

lambda= .1 # the MLE for the first treatments was .078
probs = c(.1, .9, .2) 
## Calculations ####
### Global ###
MLE_global <- optim(par=.1,f = neg_logL_global_ABRSTU,election_game=Election_game, probs=c(.5, .5, .5), p_HC=p_HC, p_LC=p_LC, Q=Q,hessian = T)
neg_logL_global_ABRSTU(election_game = Election_game,lambda = MLE_global$par, probs = c(0.8, 0.1, 0.8), p_HC = p_HC, p_LC = p_LC, Q = Q)
Global <- c(MLE_global$par,sqrt(diag(solve(MLE_global$hessian))))
### # analysis by game ####
MLE_game<- data.frame(matrix(nrow = 2,ncol= length(Election_game[1,])))
names(MLE_game) <- colnames(Election_game)
for(g in 1:length(Election_game[1,])){
  optim_game <- optim(par = 0.1,f = neg_logL_ABRSTU,election_game=Election_game, probs=c(.5, .5, .5), p_HC=p_HC, p_LC=p_LC, Q=Q,G= g,hessian = T)
  MLE_game[1,g] <- optim_game$par
  MLE_game[2,g] <- sqrt(diag(solve(optim_game$hessian)))
}
MLE <- cbind(MLE_game,Global)
row.names(MLE) <- c("Lambda","sd")
save(MLE,"results/MLE.RData")
stargazer::stargazer(MLE,summary = F,out = "results/Tables/MLE.tex",label = "tab:mle",digits = 4)

#Graphs ####
raw_data %>% group_by(Treatment,punto_ideal) %>% do(data.frame(mean(.$se_postula)))
hist()
 
## QRE Path ####


