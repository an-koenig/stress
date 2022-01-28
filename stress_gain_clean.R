library(tidyverse)

stress <- read.csv("R:/Forschung/COVIDiSTRESS/COVIDiSTRESS all global survey data/COVIDiSTRESS cleaned data and cleaning code (data from 27 April)/COVIDiSTRESS_May_30_cleaned_final.csv")


stress$gain_safe <- ifelse(stress$AD_gain == 
                             "· If Program A is adopted, 200 people will be saved.",
                           1, 0)

stress$loss_safe <- ifelse(stress$AD_loss ==
                             "· If Program C is adopted 400 people will die.",
                           1, 0)

stress$gain_risky <- ifelse(stress$AD_gain ==
                              "· If Program B is adopted, there is 1/3 probability that 600 people will be saved, and 2/3 probability that no people will be saved",
                            1, 0)

stress$loss_risky <- ifelse(stress$AD_loss ==
                              "· If Program D is adopted there is 1/3 probability that nobody will die, and 2/3 probability that 600 people will die.",
                            1, 0)

#applying exclusion criteria

stress2 <- stress %>% group_by(Country) %>% mutate(ns = n()) %>% filter(ns>=30) #Lieberoth et al. (2021)

#%>% select(Country,ns)

stress3 <- stress2 %>% filter(Duration..in.seconds.>= 132)

stress4 <- stress3 %>% ungroup()



stress5 <- stress4 %>% select(Dem_gender, Dem_age, gain_safe, PSS10_avg, neu, ext, ope, agr, con) 

stress5 <- na.omit(stress5) #third exclusion criterion

names(stress5)[names(stress5) == 'Dem_gender'] <- 'gender'
names(stress5)[names(stress5) == 'Dem_age'] <- 'age'
names(stress5)[names(stress5) == 'gain_safe'] <- 'choice'
names(stress5)[names(stress5) == 'PSS10_avg'] <- 'stress'
names(stress5)[names(stress5) == 'neu'] <- 'N'
names(stress5)[names(stress5) == 'ext'] <- 'E'
names(stress5)[names(stress5) == 'ope'] <- 'O'
names(stress5)[names(stress5) == 'agr'] <- 'A'
names(stress5)[names(stress5) == 'con'] <- 'C'


stress5 <- mutate(stress5, 
                  gender = case_when(
                    gender == "Female" ~ 2,
                    gender == "Male" ~ 1,
                    gender == "Other/would rather not say" ~ 0
                  )) 



library(bnlearn)
library(qgraph)

stress5$gender <- factor(stress5$gender,
                         levels = c(0,1,2),
                         labels = c("Other/would rather not say","Male", "Female"))



for(i in c(2:9)) {
  stress5[,i] <- as.numeric(unlist(stress5[,i]))
}



Whitelist <- matrix(c( #define direction, needs to be matrix
  "age", "choice"
),,2,byrow = T)

colnames(Whitelist) <- c("from", "to")

Blacklist <- matrix(c( #nothing can cause age, needs to be matrix
  "stress", "age",
  "choice", "age",
  "N", "age",
  "E", "age",
  "O", "age",
  "A", "age",
  "C", "age",
  "stress", "gender",
  "choice", "gender",
  "N", "gender",
  "E", "gender",
  "O", "gender",
  "A", "gender",
  "C", "gender",
  "gender", "age",
  "age", "gender",
  "stress", "N",
  "stress", "E",
  "stress", "O",
  "stress", "A",
  "stress", "C"
),,2,byrow=T)

colnames(Blacklist) <- c("from", "to")

Res <- pc.stable(stress5,
            whitelist=Whitelist,
            blacklist=Blacklist)

#plot network

Labels <- c(
  "female, male, other",
  "in years",
  "safe choice (gain)",
  "perceived stress",
  "neuroticism",
  "extraversion",
  "openness",
  "agreeableness",
  "conscientiousness"
)

g <- qgraph(Res, nodeNames = Labels, legend.cex = 0.4,
            asize = 5, edge.color = "black") 

#g <- qgraph(Res, nodeNames = Labels, legend.cex = 0.4,
#       asize = 5, edge.color = "black", filetype = "jpg") 


Res <- set.arc(Res, from = "N", to = "E")
Res <- set.arc(Res, from = "C", to = "E")

#g <- qgraph(Res, nodeNames=Labels, legend.cex = 0.5,
#asize = 0.5, edge.color = "black")
#g

fit <- bn.fit(Res, as.data.frame(stress5))

fit$choice
fit$stress

boot <- boot.strength(stress5, R = 1000,
                      algorithm = "pc.stable",
                      algorithm.args = list(
                        whitelist = Whitelist,
                        blacklist = Blacklist
                      ))

qgraph(boot, nodeNames = Labels, legend.cex = 0.5,
       edge.labels = T, layout = g$layout,
       asize = 5,
       edge.color = "black", filetype = "jpg")

