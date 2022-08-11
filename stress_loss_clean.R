library(tidyverse)

stress <- read_csv("R:/Forschung/COVIDiSTRESS/COVIDiSTRESS all global survey data/COVIDiSTRESS cleaned data and cleaning code (data from 27 April)/COVIDiSTRESS_May_30_cleaned_final.csv")

set.seed(2)

#applying exclusion criteria

#%>% select(Country,ns)

stress2 <- stress %>% filter(Duration..in.seconds.>= 132)


stress3 <- stress2 %>% select(Country, Dem_gender, Dem_age, AD_loss, PSS10_avg, neu, ext, ope, agr, con) 

stress3 <- na.omit(stress3) #second exclusion criterion
stress4 <- stress3 %>% group_by(Country) %>% mutate(ns = n()) %>% filter(ns>=30) #Lieberoth et al. (2021), third exclusion criterion
stress5 <- stress4 %>% ungroup()
stress5 <- stress5[!(stress5$Country=="other"),] #drop since country unclear (third exclusion criterion)

#descriptives

mean(stress5$Dem_age)
sd(stress5$Dem_age)
table(stress5$Dem_gender)
table(stress5$Country)

stress5$Country <- NULL
stress5$ns <- NULL


#define safe choice

stress5$loss_safe <- 99

for (i in 1:nrow(stress5)){
  if (grepl("Program C", stress5$AD_loss[i])){
    stress5$loss_safe[i] <- 1}
  if (grepl("Program D", stress5$AD_loss[i])){
    stress5$loss_safe[i] <- 0}
}

stress5$AD_loss <- NULL

names(stress5)[names(stress5) == 'Dem_gender'] <- 'gender'
names(stress5)[names(stress5) == 'Dem_age'] <- 'age'
names(stress5)[names(stress5) == 'loss_safe'] <- 'choice'
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

stress5$choice <- factor(stress5$choice,
                         levels = c(0,1),
                         labels = c("Risky","Safe"))


for(i in c(2:8)) {
  stress5[,i] <- as.numeric(unlist(stress5[,i]))
}

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
            blacklist=Blacklist)

#plot network

Labels <- c(
  "female, male, other",
  "in years",
  "perceived stress",
  "neuroticism",
  "extraversion",
  "openness",
  "agreeableness",
  "conscientiousness",
  "safe choice (loss)"
)

g <- qgraph(Res, nodeNames = Labels, legend.cex = 0.4,
            asize = 5, edge.color = "black") 

#g <- qgraph(Res, nodeNames = Labels, legend.cex = 0.4,
#       asize = 5, edge.color = "black", filetype = "jpg") 

Res <- set.arc(Res, from = "N", to = "E")
Res <- set.arc(Res, from = "E", to = "C")

#g <- qgraph(Res, nodeNames=Labels, legend.cex = 0.5,
#asize = 0.5, edge.color = "black")
#g

fit <- bn.fit(Res, as.data.frame(stress5))

fit$choice
fit$stress

boot <- boot.strength(stress5, R = 1000,
                      algorithm = "pc.stable",
                      algorithm.args = list(
                        blacklist = Blacklist
                      ))

qgraph(boot, nodeNames = Labels, legend.cex = 0.5,
       edge.labels = T, layout = g$layout,
       asize = 5,
       edge.color = "black", filetype = "jpg")

