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

#gain_safe

#stress3 <- stress %>% select(Dem_gender, Dem_employment, Dem_age, gain_safe, PSS10_avg) 

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


#stress4 <- mutate(stress4, 
#                  Dem_employment = case_when(
#                    Dem_employment == "Student" ~ 1,
#                    Dem_employment == "Full time employed" ~ 2,
#                    Dem_employment == "Part time employed" ~ 3,
#                    Dem_employment == "Self-employed" ~ 4,
#                    Dem_employment == "Not employed" ~ 5,
#                    Dem_employment == "Retired" ~ 6
#                  )) 

#stress4 <- mutate(stress4, 
#                  Country = case_when(
#                    Country == "other" ~ 0,
#                    Country == "Afghanistan" |  Country =="Armenia"|  Country =="Azerbaijan"|
#                      Country =="Bahrain"| Country =="Bangladesh"|  Country =="Bhutan"|  Country =="Brunei"|
                  #     Country =="Cambodia"| Country =="China"|  Country =="Cyprus"|  Country =="East Timor (Timor-Leste)"|
                  #     Country =="Georgia"| Country =="India"|  Country =="Indonesia"|  Country =="Iran"|  Country =="Iraq"|  Country =="Israel"| 
                  #     Country =="Japan"|  Country =="Jordan"|  Country =="Kazakhstan"|  Country =="Korea, North"|  Country =="Korea, South"| 
                  #     Country =="Kuwait"|  Country =="Kyrgyzstan"|  Country =="Laos"|  Country =="Lebanon"|  Country =="Malaysia"|  Country =="Maldives"| 
                  #     Country =="Myanmar (Burma)"|  Country =="Nepal"|  Country =="Oman"|  Country =="Pakistan"|  Country =="Philippines"|
                  #     Country =="Qatar"|  Country =="Saudi Arabia"|  Country =="Singapore"|  Country =="Sri Lanka"|  Country =="Taiwan"|  Country =="Tajikistan"| 
                  #     Country =="Thailand"|  Country =="Turkey"|  Country =="Turkmenistan"|  Country =="United Arab Emirates"| 
                  #     Country =="Uzbekistan"|  Country =="Vietnam" ~ 1, #Asia
                  #   Country == "Albania"|  Country =="Andorra"|  Country =="Austria"|
                  #     Country =="Belarus"|  Country =="Belgium"|  Country =="Bosnia and Herzegovina"|
                  #     Country =="Bulgaria"|  Country =="Croatia"|  Country =="Czech Republic"|
                  #     Country =="Denmark"|  Country =="Estonia"|  Country =="Finland"|  Country =="France"|  Country =="Germany"|
                  #     Country =="Greece"|  Country =="Hungary"|  Country =="Iceland"|  Country =="Ireland"|  Country =="Italy"|  Country =="Kosovo"| 
                  #     Country =="Latvia"|  Country =="Liechtenstein"|  Country =="Lithuania"|  Country =="Luxembourg"|  Country =="Malta"| 
                  #     Country =="Moldova"|  Country =="Monaco"|  Country =="Montenegro"|  Country =="Netherlands"|  Country =="North Macedonia"| 
                  #     Country =="Norway"|  Country =="Poland"|  Country =="Portugal"|  Country =="Romania"|  Country =="Russia"|  Country =="San Marino"| 
                  #     Country =="Serbia"|  Country =="Slovakia"|  Country =="Slovenia"|  Country =="Spain"|  Country =="Sweden"|  Country =="Switzerland"| 
                  #     Country =="Ukraine"|  Country =="United Kingdom" ~ 2, #Europe
                  #   Country == "Algeria"|  Country =="Angola"|  Country =="Benin"|
                  #     Country =="Burkina Faso"|  Country =="Burundi"|  Country =="Cabo Verde"|
                  #     Country =="Cameroon"|  Country =="Comoros"|  Country =="Congo, Democratic Republic of the"|
                  #     Country =="Congo, Republic of the"|  Country =="Côte d'Ivoire"|
                  #     Country =="Djibouti"|  Country =="Egypt"|  Country =="Eritrea"|  Country =="Ethiopia"|
                  #     Country =="Gabon"|  Country =="Ghana"|  Country =="Guinea"|  Country =="Guinea-Bissau"|  Country =="Kenya"|
                  #     Country =="Lesotho"|  Country =="Libya"|  Country =="Madagascar"|  Country =="Malawi"|  Country =="Mali"|  Country =="Mauritania"| 
                  #     Country =="Mauritius"|  Country =="Morocco"|  Country =="Mozambique"|  Country =="Namibia"|  Country =="Niger"|
                  #     Country =="Nigeria"|  Country =="Rwanda"|  Country =="Sao Tome and Principe"|  Country =="Senegal"|  Country =="Seychelles"| 
                  #     Country =="Somalia"|  Country =="South Africa"|  Country =="Sudan"|  Country =="Sudan, South"|  Country =="Tanzania"| 
                  #     Country =="The Gambia"|  Country =="Tunisia"|  Country =="Uganda"|  Country =="Zambia"|  Country =="Zimbabwe" ~ 3, #Africa
                  #   Country == "Antigua and Barbuda"|  Country =="Argentina"|
                  #     Country =="Barbados"|  Country =="Belize"|  Country =="Bolivia"|  Country =="Brazil"|
                  #     Country =="Canada"|  Country =="Chile"|  Country =="Colombia"|  Country =="Costa Rica"|
                  #     Country =="Cuba"|  Country =="Dominica"|  Country =="Dominican Republic"|  Country =="Ecuador"|
                  #     Country =="El Salvador"|  Country =="Grenada"|  Country =="Guatemala"|  Country =="Guyana"|
                  #     Country =="Haiti"|  Country =="Honduras"|  Country =="Jamaica"|  Country =="Mexico"|  Country =="Nicaragua"|
                  #     Country =="Panama"|  Country =="Paraguay"|  Country =="Peru"|  Country =="Saint Lucia"|
                  #     Country =="Saint Vincent and the Grenadines"|  Country =="Suriname"|  Country =="The Bahamas"| 
                  #     Country =="Trinidad and Tobago"|  Country =="United States"|  Country =="Uruguay"|  Country =="Venezuela" ~ 4, #Americas
                  #   Country == "Australia"|  Country =="Fiji"|  Country =="Kiribati"|  Country =="Micronesia, Federated States of"| 
                  #     Country =="Nauru"|  Country =="New Zealand"|  Country =="Solomon Islands" ~5 #Oceania
                  # ))



library(bnlearn)
library(qgraph)

stress5$gender <- factor(stress5$gender,
                         levels = c(0,1,2),
                         labels = c("Other/would rather not say","Male", "Female"))

#stress5$Dem_employment <- factor(stress5$Dem_employment,
#                             levels = c(1,2,3,4,5,6),
#                             labels = c("Student","Full time employed", "Part time employed", 
#                                        "Self-employed", "Not employed", "Retired"))



# stress5$Country <- factor(stress5$Country,
#                              levels = c(0,1,2,3,4,5),
#                              labels = c("Other","Region 1", "Region 2",
#                                         "Region 3", "Region 4", "Region 5"))



for(i in c(2:9)) {
  stress5[,i] <- as.numeric(unlist(stress5[,i]))
}



#selection <- c(
#  "age", 
#  "education", 
#  "C5", #waste my time 
#  "O1", #am full of ideas
#  "N5" # panic easily
#)


#bfiSub <- bfiNoNA[, selection]

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

