install.packages("tidyverse")
install.packages("descr")
install.packages("broom")
library(tidyverse)
library(decr)
library(broom)
setwd("/Users/leadousset/Dropbox/Teaching/Y2 - S1 - Intro to R/student perspectives/Class2")
X<-read_delim(file="Data/fr-esr-insertion_professionnelle-master.csv",delim=";",col_names=TRUE, 
skip=0,locale=locale(encoding="UTF-8"))
summary(X)
Y<-X%>% select(poids_de_la_discipline:salaire_net_mensuel_regional_3eme_quartile) %>%
unlist(.) %>% unique(.) %>% sort(.)
Z<-c(".","fe","nd","ns") 
df.clean<-df %>%  mutate_all(funs(ifelse(. %in% Z,NA,.))) %>% 
mutate_at(vars(nombre_de_reponses:salaire_net_mensuel_regional_3eme_quartile), funs(as.numeric(.)))
A<-X %>% mutate_at(vars(nombre_de_reponses:salaire_net_mensuel_regional_3eme_quartile),~as.numeric(.))

###########################################

# download and load needed packages 
install.packages("tidyverse")
install.packages("descr")
install.packages("broom")
library(tidyverse)
library(decr)
library(broom)
# set working directory
setwd("/Users/leadousset/Dropbox/Teaching/Y2 - S1 - Intro to R/student perspectives/Class2")
# load data
X<-read_delim(file="Data/fr-esr-insertion_professionnelle-master.csv",delim=";",col_names=TRUE, 
skip=0,locale=locale(encoding="UTF-8"))
# inspect data
summary(X)
# display all values taken by the character variables in the dataset
Y<-X %>% select(poids_de_la_discipline:salaire_net_mensuel_regional_3eme_quartile) %>%
unlist(.) %>% unique(.) %>% sort(.)
# store weird values to be cleaned in a vector 
Z<-c(".","fe","nd","ns") 
# clean missing values
A<-X %>%  mutate_all(funs(ifelse(. %in% Z,NA,.))) %>% 
mutate_at(vars(nombre_de_reponses:salaire_net_mensuel_regional_3eme_quartile), funs(as.numeric(.)))

################################################

# download and load needed packages 
install.packages("tidyverse")
install.packages("descr")
install.packages("broom")

library(tidyverse)
library(decr)
library(broom)

# set working directory
setwd("/Users/leadousset/Dropbox/Teaching/Y2 - S1 - Intro to R/student perspectives/Class2")

# load data
X <- read_delim(file = "Data/fr-esr-insertion_professionnelle-master.csv", 
               delim = ";", 
               col_names = TRUE, 
               skip = 0, 
               locale = locale(encoding = "UTF-8"))

# inspect data
summary(X)

# display all values taken by the character variables in the dataset
Y <- X %>% 
  select(poids_de_la_discipline:salaire_net_mensuel_regional_3eme_quartile) %>%
  unlist(.) %>% 
  unique(.) %>% 
  sort(.)

# store weird values to be cleaned 
Z <- c(".", "fe", "nd", "ns") 

# clean missing values
A <- X %>% 
  mutate_all(funs(ifelse(. %in% Z, NA, .))) %>% 
  mutate_at(vars(nombre_de_reponses:salaire_net_mensuel_regional_3eme_quartile), funs(as.numeric(.)))

###############################################

# download and load needed packages 
install.packages("tidyverse")
install.packages("descr")
install.packages("broom")

library(tidyverse)
library(decr)
library(broom)

# set working directory
setwd("/Users/leadousset/Dropbox/Teaching/Y2 - S1 - Intro to R/student perspectives/Class2")

# load data
df <- read_delim(file = "Data/fr-esr-insertion_professionnelle-master.csv", 
                delim = ";", 
                col_names = TRUE, 
                skip = 0, 
                locale = locale(encoding = "UTF-8"))

# inspect data
summary(df)

# display all values taken by the character variables in the dataset
unique.values <- df %>% 
  select(poids_de_la_discipline:salaire_net_mensuel_regional_3eme_quartile) %>%
  unlist(.) %>% 
  unique(.) %>% 
  sort(.)

# store weird values to be cleaned 
wrong.values <- c(".", "fe", "nd", "ns") 

# clean missing values
df.clean <- wrong.values %>% 
  mutate_all(funs(ifelse(. %in% wrong_values, NA, .))) %>% 
  mutate_at(vars(nombre_de_reponses:salaire_net_mensuel_regional_3eme_quartile), funs(as.numeric(.)))

# plot the data
mean(df.clean$taux_dinsertion, na.rm=T) # it is 88.69

plot1 <- ggplot(df_graph, aes(x=taux_dinsertion)) +
  geom_histogram() +
  theme_classic() +
  labs(x = "Employment rates") +
  geom_vline(data=df_mean_empl, aes(xintercept=88.69), linetype="dashed", size=1) 
plot1

##############################################

# download and load needed packages 
install.packages("tidyverse")
install.packages("descr")
install.packages("broom")

library(tidyverse)
library(decr)
library(broom)

# set working directory
setwd("/Users/leadousset/Dropbox/Teaching/Y2 - S1 - Intro to R/student perspectives/Class2")

# load data
df <- read_delim(file = "Data/fr-esr-insertion_professionnelle-master.csv", 
                 delim = ";", 
                 col_names = TRUE, 
                 skip = 0, 
                 locale = locale(encoding = "UTF-8"))

# inspect data
summary(df)

# display all values taken by the character variables in the dataset
unique.values <- df %>% 
  select(poids_de_la_discipline:salaire_net_mensuel_regional_3eme_quartile) %>%
  unlist(.) %>% 
  unique(.) %>% 
  sort(.)

# store weird values to be cleaned 
wrong.values <- c(".", "fe", "nd", "ns") 

# clean missing values
df.clean <- wrong.values %>% 
  mutate_all(funs(ifelse(. %in% wrong_values, NA, .))) %>% 
  mutate_at(vars(nombre_de_reponses:salaire_net_mensuel_regional_3eme_quartile), funs(as.numeric(.)))

# plot the data
df_mean_empl<- df_graph %>% 
  summarise(mean_empl = mean(taux_dinsertion, na.rm=T))

plot1 <- ggplot(df_graph, aes(x=taux_dinsertion)) +
  geom_histogram() +
  theme_classic() +
  labs(x = "Employment rates") +
  geom_vline(data=df_mean_empl, aes(xintercept=mean_empl), linetype="dashed", size=1) 
plot1



##############################################
####### Class 2 - Introduction to R ##########
##############################################

### Install if necessary and load packages
list.of.packages <- c("tidyverse", "descr", "broom")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

invisible(lapply(list.of.packages, library, character.only = TRUE))

### Set working directory 
setwd("/Users/leadousset/Dropbox/Teaching/Y2 - S1 - Intro to R/student perspectives/Class2")

### Load data
df <- read_delim(file = "Data/fr-esr-insertion_professionnelle-master.csv", 
                 delim = ";", 
                 col_names = TRUE, 
                 skip = 0, 
                 locale = locale(encoding = "UTF-8"))


##############################################
############# Inspecting the data ############
##############################################
# get some general information about all the variables
summary(df)


############################################
############# Cleaning the data ############
############################################

# display all values taken by the character variables in the dataset
unique.values <- df %>% 
  select(poids_de_la_discipline:salaire_net_mensuel_regional_3eme_quartile) %>%
  unlist(.) %>%
  unique(.) %>%
  sort(.)

# store values to be cleaned in a vector called wrong.values
wrong.values <- c(".", "fe", "nd", "ns") 

# clean missing values
df.clean <- df %>% 
  mutate_all(funs(ifelse(. %in% wrong.values, NA, .))) %>%
  mutate_at(vars(nombre_de_reponses:salaire_net_mensuel_regional_3eme_quartile), funs(as.numeric(.)))


########################################
############# Plot the data ############
########################################

df_mean_empl<- df_graph %>% 
  summarise(mean_empl = mean(taux_dinsertion, na.rm=T))

plot1 <- ggplot(df_graph, aes(x=taux_dinsertion)) +
  geom_histogram() +
  theme_classic() +
  labs(x = "Employment rates") +
  geom_vline(data=df_mean_empl, aes(xintercept=mean_empl), linetype="dashed", size=1) 
plot1