# this project is trying to obtain a prediction model able to predict 
# the results of the english premier league football games based on predictors deduced from 
# the FIFA 21 player ratings.

# to develop this model I will follow the next steps

# Download the required databases. 
# Datasets exploration
# Wrangling the databases into the required format
# database inspection and analysis
# predictors selection
# creation of training and test sets
# model testing
# model evaluation




#####################################################################################
# Download the required databases
####################################################################################

#install packages if required
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages( "readr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages( "Rborist", repos = "http://cran.us.r-project.org")
   
  # required libraries
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library (readr)
library(corrplot)
library(Rborist)

# Data base including premier league results
# the automatic column type defined by R was wrong because the initial lines contain several NAs
# therefore the code required to define the types of the columns

#EPL_results <- read.csv("C:/Users/jepet/Desktop/DATA WRANGLING COURSE/capstone/final project/results.csv")
EPL_results <- read_csv("https://raw.githubusercontent.com/jepeteso/finalproject/main/results.csv", 
         col_types = list(col_character(), col_character(),  col_character(), col_character(),
                          col_number(), col_number(), col_character(), col_number(),
                          col_number(), col_character(), col_character(),  col_number(),
                          col_number(), col_number(), col_number(), col_number(),
                          col_number(), col_number(), col_number(),col_number(),
                          col_number(),col_number(),col_number()))

# Download  fifa 20 English Premier League player's ratings

#EPL_fifa_ratings <- read.csv("C:/Users/jepet/Desktop/DATA WRANGLING COURSE/capstone/final project/EPL Player Info fifa.csv")
urlfile="https://raw.githubusercontent.com/jepeteso/finalproject/main/EPL%20Player%20Info%20fifa.csv"
EPL_fifa_ratings<- read_csv(url(urlfile))


###################################################################
#Datasets exploration
####################################################################
colnames(EPL_results)

#number of draw wins and loses by hometeam 
EPL_results %>% group_by(HomeTeam, FTR) %>% summarise(n=n()) %>% ggplot(aes(HomeTeam, n, color=FTR))+
  geom_point() + theme(axis.text.x = element_text(angle = 90))

#number of draw wins and loses by awayteam 
EPL_results %>% group_by(AwayTeam, FTR) %>% summarise(n=n()) %>% ggplot(aes(AwayTeam, n, color=FTR))+
  geom_point() + theme(axis.text.x = element_text(angle = 90))

#Number of times  an  specific amount of goals have been scored by an specific hometeam 
EPL_results %>% group_by(HomeTeam, FTHG) %>% summarise(n=n()) %>% ggplot(aes(HomeTeam, FTHG, size=n)) +
  geom_point(color="red") +   theme(axis.text.x = element_text(angle = 90)) + 
                                scale_y_continuous(limits=c(0,10), breaks=1:10)

#Number of times  an specific amount of goals have been scored by an specific Awayteam
EPL_results %>% group_by(AwayTeam, FTAG) %>% summarise(n=n()) %>% ggplot(aes(AwayTeam, FTAG, size=n)) +
  geom_point(color="Blue") +   theme(axis.text.x = element_text(angle = 90)) + 
  scale_y_continuous(limits=c(0,10), breaks=1:10)

# Results summary analyzing full time results, season and home team
EPL_results %>% group_by(Season, FTR, HomeTeam)%>% summarise(n=n())%>%  
  ggplot(aes(HomeTeam, Season, color= FTR, size=n)) + geom_point() +facet_grid(FTR ~ .) +  
  theme(axis.text.y = element_text(size=5), axis.text.x = element_text(angle = 90)) 
 
# Results summary analyzing full time results, season and away team
EPL_results %>% group_by(Season, FTR, AwayTeam)%>% summarise(n=n())%>%  
  ggplot(aes(AwayTeam, Season, color= FTR, size=n)) + geom_point() +facet_grid(FTR ~ .) +  
  theme(axis.text.y = element_text(size=5), axis.text.x = element_text(angle = 90)) 

###########################################################
# FIFA ratings dataset exploration
###########################################################

colnames(EPL_fifa_ratings)

#as several of the the column titles included spaces, it is necessary to replace the empty spaces
#with a character in this case I will utilize "_"

new_column_names <- str_replace_all(colnames(EPL_fifa_ratings), " ", "_") 
old_column_names<- colnames(EPL_fifa_ratings)
EPL_fifa_ratings <- EPL_fifa_ratings %>% rename_at(vars(old_column_names), ~new_column_names) 
colnames(EPL_fifa_ratings)

#the columns that have "(Euros)" in their names are removed using the following code
new_column_names2 <- str_replace_all(colnames(EPL_fifa_ratings), "_\\(Euros\\)", "")
old_column_names<- colnames(EPL_fifa_ratings)
EPL_fifa_ratings <- EPL_fifa_ratings %>% rename_at(vars(old_column_names), ~new_column_names2)
colnames(EPL_fifa_ratings)

#the columns that have "(FIFA)" in their names are removed using the following code
new_column_names3 <- str_replace_all(colnames(EPL_fifa_ratings), "\\(FIFA\\)", "")
old_column_names<- colnames(EPL_fifa_ratings)
EPL_fifa_ratings <- EPL_fifa_ratings %>% rename_at(vars(old_column_names), ~new_column_names3)
colnames(EPL_fifa_ratings)

#the columns that have "(Short)" in their names are removed using the following code
new_column_names4 <- str_replace_all(colnames(EPL_fifa_ratings), "_\\(short\\)", "S")
old_column_names<- colnames(EPL_fifa_ratings)
EPL_fifa_ratings <- EPL_fifa_ratings %>% rename_at(vars(old_column_names), ~new_column_names4)

#the columns that have "(kg)" in their names are removed using the following code
new_column_names5 <- str_replace_all(colnames(EPL_fifa_ratings), "_\\(kg\\)", "")
old_column_names<- colnames(EPL_fifa_ratings)
EPL_fifa_ratings <- EPL_fifa_ratings %>% rename_at(vars(old_column_names), ~new_column_names5)

#the columns that have "_" at the beginning of the name are removed using the following code
new_column_names6 <- str_replace_all(colnames(EPL_fifa_ratings), "_Dribbling", "Dribbling1")
old_column_names<- colnames(EPL_fifa_ratings)
EPL_fifa_ratings <- EPL_fifa_ratings %>% rename_at(vars(old_column_names), ~new_column_names6)
colnames(EPL_fifa_ratings)

# I will evaluate the amount of NAs in each column
colMeans(is.na(EPL_fifa_ratings))

# proportion of NA per column
NAS <-colMeans(is.na(EPL_fifa_ratings))%>% data.frame() 

#give a column name to the average
names(NAS)[1] <-"variable"

#identify the columns with a proportion of data above 90%
HighNA<- NAS %>% filter(variable > 0.10) %>% rownames()

#create a new data set without the columns with a proportion of data above 90% and 
#removal rows with NA from the data set
EPL_fifa_ratings <- EPL_fifa_ratings%>% select(!HighNA) %>% drop_na()


#Summarizing players characteristics by team

Clubs_summaries <- EPL_fifa_ratings %>% group_by(Club_NameS) %>% mutate(MoneyValue=Market_Value/Overall) %>% 
  summarise(Age=mean(Age), Weight=mean(Weight),Overall=mean(Overall), 
            MarketValue=mean(Market_Value), Potential=mean(Potential), Wages=mean(FIFA_Wage),
            MoneyValue=mean(MoneyValue)) %>%   tibble() 

# plots correlating Money value overall, potential and wages by team
Clubs_summaries %>%  arrange(MoneyValue) %>% ggplot(aes(MoneyValue, Overall, label=Club_NameS)) + geom_label()  
Clubs_summaries %>%  arrange(MoneyValue) %>% ggplot(aes(MoneyValue, Potential, label=Club_NameS)) + geom_label() 
Clubs_summaries%>%  arrange(MoneyValue)  %>% ggplot(aes(MoneyValue, Wages, label=Club_NameS)) + geom_label()

# plots exhibiting player names and their market_value, overall, club names, wages and team
EPL_fifa_ratings%>% group_by(General_Position) %>%
  filter(Market_Value>mean(Market_Value)) %>% 
  select(Player_Name, Market_Value, Club_NameS, Overall ) %>% 
  arrange(Market_Value) %>% ggplot(aes(Market_Value, Overall, label=Player_Name, Color=Club_NameS))+ 
  geom_label()+ geom_point(aes(color=Club_NameS))+facet_grid(General_Position ~ .)


# summary of players attributes in 7 categories and grouped by club name
Ratingssummary <- EPL_fifa_ratings%>% group_by(Club_NameS, General_Position)%>% 
  mutate(Pace=Acceleration+Sprint_Speed, 
         Shooting= Attacking_Finishing+Long_Shots+Penalities+Positioning+Shot_Power+Attacking_Volleys,
         Passing=Attacking_Crossing + Curve + FK_Accuracy + Long_Passing + Attacking_Short_Passing + Vision,
         Dribbling2= Agility + Balance + Ball_Control + Composure + Dribbling1 + Reactions,
         Defending= Attacking_Heading_Accuracy + Interceptions + Marking + Sliding_Tackle + Standing_Tackle,
         Physical= Aggression + Jumping + Stamina + Strength, 
         Goalkeeping= GK_Diving2 + GK_Handling2 + GK_Kicking2 + GK_Positioning2 + GK_Reflexes2 )%>%
  select(Pace,Shooting, Passing, Dribbling2, Defending, Physical,Goalkeeping) %>%   summarise(Pace=mean(Pace),
            Shooting=mean(Shooting), 
            Passing=mean(Passing), 
            Dribbling=mean(Dribbling2), 
            Defending=mean(Defending), 
            Physical=mean(Physical),
            Goalkeeping=mean(Goalkeeping)) %>% tibble()

#pace plot
Ratingssummary %>% 
  ggplot(aes(Pace,General_Position, color=Club_NameS))  + geom_point(size=3, show.legend=FALSE) + 
  geom_text(aes(Pace,General_Position, label=Club_NameS),show.legend=FALSE, size= 3, angle=90, hjust = 1.2)

#shooting plot
Ratingssummary %>% ggplot(aes(Shooting,General_Position, color=Club_NameS))  + geom_point(size=3, show.legend=FALSE) + 
  geom_text(aes(Shooting,General_Position, label=Club_NameS),show.legend=FALSE, size= 3, angle=90, hjust = 1.2)

#Dribling plot
Ratingssummary %>% ggplot(aes(Dribbling,General_Position, color=Club_NameS))  + geom_point(size=3, show.legend=FALSE) + 
  geom_text(aes(Dribbling,General_Position, label=Club_NameS),show.legend=FALSE, size= 3, angle=90, hjust = 1.2)

#Defending plot
Ratingssummary %>% ggplot(aes(Defending,General_Position, color=Club_NameS))  + geom_point(size=3, show.legend=FALSE) + 
  geom_text(aes(Defending,General_Position, label=Club_NameS),show.legend=FALSE, size= 3, angle=90, hjust = 1.2)

#Physical plot
Ratingssummary %>% ggplot(aes(Physical,General_Position, color=Club_NameS))  + geom_point(size=3, show.legend=FALSE) + 
  geom_text(aes(Physical,General_Position, label=Club_NameS),show.legend=FALSE, size= 3, angle=90, hjust = 1.2)

#Goalkepping plot
Ratingssummary %>% ggplot(aes(Goalkeeping,General_Position, color=Club_NameS))  + geom_point(size=3, show.legend=FALSE) + 
  geom_text(aes(Goalkeeping,General_Position, label=Club_NameS),show.legend=FALSE, size= 3, angle=90, hjust = 1.2)



###################################################################################
#Datasets wrangling process for match results prediction model for the season 2020
###################################################################################

#Select the results for each match played in the 2019-2020 season of the EPL
S2019_20 <- EPL_results %>% filter(Season == "2019-20")

# Selection of variables from the EPL_fifa_ratings database 

char<- EPL_fifa_ratings %>% select_if(is.character)%>% colnames() # selecting all the character columns
numer<- EPL_fifa_ratings %>% select_if(is.numeric)%>% colnames() #select all the numeric columns
EPL_fifa_ratings_r <- EPL_fifa_ratings[,c(char, numer)] # organice the columns locating the character columns first followed by the numeric


##################################################################################
#organizing home team data set
##################################################################################

# change the column Club_NameS in ratings to have the same as season 2019-20
HomeTeam_ratings <- EPL_fifa_ratings_r %>% rename(HomeTeam=Club_NameS) 

#test if all the club names are the same
identical(levels(as.factor(HomeTeam_ratings$HomeTeam)),levels(as.factor(S2019_20$HomeTeam))) 

#As the names of the clubs were not identical we need to homogenize the club names in both datasets

A <- levels(as.factor(HomeTeam_ratings$HomeTeam)) # levels in ratings HomeTeam
B <- levels(as.factor(S2019_20$HomeTeam)) # levels in results HomeTeam

#Identify the club names missing in A
A[!(A%in%B)]

#Homogenize the club names in the FIFA ratings and 2019-20 results
HomeTeam_ratings <- HomeTeam_ratings %>% 
  mutate(HomeTeam = str_replace_all(HomeTeam_ratings$HomeTeam, "Man Utd", "Man United")) # replace club names in ratings

HomeTeam_ratings_N <- HomeTeam_ratings %>% 
  mutate(HomeTeam = str_replace_all(HomeTeam_ratings$HomeTeam, "Spurs", "Tottenham" )) # replace club names in ratings

S2019_20_N <- S2019_20 %>% 
  mutate(HomeTeam = str_replace_all(S2019_20$HomeTeam, "Sheffield United","Sheffield Utd.")) #replace club names in results

identical(levels(as.factor(HomeTeam_ratings_N$HomeTeam)),levels(as.factor(S2019_20_N$HomeTeam))) # check if club names are identical


# Integration of HomeTeam's player's rating to results
Resuls_HomeRatings <- left_join(S2019_20_N2, HomeTeam_ratings_N, by= "HomeTeam" )

#columns reorganization to facilitate further calculations 
RH_num <- Resuls_HomeRatings %>% select_if(is.numeric)%>% colnames() #select all the numeric columns
RH_char <- Resuls_HomeRatings %>% select_if(is.character)%>% colnames() # select all the character columns
Resuls_HomeRatings_c <- Resuls_HomeRatings[,c(RH_char, RH_num )]  #organize the columns locating the character columns first followed by the numeric
Resuls_HomeRatings_c <-Resuls_HomeRatings_c %>%  
  mutate(Pace=Acceleration+Sprint_Speed, 
         Shooting= Attacking_Finishing+Long_Shots+Penalities+Positioning+Shot_Power+Attacking_Volleys,
         Passing=Attacking_Crossing + Curve + FK_Accuracy + Long_Passing + Attacking_Short_Passing + Vision,
         Dribbling2= Agility + Balance + Ball_Control + Composure + Dribbling1 + Reactions,
         Defending= Attacking_Heading_Accuracy + Interceptions + Marking + Sliding_Tackle + Standing_Tackle,
         Physical= Aggression + Jumping + Stamina + Strength, 
         Goalkeeping= GK_Diving2 + GK_Handling2 + GK_Kicking2 + GK_Positioning2 + GK_Reflexes2)

#correlation matrix using player ratings
Cor_play<- cor(Resuls_HomeRatings_c[28:104])


#correlation matrix plot
corrplot(Cor_play, method="color", type = "lower", tl.cex = 0.7 )


#as the correlation between the fifa ratings and the full time home team goals is really low,
# I will calculate the team ratings averaging the ratings for each home team


# summary of FIFA ratings by HomeTeam
HomeClub_summary <- Resuls_HomeRatings_c %>% group_by(HomeTeam)%>%  
  summarise_at(vars(27:103), mean) %>% data.frame()

#calculation of club ratings correlation matrix
HTR<- cor(HomeClub_summary[2:78])


#Correlation plot using home team ratings
corrplot(HTR, method="color", type = "lower", tl.cex = 0.7 )

########################################################################################
#Organizing away team data set
#######################################################################################

# change the column Club.Name..short in ratings to have the same as season 2019-20
AwayTeam_ratings <- EPL_fifa_ratings_r %>% rename(AwayTeam=Club_NameS) 

#test if all the club names are the same
identical(levels(as.factor(AwayTeam_ratings$AwayTeam)),levels(as.factor(S2019_20$AwayTeam)))


#As the names of the clubs were not identical we need to homogenize the club names in both datasets
x <- levels(as.factor(AwayTeam_ratings$AwayTeam))
y <- levels(as.factor(S2019_20$AwayTeam))

#Identify the club names missing in A
x[!(x%in%y)]


#Homogenize the club names in the FIFA ratings and 2019-20 results
AwayTeam_ratings <- AwayTeam_ratings %>% mutate(AwayTeam = str_replace_all(AwayTeam_ratings$AwayTeam, "Man Utd", "Man United")) 
AwayTeam_ratings_N <- AwayTeam_ratings %>% mutate(AwayTeam = str_replace_all(AwayTeam_ratings$AwayTeam, "Spurs", "Tottenham" ))

S2019_20_AT <- S2019_20 %>% mutate(AwayTeam = str_replace_all(S2019_20$AwayTeam, "Sheffield United","Sheffield Utd."))

identical(levels(as.factor(AwayTeam_ratings_N$AwayTeam)),levels(as.factor(S2019_20_AT$AwayTeam)))


# Integration of AwayTeam's player's rating to results
Results_AwayRatings <- left_join(S2019_20_AT, AwayTeam_ratings_N , by= "AwayTeam" )

#columns reorganization to facilitate further calculations 
RA_num <- Results_AwayRatings  %>% select_if(is.numeric)%>% colnames() #select all the numeric columns
RA_char <- Results_AwayRatings  %>% select_if(is.character)%>% colnames() # select all the character columns
Results_AwayRatings_c <- Results_AwayRatings [,c(RA_char, RA_num )]  #organize the columns locating the character columns first followed by the numeric

 
#add summarizing variables
Results_AwayRatings_c <- Results_AwayRatings_c %>%  
  mutate(Pace=Acceleration+Sprint_Speed, 
         Shooting= Attacking_Finishing+Long_Shots+Penalities+Positioning+Shot_Power+Attacking_Volleys,
         Passing=Attacking_Crossing + Curve + FK_Accuracy + Long_Passing + Attacking_Short_Passing + Vision,
         Dribbling2= Agility + Balance + Ball_Control + Composure + Dribbling1 + Reactions,
         Defending= Attacking_Heading_Accuracy + Interceptions + Marking + Sliding_Tackle + Standing_Tackle,
         Physical= Aggression + Jumping + Stamina + Strength, 
         Goalkeeping= GK_Diving2 + GK_Handling2 + GK_Kicking2 + GK_Positioning2 + GK_Reflexes2)


#correlation matrix using player ratings
Cor_play_Away<- cor(Results_AwayRatings_c[28:104])

#correlation matrix plot
corrplot(Cor_play_Away, method="color", type = "lower", tl.cex = 0.7 )


#as the correlation between the FIFA ratings and the full time away team goals is low,
# I will explore the team ratings by averaging the ratings for each away team

# summary of fifa ratings by AwayTeam
Club_Away_summary <- Results_AwayRatings_c %>% group_by(AwayTeam)%>% 
  summarise_at(vars(27:103), mean) %>% data.frame()

#calculation of correlation matrix
ATR<- cor(Club_Away_summary[2:78])

#correlation plot using Away team ratings
corrplot(ATR, method="color", type = "lower", tl.cex = 0.7 )

########################################################################################
#Home and away data set integration
#######################################################################################

#integration of results with summarised datasets
HomeClub_summary <- HomeClub_summary %>% select(1, 18:78) #remove the summarized results from homeclub
Club_Away_summary <- Club_Away_summary %>% select(1, 18:78) #remove the summarized results from awayclub

results_summary_club_home <- left_join(S2019_20_N, HomeClub_summary, "HomeTeam" ) # hometeam joined dataset
RSA_num <- results_summary_club_home  %>% select_if(is.numeric)%>% colnames() #select all the numeric columns
RSA_char <- results_summary_club_home  %>% select_if(is.character)%>% colnames() # select all the character columns
results_summary_club_home <- results_summary_club_home [,c(RSA_char, RSA_num )]  #organize the columns locating the character columns first followed by the numeric

results_summary_club_Away <- left_join(S2019_20_AT, Club_Away_summary, "AwayTeam") #awayteam joined dataset
RSH_num <- results_summary_club_Away %>% select_if(is.numeric)%>% colnames() #select all the numeric columns
RSH_char <- results_summary_club_Away  %>% select_if(is.character)%>% colnames() # select all the character columns
results_summary_club_Away <- results_summary_club_Away [,c(RSH_char, RSH_num )]  #organize the columns locating the character columns first followed by the numeric


#Whole data set integration
all<- cbind(results_summary_club_home, results_summary_club_Away[24:84]) 

#renaming columns with duplicated names
colnames(all)[85:145]<-paste("Away", colnames(all)[85:145],sep="_" ) #add away to the away ratings columns


#columns reorganization to facilitate further calculations 
all_num <- all %>% select_if(is.numeric)%>% colnames() #select all the numeric columns
all_char <- all  %>% select_if(is.character)%>% colnames() # select all the character columns
all <- all [,c(all_char, all_num )]  #organize the columns locating the character columns first followed by the numeric



#calculation of correlation matrix
home_away_cor <- cor(all[8:145])

#correlation plot using home and away 
corrplot(home_away_cor, method="color", type = "lower", tl.cex = 0.7 )


#As both home team rating and away team ratings are correlated with the full time home goals and full time away goals
#I will evaluate different prediction model to predict the final results using the average fifa team ratings


####################################################################################################
#PREDICTION MODELS
####################################################################################################

# away ratings training and test data sets
set.seed(1, sample.kind = "Rounding") 
test_index_away <- createDataPartition(results_summary_club_Away$FTR, times = 1, p = 0.2, list = FALSE) # create a 20% test set
test_set_away <- results_summary_club_Away[test_index_away,]
train_set_away <- results_summary_club_Away[-test_index_away,]

# Home ratings training and test data sets
set.seed(1, sample.kind = "Rounding") 
test_index_home <- createDataPartition(results_summary_club_home$FTR, times = 1, p = 0.2, list = FALSE) # create a 20% test set
test_set_home <- results_summary_club_home[test_index_home,]
train_set_home <- results_summary_club_home[-test_index_home,]

# Home and away ratings training and test data sets
set.seed(1, sample.kind = "Rounding") 
test_index_all <- createDataPartition(all$FTR, times = 1, p = 0.2, list = FALSE) # create a 20% test set
test_set_all <- all[test_index_all,]
train_set_all <- all[-test_index_all,]

############################################################################
#Guessing the results model
##########################################################################

# A model based on guessing the result using the away data set and montecarlo simulation
B <- 10^5
set.seed(1, sample.kind = "Rounding")
Away_guess <- replicate(B, {
  y_hat_guess_a <- sample(c("H", "A", "D"), length(test_index_away), replace = TRUE)
  mean(y_hat_guess_a == test_set_away$FTR)
  })

GA<- mean(Away_guess)  #mean accuracy of the simulation
GAmax <- max(Away_guess) #max accuracy obtained in the simulation
GAmin <- min(Away_guess) #min accuracy obtained in the simulation


# A model based on guessing the result using the Home data set and montecarlo simulation
B <- 10^5
set.seed(1, sample.kind = "Rounding")
Home_guess <- replicate(B, {
  y_hat_guess_h <- sample(c("H", "A", "D"), length(test_index_home), replace = TRUE)
  mean(y_hat_guess_h == test_set_home$FTR)
  })
GH<- mean(Home_guess)  # mean accuracy of the simulation
GHmax <- max(Home_guess) # max accuracy obtained in the simulation
GHmin <- min(Home_guess) #min accuracy obtained in the simulation

# A model based on guessing the result using the home and away data set and montecarlo simulation
B <- 10^5
set.seed(1, sample.kind = "Rounding")
All_guess <- replicate(B, {
  y_hat_guess_all <- sample(c("H", "A", "D"), length(test_index_all), replace = TRUE)
  mean(y_hat_guess_all == test_set_all$FTR)
  })

GAll <- mean(All_guess) # mean accuracy of the simulation
GAllmax <- max(All_guess) # max accuracy obtained in the simulation
GAllmin <- min(All_guess)#min accuracy obtained in the simulation

# a table including the accuracy of the three guess models from the three data sets
data.frame(Model=c("Away Team Ratings data", "Home Team Ratings data", "Home and Away Ratings data"),
          Mean_Accuracy=c(GA, GH,GAll), MaxAccuracy=c(GAmax, GHmax,GAllmax),
       MinAccuracy=c(GAmin, GHmin,GAllmin), MaxAccuracy_prob=c(mean(str_detect(Away_guess, "0.5641026")), 
                                                              mean(str_detect(Home_guess, "0.5641026")),
                                                              mean(str_detect(All_guess, "0.5641026"))),
       MinAccuracy_prob=c(mean(str_detect(Away_guess, "0.1153846")), 
                         mean(str_detect(Home_guess, "0.1153846")),
                         mean(str_detect(All_guess, "0.1153846"))))


##########################################################################
#LDA model
##########################################################################

# Model using only the away ratings and lda
train_lda1 <- train(train_set_away[25:84], train_set_away$FTR, method = "lda", data = train_set_away )
y_hat_aw <- predict(train_lda1, test_set_away )
Away_lda_ac <- confusionMatrix(data = y_hat_aw, reference = as.factor(test_set_away$FTR))$overall["Accuracy"]
Away_lda_var <- data.frame(confusionMatrix(data = y_hat_aw, reference = as.factor(test_set_away$FTR))$byClass)%>%
  select(Sensitivity, Specificity, Precision, Balanced.Accuracy, Prevalence )
Awayldasum <-tibble(Dataset= "LDA - Away Team Ratings data", Match_result=c("A","D","H"), Away_lda_var )

# Model using only the home ratings and lda
train_lda_hom <- train(train_set_home[25:84], train_set_home$FTR, data =train_set_home, method = "lda")
y_hat_hom <- predict(train_lda_hom, test_set_home )
Home_lda_ac <-confusionMatrix(data = y_hat_hom, reference = as.factor(test_set_home$FTR))$overall["Accuracy"]
Home_lda_var <-data.frame(confusionMatrix(data = y_hat_hom, reference = as.factor(test_set_home$FTR))$byClass)%>%
                            select(Sensitivity, Specificity, Precision, Balanced.Accuracy, Prevalence )
Homeldasum <-tibble(Dataset= "LDA-Home Team Ratings data", Match_result=c("A","D","H"), Home_lda_var )

#varImp(train_lda_hom)

# Model using only the home and away ratings and lda

train_lda_all <- train(train_set_all[43:145], train_set_all$FTR, data =train_set_all, method = "lda")
y_hat_all <- predict(train_lda_all, test_set_all )
All_lda_ac <-confusionMatrix(data = y_hat_all, reference = as.factor(test_set_all$FTR))$overall["Accuracy"]
All_lda_var <- data.frame(confusionMatrix(data = y_hat_all, reference = as.factor(test_set_all$FTR))$byClass)%>%
                            select(Sensitivity, Specificity, Precision, Balanced.Accuracy, Prevalence )
Alldasum <-tibble(Dataset= "LDA - Home and Away Team Ratings data", Match_result=c("A","D","H"), All_lda_var ) #model information table

#LDA models accuracy table
LDAAccu<- tibble(Model=c("LDA-Away Team Ratings data", "LDA-Home Team Ratings data", "LDA-Home and Away Ratings data"),
       Accuracy= c(Away_lda_ac,Home_lda_ac,All_lda_ac))
LDAAccu %>% knitr::kable() 

#model summarizing table
LDA_summary <- bind_rows(Awayldasum, Homeldasum, Alldasum)
LDA_summary %>% knitr::kable()


#As all the lda model generated a warning associated with colinnearity among the variables 
#I will remove the predictor variables that are highly correlated between them

##########################################################################
#nocolinear lda model
###############################################################################

#REMOVING COLLINEARITY FROM THE DATA SETS

#removing collinearity from the away data set
w <- which( ATR>0.95 & ATR<1, arr.ind = TRUE) # selection of correlation between 0.95 and 1 in the covarience matrix of the away data set
H <- w %>% as.data.frame() %>% group_by(col)%>% summarise(n=n()) %>% arrange(col) %>% data.frame() # identify the column number in the correlation matrix with collinearity
Ss<-H$col+ (ncol(results_summary_club_Away)-ncol(ATR))  # identification of the columns number in the results data set using the information from the correlation matrix
Away_nocolinear_data <- results_summary_club_Away%>% select(!Ss) #removing colinear columns
Cor_play_Awaynocol<- cor(Away_nocolinear_data[8:ncol(Away_nocolinear_data)]) #correlation matrix
corrplot(Cor_play_Awaynocol, method="color", type = "lower", tl.cex = 0.7 )  #correlation plot

#removing collinearity from the home data set
hn <- which( HTR>0.95 & HTR<1, arr.ind = TRUE) # selection of correlation between 0.95 and 1 in the covarience matrix of the home data set
HN<- hn %>% as.data.frame() %>% group_by(col)%>% summarise(n=n()) %>% arrange(col) %>% tibble() # identify the column number in the correlation matrix with collinearity
Ssn<-HN$col+7 # identification of the columns number in the results data set using the information from the correlation matrix
Home_nocolinear_data <- results_summary_club_home%>% select(!Ssn) #removing colinear columns
Cor_play_homenocol<- cor(Home_nocolinear_data[8:ncol(Home_nocolinear_data)]) #correlation matrix
corrplot(Cor_play_homenocol, method="color", type = "lower", tl.cex = 0.7 ) #correlation plot

#removing colinearity from the home and away data set
an <- which( home_away_cor>0.95 & home_away_cor<1, arr.ind = TRUE) # selection of correlation between 0.95 and 1 in the covarience matrix of the home data set
aN<- an %>% as.data.frame() %>% group_by(col)%>% summarise(n=n()) %>% arrange(col) %>% tibble() # identify the column number in the correlation matrix with collinearity
Asn<-aN$col+7 # identification of the columns number in the results data set using the information from the correlation matrix
all_nocolinear_data <- all%>% select(!Asn)#removing colinear columns
Cor_play_allnocol<- cor(all_nocolinear_data[8:ncol(all_nocolinear_data)]) #correlation matrix
corrplot(Cor_play_allnocol, method="color", type = "lower", tl.cex = 0.7 )#correlation plot


#evaluating the lda model using the noncolinear away data set
set.seed(1, sample.kind = "Rounding") 
test_index_nocol <- createDataPartition(Away_nocolinear_data$FTR, times = 1, p = 0.2, list = FALSE) # create a 20% test set
test_set_away_ncol <- Away_nocolinear_data[test_index_nocol,] #non-collinear test set
train_set_away_ncol <- Away_nocolinear_data[-test_index_nocol,] # non-collinear train test
train_lda11 <- train(train_set_away_ncol[25:ncol(Away_nocolinear_data)], train_set_away_ncol$FTR, 
                     method = "lda", data = train_set_away_ncol )
y_hat_aw1 <- predict(train_lda11, test_set_away_ncol  )
Awaynoc_lda_ac <-confusionMatrix(data = y_hat_aw1, reference = as.factor(test_set_away_ncol$FTR))$overall["Accuracy"]
Awaynoc_lda_var <-data.frame(confusionMatrix(data = y_hat_aw1, reference = as.factor(test_set_away_ncol$FTR))$byClass)%>%
                               select(Sensitivity, Specificity, Precision, Balanced.Accuracy, Prevalence )
Awaynoc_ldasum <-tibble(Dataset= "LDA-No colinear Away Team Ratings data", Match_result=c("A","D","H"), Awaynoc_lda_var ) #model information table


#evaluating the lda model using the noncolinear home data set
set.seed(1, sample.kind = "Rounding") 
test_index_hnc <- createDataPartition(Home_nocolinear_data$FTR, times = 1, p = 0.2, list = FALSE) # create a 20% test set
test_set_home_hnc <- Home_nocolinear_data[test_index_hnc,]
train_set_home_hnc <- Home_nocolinear_data[-test_index_hnc,]
train_lda2 <- train(train_set_home_hnc[25:ncol(Home_nocolinear_data)], train_set_home_hnc$FTR,
                    method = "lda", data = train_set_home_hnc )
y_hat_hom <- predict(train_lda2, test_set_home )
Homenoc_lda_ac <- confusionMatrix(data = y_hat_hom, reference = as.factor(test_set_home_hnc$FTR))$overall["Accuracy"]
Homenoc_lda_var <- data.frame(confusionMatrix(data = y_hat_hom, reference = as.factor(test_set_home_hnc$FTR))$byClass)%>% 
                                select(Sensitivity, Specificity, Precision, Balanced.Accuracy, Prevalence )
Homenoc_ldasum <-tibble(Dataset= "LDA-No colinear Home Team Ratings data", 
                        Match_result=c("A","D","H"), Homenoc_lda_var ) #model information table


#evaluating the lda model using the noncolinear home and away data set
set.seed(1, sample.kind = "Rounding") 
test_index <- createDataPartition(all_nocolinear_data $FTR, times = 1, p = 0.2, list = FALSE) # create a 20% test set
test_set_all <- all_nocolinear_data [test_index,]
train_set_all <- all_nocolinear_data [-test_index,]
train_lda3 <- train(train_set_all[25:ncol(all_nocolinear_data)], train_set_all$FTR, method = "lda", data = train_set_away )
y_hat_all <- predict(train_lda3, test_set_all )
Allnoc_lda_ac <- confusionMatrix(data = y_hat_all, reference = as.factor(test_set_all$FTR))$overall["Accuracy"]
Allnoc_lda_var <- data.frame(confusionMatrix(data = y_hat_all, reference = as.factor(test_set_all$FTR))$byClass)%>%
                               select(Sensitivity, Specificity, Precision, Balanced.Accuracy, Prevalence )
Allnoc_ldasum <-tibble(Dataset= "LDA-No colinear Home and Away Team Ratings data", 
                        Match_result=c("A","D","H"), Allnoc_lda_var ) #model information table

#no collinear LDA models accuracy table
LDAAccu<- tibble(Model=c("Non collinear LDA - Away Team Ratings data", 
                         "Non collinear LDA - Home Team Ratings data", 
                         "Non collinear LDA - Home and Away Ratings data"),
                 Accuracy= c(Awaynoc_lda_ac,Homenoc_lda_ac,Allnoc_lda_ac))
LDAAccu %>% knitr::kable() 

#model summarizing table
LDA_summary <- bind_rows(Awaynoc_ldasum, Homenoc_ldasum, Allnoc_ldasum)
LDA_summary %>% knitr::kable()


################################################
#knn models 
################################################

# knn model with away data
set.seed(1, sample.kind = "Rounding") # simulate R 3.5
tuning <- data.frame(k = seq(3, 55, 1))
awaytrain_knn <- train(train_set_away[25:ncol(train_set_away)], train_set_away$FTR, 
                   method = "knn", tuneGrid = tuning )
awayknn_preds <- predict(awaytrain_knn, test_set_away)
awaytrain_knn$bestTune
away_knn_ac <- confusionMatrix(data = awayknn_preds, reference = as.factor(test_set_away$FTR))$overall["Accuracy"]
away_knn_var <-data.frame(confusionMatrix(data = awayknn_preds, reference = as.factor(test_set_away$FTR))$byClass)%>%
  select(Sensitivity, Specificity, Precision, Balanced.Accuracy, Prevalence)
away_knnsum <-tibble(Dataset= "KNN model - Away Team Ratings data", 
                    Match_result=c("A","D","H"),away_knn_var ) #model information table

# knn model with home data
set.seed(1, sample.kind = "Rounding") # simulate R 3.5
tuning <- data.frame(k = seq(3, 55, 1))
hometrain_knn <- train(train_set_home[25:ncol(train_set_home)], train_set_home$FTR, 
                       method = "knn", tuneGrid = tuning )
homeknn_preds <- predict(hometrain_knn, test_set_home)
hometrain_knn$bestTune
home_knn_ac <- confusionMatrix(data = homeknn_preds, reference = as.factor(test_set_home$FTR))$overall["Accuracy"]
home_knn_var <-data.frame(confusionMatrix(data = homeknn_preds, reference = as.factor(test_set_home$FTR))$byClass)%>%
  select(Sensitivity, Specificity, Precision, Balanced.Accuracy, Prevalence)
home_knnsum <-tibble(Dataset= "KNN model - Home Team Ratings data", 
                     Match_result=c("A","D","H"),home_knn_var ) #model information table

# knn model with home and away data
set.seed(1, sample.kind = "Rounding") # simulate R 3.5
tuning <- data.frame(k = seq(3, 55, 1))
train_knn <- train(train_set_all[25:ncol(train_set_all)], train_set_all$FTR, 
                   method = "knn", tuneGrid = tuning )
knn_preds <- predict(train_knn, test_set_all)
train_knn$bestTune
All_knn_ac <- confusionMatrix(data = knn_preds, reference = as.factor(test_set_all$FTR))$overall["Accuracy"]
All_knn_var <-data.frame(confusionMatrix(data = knn_preds, reference = as.factor(test_set_all$FTR))$byClass)%>%
                           select(Sensitivity, Specificity, Precision, Balanced.Accuracy, Prevalence)
All_knnsum <-tibble(Dataset= "KNN model - Home and Away Team Ratings data", 
                    Match_result=c("A","D","H"), All_knn_var ) #model information table

# knn  models accuracy table
knn_Accu<- tibble(Model=c("knn - Away Team Ratings data", 
                         "knn - Home Team Ratings data", 
                         "knn - Home and Away Ratings data"),
                 Accuracy= c(away_knn_ac,home_knn_ac,All_knn_ac))
knn_Accu %>% knitr::kable() 

# knn models summarizing table
knn_summary <- bind_rows(away_knnsum , home_knnsum, All_knnsum )
knn_summary %>% knitr::kable()

#####################################################################################
#RANDOM FOREST models, this section is a little slow to run it will take around 5 min
#####################################################################################

#model using away team ratings data set and random forest
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,15) , predFixed = c(10, 15, 25, 35, 50))
awaytrain_rf <-  train(train_set_away[25:ncol(train_set_away)], train_set_home$FTR,
                       method = "Rborist",
                       nTree = 50,
                       trControl = control,
                       tuneGrid = grid,
                       nSamp = 5000) #model training

ggplot(awaytrain_rf) #PLOT
awaytrain_rf$bestTune$minNode #BEST MIN NODE
awaytrain_rf$bestTune$predFixed # BEST PREDFIXED

awaypred_rf <- Rborist(train_set_away[25:ncol(train_set_away)], as.factor(train_set_away$FTR),
                       nTree = 1000,
                       minNode = awaytrain_rf$bestTune$minNode,
                       predFixed = awaytrain_rf$bestTune$predFixed) #model prediction

y_hat_awayrf <- predict(awaypred_rf, test_set_away[25:ncol(test_set_away)])$yPred
away_RF_ac <-confusionMatrix(data = y_hat_awayrf, reference = as.factor(test_set_away$FTR))$overall["Accuracy"]
away_RF_var <- data.frame(confusionMatrix(data = y_hat_awayrf, reference = as.factor(test_set_away$FTR))$byClass)%>%
  select(Sensitivity, Specificity, Precision, Balanced.Accuracy, Prevalence)
away_RFsum <-tibble(Dataset= "RF model - Away Team Ratings data", 
                    Match_result=c("A","D","H"), away_RF_var ) #model information table



#model using home team ratings data set and random forest
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,15) , predFixed = c(10, 15, 25, 35, 50))
hometrain_rf <-  train(train_set_home[25:ncol(train_set_home)], train_set_home$FTR,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000) #model training

ggplot(hometrain_rf) #PLOT
hometrain_rf$bestTune$minNode #BEST MIN NODE
hometrain_rf$bestTune$predFixed # BEST PREDFIXED

homepred_rf <- Rborist(train_set_home[25:ncol(train_set_home)], as.factor(train_set_home$FTR),
                   nTree = 1000,
                   minNode = hometrain_rf$bestTune$minNode,
                   predFixed = hometrain_rf$bestTune$predFixed) #model prediction

y_hat_homerf <- predict(homepred_rf, test_set_home[25:ncol(test_set_home)])$yPred
home_RF_ac <-confusionMatrix(data = y_hat_homerf, reference = as.factor(test_set_home$FTR))$overall["Accuracy"]
home_RF_var <- data.frame(confusionMatrix(data = y_hat_homerf, reference = as.factor(test_set_home$FTR))$byClass)%>%
  select(Sensitivity, Specificity, Precision, Balanced.Accuracy, Prevalence)
home_RFsum <-tibble(Dataset= "RF model - Home Team Ratings data", 
                   Match_result=c("A","D","H"), home_RF_var ) #model information table


#model using home and away ratings data set and random forest
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,15) , predFixed = c(10, 15, 25, 35, 50))
train_rf <-  train(train_set_all[25:ncol(train_set_all)], train_set_all$FTR,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000) #model training

ggplot(train_rf) #PLOT
train_rf$bestTune$minNode #BEST MIN NODE
train_rf$bestTune$predFixed # BEST PREDFIXED

pred_rf <- Rborist(train_set_all[25:ncol(train_set_all)], as.factor(train_set_all$FTR),
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed) #model prediction

y_hat_rf <- predict(pred_rf, test_set_all[25:ncol(train_set_all)])$yPred
All_RF_ac <-confusionMatrix(data = y_hat_rf, reference = as.factor(test_set_all$FTR))$overall["Accuracy"]
All_RF_var <- data.frame(confusionMatrix(data = y_hat_rf, reference = as.factor(test_set_all$FTR))$byClass)%>%
                           select(Sensitivity, Specificity, Precision, Balanced.Accuracy, Prevalence)
All_RFsum <-tibble(Dataset= "RF model - Home and Away Team Ratings data", 
                    Match_result=c("A","D","H"), All_RF_var ) #model information table




# RAndom forest  models accuracy table
RF_Accu<- tibble(Model=c("RF - Away Team Ratings data", 
                          "RF - Home Team Ratings data", 
                          "RF - Home and Away Ratings data"),
                  Accuracy= c(away_RF_ac,home_RF_ac,All_RF_ac))
RF_Accu %>% knitr::kable() 

# random forest models summarizing table
RF_summary <- bind_rows(away_RFsum , home_RFsum, All_RFsum )
RF_summary %>% knitr::kable()


################################################
#final summarizing table
################################################

#as the data set including home and away ratings data set performed the best I compared the 4 methods used with that data set

#home and away accuracy
whole_Accu<- tibble(Model=c( "Guess Model - Home and Away Ratings data",
                             "LDA-Home and Away Ratings data",
                             "Non collinear LDA - Home and Away Ratings data",
                             "knn - Home and Away Ratings data", 
                             "RF - Home and Away Ratings data"), 
                    Accuracy= c(GAll,All_lda_ac, Allnoc_lda_ac,All_knn_ac,All_RF_ac))
whole_Accu %>% knitr::kable()

# random forest models summarizing table
whole_summary <- bind_rows( Alldasum, Allnoc_ldasum, All_knnsum, All_RFsum )
whole_summary %>% knitr::kable()



