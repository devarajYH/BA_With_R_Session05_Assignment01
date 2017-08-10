###############################################################################################################

#  Creating a fields array (for variable names)

fields<-c("athlete","age","country","year","closing date","sport",
          "gold medals","silver medals","bronz medals","total medals")

# Reading the OLYMPIC data file into R
getwd()
setwd()

olympic.data <-read.csv("olympic_data.csv",stringsAsFactors = F,col.names = fields,sep = "\t")
olympic.data

nrow(olympic.data)
str(olympic.data)

###############################################################################################################

# Qn.1 Consider only those participants who have all the data points

olympic.alldata <- olympic.data[complete.cases(olympic.data),  ]

olympic.alldata

nrow(olympic.alldata)


###############################################################################################################

# Qn.2 Rank the participants in terms : . Swimming . Table Tennis . Shooting . Gymnastics . Total
# Medal

library(reshape2)
library(dplyr)

# Ranking Swimming participants
aggregate(total.medals ~ athlete + sport ,data=olympic.alldata,sum) %>% 
  filter(sport=="Swimming") %>% arrange(-total.medals)


# Ranking Table Tennis participants
aggregate(total.medals ~ athlete + sport ,data=olympic.alldata,sum) %>% 
  filter(sport=="Table Tennis") %>% arrange(-total.medals)


# Ranking Shooting Participants
aggregate(total.medals ~ athlete + sport ,data=olympic.alldata,sum) %>% 
  filter(sport=="Shooting") %>% arrange(-total.medals)


# Ranking Gymnastics Participants
aggregate(total.medals ~ athlete + sport ,data=olympic.alldata,sum) %>% 
  filter(sport=="Gymnastics") %>% arrange(-total.medals)


# Ranking all participants in terms of total medals.
aggregate(total.medals ~ athlete ,data=olympic.alldata,sum) %>% 
  arrange(-total.medals)


###### Automating the above code ######

rank <- function(game){
  aggregate(total.medals ~ athlete + sport ,data=olympic.alldata,sum) %>% 
    filter(sport==game) %>% arrange(-total.medals)  }

rank("Table Tennis")     # rank of Table Tennise Participants
rank()                   # rank of any category participants....


###############################################################################################################

# Qn.3 Rank the Categories in terms of Age.(Higher the Age,Higher the Rank)

# Ans...         METHOD 1
library(reshape2)
library(dplyr)

# Rank Swimming in terms of Age
aggregate(age ~ athlete + sport,data = olympic.alldata,max) %>% 
         filter(sport=="Swimming") %>% arrange(-age)

# Rank Table Tennise in terms of Age
aggregate(age ~ athlete + sport,data = olympic.alldata,max) %>% 
  filter(sport=="Table Tennis") %>% arrange(-age)

# Rank Shooting in terms of Age
aggregate(age ~ athlete + sport,data = olympic.alldata,max) %>% 
  filter(sport=="Shooting") %>% arrange(-age)

# Rank Gymnastics in terms of Age
aggregate(age ~ athlete + sport,data = olympic.alldata,max) %>% 
  filter(sport=="Gymnastics") %>% arrange(-age)


#             METHOD 2
####### Automating the above code into a FUNCTION #######

age.rank <- function(abc){ 
aggregate(age ~ athlete + sport,data = olympic.alldata,max) %>% 
  filter(sport==abc) %>% arrange(sport,-age) }

age.rank("Swimming")  # rank in terms of age for Gymnastics
age.rank()            # rank in terms of age for any category


#              METHOD 3
######## without using dplyr library....

age_rank <- function(game){
    game1 <- olympic.alldata[olympic.alldata$sport== game  ,  ]
    game2 <- aggregate(age ~ athlete+sport, data = game1 ,  max  )
    game_rank <- game2[order(game2$age,decreasing = T)  ,  ]
    rownames(game_rank) <- NULL
    game_rank
}
age_rank("Swimming")        # function to rank the Swimming category
age_rank("Table Tennis")    # function to rank the Table Tennis category
age_rank("Shooting")        # function to rank the Shooting category
age_rank("Gymnastics")      # function to rank the Gymnastics category
age_rank()                  # function to rank any category


###############################################################################################################

# Qn.4. Identify Year wise top participants in terms of : . Swimming . Table Tennis . Shooting 
#       .Gymnastics . Total Medal

# Swimming year wise top participants..
aggregate(total.medals ~ athlete+sport+year,data = olympic.alldata,max) %>% filter(sport=="Swimming") %>% 
                     arrange(year,-total.medals) %>% group_by(year) %>% 

# Table Tennis year wise top participants..
aggregate(total.medals ~ athlete+sport+year,data = olympic.alldata,max) %>% filter(sport=="Table Tennis") %>% 
  arrange(year,-total.medals) %>% group_by(year)

# Shooting year wise top participants..
aggregate(total.medals ~ athlete+sport+year,data = olympic.alldata,max) %>% filter(sport=="Shooting") %>% 
  arrange(year,-total.medals) %>% group_by(year)

# Gymnastics year wise top participants..
aggregate(total.medals ~ athlete+sport+year,data = olympic.alldata,max) %>% filter(sport=="Gymnastics") %>% 
  arrange(year,-total.medals) %>% group_by(year)



######### Automating the above code.#######

yearwise_rank <- function(game){
  
  aggregate(total.medals ~ athlete+sport+year,data = olympic.alldata,max) %>% filter(sport==game) %>% 
    arrange(year,-total.medals) %>% group_by(year)
}

yearwise_rank("Shooting")    # Yearwise top participants for Shooting..

yearwise_rank()              # Yearwise top participants for ANY CATEGORY..


###############################################################################################################

