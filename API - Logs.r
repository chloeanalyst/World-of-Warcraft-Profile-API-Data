
library(wowr)
library(httr)
library(jsonlite)
library(dplyr)

##########
#Set up 
#########



#token <-  wow_auth(client_id = 'be67b77d7d2046deb63cd8835238e688',
#          client_secret = 'fIH54ogLASDcybkoCv1x0tx3g4f4UvgQ',
#         region = 'EU')

#Link for manual token - https://develop.battle.net/documentation/world-of-warcraft/profile-apis

manual_token <- '&access_token=USwhepA3fgP0jWSyBbBIpQRoQgj9vMfmZM'


#################
### Raider Data
#################


Realm <- 'kazzak'
#Raider <- 'peakyskengz'

Raider_List <- list('peakyskengz',
                    'amphali',
                    'pejn',
                    'busko',
                    'fionah',
                    'heraphim',
                    'kaelthulas',
                    'tamba',
                    'vardoran',
                    'voixe',
                    'moratoria')

##########
## Total BattleGround Stats
##########





#########
## Roster Stats
#########






#Get Requests for all raiders.



Raider_Requests <- list()
Raider_Name <- list()
Raider_Class <- list()
Raider_Race <- list()
Raider_Spec <- list()
Raider_ItemLevel <- list()
PVP_Requests <- list()
PVP_Stats <- list()
BattleGround_Requests <- list()
BattleGround_RecentRequests <- list()
BattleGround_Rating <- list()
BattleGround_Played <- list()
BattleGround_Won <- list()
BattleGround_Lost <- list()
BattleGround_Played_Trend <- list()
BattleGround_Won_Trend <- list()
BattleGround_Lost_Trend <- list()
BattleGround_Map <- list()

#Get Requests for Character Data
for (i in Raider_List) {
  Raider_Requests[[i]] <- GET(paste0('https://eu.api.blizzard.com/profile/wow/character/',Realm,'/',i,'?namespace=profile-eu&locale=en_GB',manual_token))
  }

#Pull Character Details from Request content
for (i in Raider_List){
Raider_Name <- c(Raider_Name,fromJSON(rawToChar(Raider_Requests[[i]]$content))$name)
Raider_Class <- c(Raider_Class,fromJSON(rawToChar(Raider_Requests[[i]]$content))$character_class$name)
Raider_Race <- c(Raider_Race,fromJSON(rawToChar(Raider_Requests[[i]]$content))$race$name)
Raider_Spec <- c(Raider_Spec, fromJSON(rawToChar(Raider_Requests[[i]]$content))$active_spec$name)
Raider_ItemLevel <- c(Raider_ItemLevel, fromJSON(rawToChar(Raider_Requests[[i]]$content))$average_item_level)
}

#Get Requests for PVP

for (i in Raider_List) {
  PVP_Requests[[i]] <- GET(paste0(fromJSON(rawToChar(Raider_Requests[[i]]$content))$pvp_summary$href,manual_token))
}
  
#Pull PVP Stats Content
for (i in Raider_List){
  PVP_Stats[[i]] <- c(PVP_Stats,fromJSON(rawToChar(PVP_Requests[[i]]$content)))
}



#Pull BattleGround Stats Content

for (i in Raider_List) {
  BattleGround_Requests[[i]] <- GET(paste0(fromJSON(rawToChar(Raider_Requests[[i]]$content))$pvp_summary$href,manual_token))
}


#Get Most Recent BattleGround Stats

for (i in Raider_List) {
  BattleGround_RecentRequests[[i]] <- GET(paste0(fromJSON(rawToChar(BattleGround_Requests[[i]]$content))$brackets$href[1],manual_token))
}


#Store BattleGround Stats

for (i in Raider_List) {
  
  BattleGround_Rating <- c(BattleGround_Rating, fromJSON(rawToChar(BattleGround_RecentRequests[[i]]$content))$rating)
  BattleGround_Played <- c(BattleGround_Played, PVP_Stats[[i]]$pvp_map_statistics$match_statistics$played[1])
  BattleGround_Lost <- c(BattleGround_Lost, PVP_Stats[[i]]$pvp_map_statistics$match_statistics$lost[1])
  BattleGround_Won <- c(BattleGround_Won, PVP_Stats[[i]]$pvp_map_statistics$match_statistics$won[1])
}


#Store BattleGround Stats (Trended)

for (i in Raider_List) {
  
  BattleGround_Map[[i]] <- c(BattleGround_Map, PVP_Stats[[i]]$pvp_map_statistics$world_map$name$en_GB)
  BattleGround_Played_Trend[[i]] <- c(BattleGround_Played, PVP_Stats[[i]]$pvp_map_statistics$match_statistics$played)
  BattleGround_Lost_Trend[[i]] <- c(BattleGround_Lost, PVP_Stats[[i]]$pvp_map_statistics$match_statistics$lost)
  BattleGround_Won_Trend[[i]] <- c(BattleGround_Won, PVP_Stats[[i]]$pvp_map_statistics$match_statistics$won)
}



#Unlist data

Raider_Name <- unlist(Raider_Name)
Raider_Class <- unlist(Raider_Class)
Raider_Race <- unlist(Raider_Race)
Raider_Spec <- unlist(Raider_Spec)
Raider_ItemLevel <- unlist(Raider_ItemLevel)
BattleGround_Rating <- unlist(BattleGround_Rating)
BattleGround_Played <- unlist(BattleGround_Played)
BattleGround_Won <- unlist(BattleGround_Won)
BattleGround_Lost <- unlist(BattleGround_Lost)
BattleGround_Played_Trend <- unlist(BattleGround_Played_Trend)
BattleGround_Won_Trend <- unlist(BattleGround_Won_Trend)
BattleGround_Lost_Trend <- unlist(BattleGround_Lost_Trend)
BattleGround_Map <- unlist(BattleGround_Map)


#Combine Lists

RosterStats_Output <- cbind(Raider_Name,
                Raider_Class, 
                Raider_Race,
                Raider_Spec,
                Raider_ItemLevel,
                BattleGround_Rating,
                BattleGround_Played,
                BattleGround_Won,
                BattleGround_Lost
          )


PeakyStats1 <- BattleGround_Played_Trend$peakyskengz
PeakyStats2 <- BattleGround_Won_Trend$peakyskengz
PeakyStats3 <- BattleGround_Lost_Trend$peakyskengz


Test <- data.frame(rbind(PeakyStats1,PeakyStats2,PeakyStats3))





####################

### Single Player Stats

#####################
  
#All profile data

#profile_data = GET(paste0('https://eu.api.blizzard.com/profile/wow/character/',Realm,'/',Raider,'?namespace=profile-eu&locale=en_GB',manual_token))
profile_data = GET('https://eu.api.blizzard.com/profile/wow/character/kazzak/peakyskengz?namespace=profile-eu&locale=en_GB&access_token=USwhepA3fgP0jWSyBbBIpQRoQgj9vMfmZM')
data = fromJSON(rawToChar(profile_data$content))
data

#Guild name
GuildName <- data$guild$name

#Player name

Player <- data$name

#Race
Race <- data$race$name

#CharacterClass
CharacterClass <- data$character_class$name

#ActiveSpec
ActiveSpec <- data$active_spec$name

#Item Level
ItemLevel <- data$average_item_level

#Battle Ground PVP Summary
PVP = GET(paste0(data$pvp_summary$href,manual_token))
PVP_Stat = fromJSON(rawToChar(PVP$content))

PVP_Last = GET(paste0(PVP_Stat$brackets$href[3],manual_token))
PVP_Last_Data = fromJSON(rawToChar(PVP_Last$content))


RBG_Rating <- PVP_Last_Data$rating
RBG_Played <- PVP_Last_Data$season_match_statistics$played
RBG_Won <- PVP_Last_Data$season_match_statistics$won
RBG_Lost <- PVP_Last_Data$season_match_statistics$lost


#Dataframe

Output <- data.frame(GuildName,Player,Race,CharacterClass,ActiveSpec,ItemLevel,RBG_Rating,RBG_Played,RBG_Won,RBG_Lost)

print(Output)

  


