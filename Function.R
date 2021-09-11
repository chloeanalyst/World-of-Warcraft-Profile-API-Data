
#--------------------------------FUNCTION ONE ---------------------------------- GET data from World of Warcraft API.

get.raider.stats <- function(Realm, Raider, access_token){
  
                                profile_data = GET(paste0('https://eu.api.blizzard.com/profile/wow/character/',Realm,'/',Raider,'?namespace=profile-eu&locale=en_GB',access_token))   
                                data = fromJSON(rawToChar(profile_data$content))
                                
                                PVP = GET(paste0(data$pvp_summary$href,access_token))
                                PVP_Stat = fromJSON(rawToChar(PVP$content))
                                
                                PVP_Last = GET(paste0(PVP_Stat$brackets$href[3],access_token))
                                PVP_Last_Data = fromJSON(rawToChar(PVP_Last$content))
  

#------------------------------------------------------------------------------- Collect relevant data.
  
                                GuildName <- data$guild$name 
                                Player <- data$name
                                Race <- data$race$name
                                CharacterClass <- data$character_class$name
                                ActiveSpec <- data$active_spec$name
                                ItemLevel <- data$average_item_level
                                RBG_Rating <- PVP_Last_Data$rating
                                RBG_Played <- PVP_Last_Data$season_match_statistics$played
                                RBG_Won <- PVP_Last_Data$season_match_statistics$won
                                RBG_Lost <- PVP_Last_Data$season_match_statistics$lost
                                
#------------------------------------------------------------------------------- Combine into one table.
    
                                Output <- data.frame(GuildName,
                                                     Player,
                                                     Race,
                                                     CharacterClass,
                                                     ActiveSpec,
                                                     ItemLevel,
                                                     RBG_Rating,
                                                     RBG_Played,
                                                     RBG_Won,
                                                     RBG_Lost)
                                
                                return(Output)
                                                              
                                                              }


#--------------------------------FUNCTION TWO ---------------------------------- User facing function, clean data and create final output.

rbg.stats <- function(raiders, realm, token) {
  
                                               results <- list()
                                              
                                               for (i in raiders){
                                                
                                                                  results[[i]] <- get.raider.stats(realm,i,token)
                                              
                                                                  }
  
  
                                                results <- do.call(rbind,results)
  
  #------------------------------------------------------------------------------- Create player stats.
  
                                                results$WinRate <- round(results$RBG_Won/results$RBG_Played*100,2)
                                                results$Experience <- ifelse(results$RBG_Played > mean(results$RBG_Played),1,0)
                                                results$WinningPlayer <- ifelse(results$WinRate > mean(results$WinRate),1,0)
                                                results$HighRated <- ifelse(results$RBG_Rating > mean(results$RBG_Rating),1,0)
                                                results$HighItemLevel <- ifelse(ItemLevel > mean(ItemLevel),1,0)
                                                
                                                results$Score = rowSums(results[,c(12,13,14,15)])
                                                
                                                results <- results %>% select(c(1:11,16))
                                                
                                                return(results)
  
                                              }
  