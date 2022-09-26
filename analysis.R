library(tidyverse)
library(data.table)
library(Hmisc)

ratings <- read.csv("ratingsdump-clbsc.csv",  encoding = "UTF-8")
leagues <- read.csv("leagues.csv", encoding = "UTF-8") %>%
  rename(LgLvl = X.U.FEFF.LgLvl)
teams <- read.csv("teams.csv", encoding = "UTF-8")

joined_ratings <- ratings %>%
  left_join(., leagues, by = "LgLvl") %>%
  left_join(., teams, by = c("Team" = "ID")) %>%
  rename(Name = Name.x,
         Tm.Name = Name.y) %>%
  left_join(., teams %>% mutate(Org.Name = paste(Name, Nickname)) %>% select(ID, Org.Name), by = c("Parent.Team.ID" = "ID")) %>%
  replace_na(list(LgName = "Free Agent"))

## potential not really great, would want to look at how much potential left?
## Pitchers not high enough, seperate out hitting/fielding form pitchers
z_scores <- joined_ratings %>%
  select(ID, LgName, Cntct, Gap, Pow, Eye, Ks, PotCntct, PotGap, PotPow, 
         PotEye, PotKs, Speed, Steal, Run, SacBunt, BuntHit, IFR, IFE, IFA, TDP,
         OFR, OFE, OFA, CAb, CArm, Stf, Mov, Ctrl, PotStf, PotMov, PotCtrl) %>%
  mutate(potential_hit_overhead = (PotCntct - Cntct) + (PotPow - Pow) + (PotEye - Eye) + (PotKs - Ks),
         potential_pitching_overhead = (PotStf - Stf) + (PotMov - Mov) + (PotCtrl - Ctrl)) %>%
  #group_by(LgName) %>%
  mutate(across(Cntct:potential_pitching_overhead, scale))%>%
  rowwise() %>% 
  mutate(current_hit_value = sum(c(Cntct, Pow, Eye, Ks)),
         current_fielding_value = sum(c(IFR, IFE, IFA, TDP, OFR, OFE, OFA, CAb, CArm)),
         current_pitching_value = sum(c(Stf, Mov, Ctrl))) %>%
  left_join(., joined_ratings %>% select(ID, Name), by ="ID") %>%
  select(ID, Name, LgName, current_hit_value, current_fielding_value, potential_hit_overhead, current_pitching_value, potential_pitching_overhead) %>%
  mutate(current_hit_sd_away_num = round((current_hit_value - mean(.$current_hit_value)) / sd(.$current_hit_value),0),
         current_field_sd_away_num = round((current_fielding_value - mean(.$current_fielding_value)) / sd(.$current_fielding_value),0),
         current_pitch_sd_away_num = round((current_pitching_value - mean(.$current_pitching_value)) / sd(.$current_pitching_value),0),
         hit_bucket = case_when(
           current_hit_sd_away_num == 4 ~ "Elite",
           current_hit_sd_away_num == 3 ~ "Great",
           current_hit_sd_away_num == 2 ~ "Good",
           current_hit_sd_away_num == 1 ~ "Okay",
           current_hit_sd_away_num == 0 ~ "Average",
           current_hit_sd_away_num == -1 ~ "Below Average"
         ),
         field_bucket = case_when(
           current_field_sd_away_num == 4 ~ "Elite",
           current_field_sd_away_num == 3 ~ "Great",
           current_field_sd_away_num == 2 ~ "Good",
           current_field_sd_away_num == 1 ~ "Okay",
           current_field_sd_away_num == 0 ~ "Average",
           current_field_sd_away_num == -1 ~ "Below Average"
         ),
         pitch_bucket = case_when(
           current_pitch_sd_away_num == 4 ~ "Elite",
           current_pitch_sd_away_num == 3 ~ "Great",
           current_pitch_sd_away_num == 2 ~ "Good",
           current_pitch_sd_away_num == 1 ~ "Okay",
           current_pitch_sd_away_num == 0 ~ "Average",
           current_pitch_sd_away_num == -1 ~ "Below Average"
         ))

#how many SD away from mean is a way to bucket?
## (observation - mean) / sd

league_pos_avg

#look at multi-position
#look at multi-pitch
# keep velocity

Name, Age, Height, Bats, Throws,  Tm.Name, Nickname, Org.Name,