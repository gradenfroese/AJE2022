###Diagnostic tool to clean raw paraecologist bushmeat transect data

##### clean up

rm(list = ls())

#create not in

'%ni%' <- Negate('%in%')

###load packages

library("tidyverse")
library("lubridate")
library("janitor")
library("stringi")

####STEP 1: IMPORTING AND MERGING ALL THE DATA

##get metadata (and turn characters into factors)

species_meta <- readRDS("./inputs/meta/species_meta_traits.rds")

village_meta <- readRDS("./inputs/meta/project_villages.rds")

work_days <- read_csv("./inputs/meta/work_days_months.csv",
                      locale = readr::locale(encoding = "latin1")) %>%
  mutate_if(is.character,funs(factor(.))) %>%
  filter(session < 15)

work_days$date <- dmy(work_days$date)

###import effort as individual csvs

effort_sessions <- dir("./inputs/raw_transects/effort", 
                         pattern = "\\.csv$", full.names = TRUE)

effort_sessions_list <- vector("list", length(effort_sessions))

###tell R very explicitly how you want each column to be formatted
###it does NOT like doing this directly with the date column, so...
##...you roundabout do it after using lubridate
##note that if package "scales" is loaded it masks readr's "col_factor"
#

for (i in seq_along(effort_sessions)) {
  effort_sessions_list[[i]] <- read_csv(effort_sessions[[i]],
                                        locale = readr::locale(encoding = "latin1"),
                                        col_types = cols(village = readr::col_factor(NULL),
                                                           session = col_integer(),
                                                           date = col_character(),
                                                           'matin/soir' = readr::col_factor(NULL),
                                                           'heure debut' = col_time(),
                                                           'heure fin' = col_time(),
                                                           ME = col_integer(),
                                                           MPE = col_integer(),
                                                           MV = col_integer(),
                                                           M = col_integer(),
                                                           temps = readr::col_factor(NULL)))

  effort_sessions_list[[i]]$date <- dmy(effort_sessions_list[[i]]$date)

}

###check that columns names are the same
#and that they have the same format using the janitor package

compare_df_cols_same(effort_sessions_list, verbose = TRUE)

###put together into one df
##dplyr coercves factors with diff evels into characters
##so tell it to put everything it makes a character a factor again

effort <- bind_rows(effort_sessions_list) %>%
  mutate_if(is.character,funs(factor(.))) %>%
  arrange(session,village,date)

str(effort)

#####add a unique ID by putting together village and date

effort$uniqueID <- as.factor(paste0(effort$village, "_", effort$date))

effort <- effort %>%
  select(uniqueID, everything())

###add a proportion of house entered column
###should this be /M or /(ME + MPE) ?!?!?

effort <- effort %>%
  mutate(M_prop = round(ME/(ME + MPE), 2))

###add the expected number of total maisons..
##..based on the most common number

normal_maisons <- effort %>%
  group_by(village) %>%
  summarize(normal_M = as.integer(names(which.max(table(M)))))

effort <- left_join(effort, normal_maisons, by = "village")

effort$diff_M <- effort$M - effort$normal_M

###add how long people are taking to walk on..
###...1 each transect, 2 on average, and  3 the  difference by transect

effort <- effort %>%
  mutate(walk_mins = as.integer(
    (effort$'heure fin' -
       effort$'heure debut')/60))

effort <- effort %>%
  group_by(village) %>%
  mutate(avg_walk = round(mean(walk_mins, na.rm = TRUE),0))

effort <- effort %>%
  mutate(walk_diff = walk_mins - avg_walk)

###OK MOVING ON TO THE BUSHMEAT

## Import Bushmeat as individual csvs

bushmeat_sessions <- dir("./inputs/raw_transects/bushmeat", 
             pattern = "\\.csv$", full.names = TRUE)

bushmeat_sessions_list <- vector("list", length(bushmeat_sessions))

for (i in seq_along(bushmeat_sessions)) {
  bushmeat_sessions_list[[i]] <- read_csv(bushmeat_sessions[[i]],
                                          locale = readr::locale(encoding = "latin1"))
  
}

###check that columns names are the same
#and that they have the same format using the janitor package

compare_df_cols_same(bushmeat_sessions_list, verbose = TRUE)

###if these are all the same, then the colnames should be the same throughout
#take a look at these to put everything in the format you want

colnames(bushmeat_sessions_list[[1]])

###for loop to make things the same

for (i in seq_along(bushmeat_sessions_list)) {
  
  #currently not doing ANYTHING with heure as R does a good job already with times here
  bushmeat_sessions_list[[i]]$village <- as.factor(bushmeat_sessions_list[[i]]$village)
  bushmeat_sessions_list[[i]]$session <- as.integer(bushmeat_sessions_list[[i]]$session)
  bushmeat_sessions_list[[i]]$date <- dmy(bushmeat_sessions_list[[i]]$date)
  bushmeat_sessions_list[[i]]$especes <- as.factor(bushmeat_sessions_list[[i]]$especes)
  bushmeat_sessions_list[[i]]$etat <- as.factor(bushmeat_sessions_list[[i]]$etat)
  bushmeat_sessions_list[[i]]$nombre <- as.integer(bushmeat_sessions_list[[i]]$nombre)
  bushmeat_sessions_list[[i]]$endroit <- as.factor(bushmeat_sessions_list[[i]]$endroit)
  bushmeat_sessions_list[[i]]$sex <- as.factor(bushmeat_sessions_list[[i]]$sex)
  bushmeat_sessions_list[[i]]$'obtenu via' <- as.factor(bushmeat_sessions_list[[i]]$'obtenu via')
  bushmeat_sessions_list[[i]]$'obtenu comment' <- as.factor(bushmeat_sessions_list[[i]]$'obtenu comment')
  bushmeat_sessions_list[[i]]$'obtenu ou' <- as.factor(bushmeat_sessions_list[[i]]$'obtenu ou')
  bushmeat_sessions_list[[i]]$but <- as.factor(bushmeat_sessions_list[[i]]$but)
  bushmeat_sessions_list[[i]]$prix <- as.integer(bushmeat_sessions_list[[i]]$prix)
  bushmeat_sessions_list[[i]]$'tue via' <- as.factor(bushmeat_sessions_list[[i]]$'tue via')
  bushmeat_sessions_list[[i]]$'tue quand' <- as.factor(bushmeat_sessions_list[[i]]$'tue quand')
  bushmeat_sessions_list[[i]]$'tue ou' <- as.factor(bushmeat_sessions_list[[i]]$'tue ou')
  bushmeat_sessions_list[[i]]$cote <- as.factor(bushmeat_sessions_list[[i]]$cote)
  bushmeat_sessions_list[[i]]$km <- as.numeric(bushmeat_sessions_list[[i]]$km)
  
}

###check that NOW things are the same 

compare_df_cols_same(bushmeat_sessions_list, verbose = TRUE)

###put together into one df
##dplyr coercves factors with diff evels into characters
##so let's put things back into factors now that they are together

bushmeat <- bind_rows(bushmeat_sessions_list) %>%
mutate_if(is.character,funs(factor(.))) %>%
  arrange(session,village,date)

str(bushmeat)

###add unique ID by combining village and date

bushmeat$uniqueID <- as.factor(paste0(bushmeat$village, "_", bushmeat$date))

bushmeat <- bushmeat %>%
  select(uniqueID, everything())

###STEP 2: CLEANING THE DATA

###NOW WE PERFORM ALL THE CLEANING AND CHECKING OF BUSHMEAT DATA
###FIRST WE REMOVE ANYTHING WE KNOW IS ERRONEOUS 
###THEN WE WILL GO THROUGH COLUMN BY COLUMN TO MAKE SURE EVERYTHING MAKES SENSE
###WE WILL START WTIH ETAT AS THAT IS THE MOST IMPORTANT FOR RESPONSE VARIABLE
###THEN WE WILL DO SPECIES TO AVOID ANY MAKE-BELIEVES
####WE WILL THEN PROCEED TO DATES AS THAT IS CRUCIAL TOO
###THEN WE'LL GO THROUGH ALL THE OTHER COLUMNS GENTLY AND SLOWLY 

###take out erroneous data from both effort and bushmeat (see supplemtal material S4)

bushmeat_raw <- bushmeat
effort_raw <- effort

bushmeat <- bushmeat %>% 
  filter(village == "E10" & session > 12 |
         village == "E4" & session > 12 |
         village == "C2" & session > 2 |
         village %ni% (c("E10",
                         "E4",
                         "C2")) &
           village != "C8")

effort <- effort %>% 
  filter(village == "E10" & session > 12 |
         village == "E4" & session > 12 |
         village == "C2" & session > 2 |
         village %ni% (c("E10",
                         "E4",
                         "C2")) &
           village != "C8")
         

###EFFORT CLEANING

####check that all your effort dates are indeed real work days

effort %>%
  filter(!date %in% 
           work_days$date) #if there are some here, bad news!

effort_check <-  effort %>%
  filter(!date %in% 
           work_days$date)

effort_check$problem <- NA

effort_check$problem[is.na(effort_check$problem)] <- "NOT WORK DAY"

###check your that you have no NAs for your houses
###if this happens you need to cut these days from the effort...
###....and thus the bushmeat data from that day

effort %>%
  filter(is.na(ME) |
           is.na(MPE) |
           is.na(MV) |
           is.na(M) )

effort_check <- bind_rows(effort_check, 
                          effort %>%
                            filter(is.na(ME) |
                                     is.na(MPE) |
                                     is.na(MV) |
                                     is.na(M) ))

effort_check$problem[is.na(effort_check$problem)] <- "maison NA"

effort <- effort %>%
  filter(!is.na(ME) |
           !is.na(MPE) |
           !is.na(MV) |
           !is.na(M) ) #throw away the effort with NA for maisons

###check that you have no no 0s for your ME or M

effort %>%
  filter(ME == 0 | M == 0)

effort_check <- bind_rows(effort_check,
                          effort %>%
                            filter(ME == 0 | M == 0))

effort_check$problem[is.na(effort_check$problem)] <- "maison 0"

effort <- effort %>%
  filter(!M == 0) #we throw out if total maisons = 0

####make sure that all maison categories add up to the same number
##function identical often not working

identical((effort$ME + effort$MPE + effort$MV), effort$M) #if FALSE problem

same_vec <- c() #make an empty vector

for (i in 1:nrow(effort)) {
  same_vec[i] <- identical((effort$ME + effort$MPE + effort$MV)[i], effort$M[i])
} #make a vector of where this is TRUE AND FALSE

table(same_vec) #see how many falses you have

which(FALSE == same_vec) #find where it is FALSE 

effort[c(which(FALSE == same_vec)),] #look at this in the DF

effort_check <- bind_rows(effort_check,
                          effort[c(which(FALSE == same_vec)),])
                     

effort_check$problem[is.na(effort_check$problem)] <- "maisons don't add"

###check that your total maisons are consistent

effort  %>%
  filter(!M == normal_M)

effort_check <- bind_rows(effort_check,
                          effort  %>%
                            filter(!M == normal_M))

effort_check$problem[is.na(effort_check$problem)] <- "funny total M"


####check if effort is particularly low

effort %>%
  filter(M_prop < 0.5)

effort_check <- bind_rows(effort_check,
                          effort %>%
                            filter(M_prop < 0.5))

effort_check$problem[is.na(effort_check$problem)] <- "low effort"

###make sure all our villages are correctly named
###we don't cut or NA any wrong village names because this must and can be corrected

length(unique(effort$village)) #if bigger than 20 you have a problem

possible_villages <- unique(village_meta$village)

table(effort$village %in% possible_villages) # If you have any false, there is a problem
effort[!effort$village %in% possible_villages,] #check out where your fake codes are 

effort_check <- bind_rows(effort_check, effort[!effort$village %in% possible_villages,])
effort_check$problem[is.na(effort_check$problem)] <- "village nom"

##make sure your sessions are correct
#again no NA or cutting because this can and must be corrected

possible_sess <- unique(work_days$session)

table(effort$session %in% possible_sess) # If you have any false, there is a problem
effort[!effort$session %in% possible_sess,] #check out where your fake codes are 

effort_check <- bind_rows(effort_check, effort[!effort$session %in% possible_sess,])
effort_check$problem[is.na(effort_check$problem)] <- "session"

###make sure that your transects are always M or S
#we don't cut or NA any wrong treatments becasue this can and must be corrected

possible_MS <- c("M", "S")

table(effort$'matin/soir' %in% possible_MS) # If you have any false, there is a problem
effort[!effort$'matin/soir' %in% possible_MS,] #check out where your fake codes are 

effort_check <- bind_rows(effort_check, effort[!effort$'matin/soir' %in% possible_MS,])
effort_check$problem[is.na(effort_check$problem)] <- "matin/soir"

###make sure that your temps are always correct
#if they are not give them an NA

possible_temps <- c("S", "GP", "PP", "N")

table(effort$temps %in% possible_temps) # If you have any false, there is a problem
effort[!effort$temps %in% possible_temps,] #check out where your fake codes are 

effort_check <- bind_rows(effort_check, effort[!effort$temps %in% possible_temps,])
effort_check$problem[is.na(effort_check$problem)] <- "temps"

effort$temps[!effort$temps %in% possible_temps] <- NA 

###check if parecologists are starting on time...
###...as in within 5 minutes before or after official start

m_late <- hms("07:24:00")
m_early <- hms("07:16:00")

s_late <- hms("15:54:00")
s_early <- hms("15:46:00")

effort %>%
  filter(`matin/soir` == "M" &
           `heure debut` > m_late |
           `matin/soir` == "M" &
           `heure debut` < m_early |
           `matin/soir` == "S" &
           `heure debut` > s_late |
           `matin/soir` == "S" &
           `heure debut` < s_early )
        
effort_check <- bind_rows(effort_check,
                          effort %>%
                            filter(`matin/soir` == "M" &
                                     `heure debut` > m_late |
                                     `matin/soir` == "M" &
                                     `heure debut` < m_early |
                                     `matin/soir` == "S" &
                                     `heure debut` > s_late |
                                     `matin/soir` == "S" &
                                     `heure debut` < s_early ))

effort_check$problem[is.na(effort_check$problem)] <- "early or late"

#####how long are people taking to walk transects??
###flag when more than 20 minutes different than normal

effort %>%
  filter(walk_diff > 19 |
         walk_diff < -19)

effort_check <- bind_rows(effort_check,
                          effort %>%
                            filter(walk_diff > 19 |
                                     walk_diff < -19))

effort_check$problem[is.na(effort_check$problem)] <- "brief/long walk time"

#sort effort check nicely

effort_check <- effort_check %>% 
  arrange(session,village,date)

###BUSHMEAT CLEANING
# Define possible etat's

possible_etat <- c("E", "EF", "ED",
                   "MI", "MIF", "MID",
                   "G", "GF", "GD",
                   "MR", "MRF", "MRD",
                   "P", "PF", "PD",
                   "T", "TF", "TD") #this SHOULD never be NA, you have to see the data!

table(bushmeat$etat %in% possible_etat) # If you have any false, there is a problem
bushmeat[!bushmeat$etat %in% possible_etat,] #check out where your fake codes are 

to_check <- bushmeat[!bushmeat$etat %in% possible_etat,] 
to_check$problem <- NA
to_check$problem[is.na(to_check$problem)] <- "etat"

bushmeat$etat[!bushmeat$etat %in% possible_etat] <- NA #you don't use the , cause now you are vector doing

#define possible species

possible_especes <- species_meta$species

table(bushmeat$especes %in% possible_especes) # If you have any false, there is a problem
bushmeat[!bushmeat$especes %in% possible_especes,] #check out where your fake codes are 

to_check <- bind_rows(to_check, bushmeat[!bushmeat$especes %in% possible_especes,])
to_check$problem[is.na(to_check$problem)] <- "species"

bushmeat <- bushmeat[bushmeat$especes %in% possible_especes,] #here, instead of giving NAs to wrong species, 
#we CUT them because it is meaningless information to us (, to keep it tibble!)

###date now
###first, we check if we messey up any dates with NAs

table(is.na(bushmeat$date)) 

bushmeat %>%
  filter(is.na(date))

to_check <- bind_rows(to_check,
                      bushmeat %>%
                        filter(is.na(date)))

to_check$problem[is.na(to_check$problem)] <- "NA date"

##define possible dates
###remember, we first used our metadata to make sure all dates in the...
#...effort were indeed real dates, so now we use the unique dates in effort
#but we need to use uniqueID, because each village needs corresponding effort in the same village

possible_uniqueIDs <- unique(effort$uniqueID)

table(bushmeat$uniqueID %in% possible_uniqueIDs) # If you have any false, there is a problem
bushmeat[!bushmeat$uniqueID %in% possible_uniqueIDs,] #check out where your fake codes are 

to_check <- bind_rows(to_check, bushmeat[!bushmeat$uniqueID %in% possible_uniqueIDs,])
to_check$problem[is.na(to_check$problem)] <- "date + village not in effort"

bushmeat <- bushmeat[bushmeat$uniqueID %in% possible_uniqueIDs,] #here, instead of giving NAs to wrong days, 
#we CUT them because we'll have no effort for those days

###make sure all our villages are correctly named
###we don't cut or NA any wrong village names because this must and can be corrected

length(unique(bushmeat$village)) #if bigger than 20 you have a problem

table(bushmeat$village %in% possible_villages) # If you have any false, there is a problem
bushmeat[!bushmeat$village %in% possible_villages,] #check out where your fake codes are 

to_check <- bind_rows(to_check, bushmeat[!bushmeat$village %in% possible_villages,])
to_check$problem[is.na(to_check$problem)] <- "village"

##make sure your sessions are correct
#again no NA or cutting because this can and must be corrected

table(bushmeat$session %in% possible_sess) # If you have any false, there is a problem
bushmeat[!bushmeat$session %in% possible_sess,] #check out where your fake codes are 

to_check <- bind_rows(to_check, bushmeat[!bushmeat$session %in% possible_sess,])
to_check$problem[is.na(to_check$problem)] <- "session"

###now we need to do a slew of things with nombre

###check if the number is ever zero
##if it IS, go back to the csv and change it to NA
#also we do this in R at the bottom to be safe

bushmeat %>%
  filter(nombre == 0)

to_check <- bind_rows(to_check,
                      bushmeat %>%
                        filter(nombre == 0))

to_check$problem[is.na(to_check$problem)] <- "nombre is 0"

bushmeat$nombre[bushmeat$nombre == 0] <- NA

###look at how many NAs you have for nombre
#this is NOT the end of the world, but you add them to to_check

bushmeat %>%
  filter(is.na(nombre))

to_check <- bind_rows(to_check,
                      bushmeat %>%
                        filter(is.na(nombre)))

to_check$problem[is.na(to_check$problem)] <- "nombre is NA"

###ensure that when etat is E EF or ED nombre is 1
##try and go back and fix the csv's
##if this cannot be done, both etat and nombre must become NA

bushmeat %>%
  filter(etat == "E" & nombre > 1 
         | etat == "EF" & nombre > 1
         | etat == "ED" & nombre > 1
         | etat == "E" & is.na(nombre)
         | etat == "EF" & is.na(nombre)
         | etat == "ED" & is.na(nombre)) 

to_check <- bind_rows(to_check,
                      bushmeat %>%
                        filter(etat == "E" & nombre > 1 
                               | etat == "EF" & nombre > 1
                               | etat == "ED" & nombre > 1
                               | etat == "ED" & is.na(nombre)
                               | etat == "ED" & is.na(nombre)
                               | etat == "ED" & is.na(nombre)))


to_check$problem[is.na(to_check$problem)] <- "nombre E/EF/ED not 1"

#if the number isn't 1, we can't be sure the etat was correct so it gets replaced with NA

bushmeat$etat[bushmeat$etat == "E" & bushmeat$nombre > 1 |
                bushmeat$etat == "E" & is.na(bushmeat$nombre) |
                bushmeat$etat == "EF" & bushmeat$nombre > 1 | 
                bushmeat$etat == "EF" & is.na(bushmeat$nombre) | 
                bushmeat$etat == "ED" & bushmeat$nombre > 1 | 
                bushmeat$etat == "ED" & is.na(bushmeat$nombre)] <- NA

#and if we don't know the etat, surely we don't have a good idea on the nombre

bushmeat$nombre[is.na(bushmeat$etat)] <- NA

###check if you have any crazy high numbers
##if this is high do some filtering to see how many are high
#make sure that these high numbers are with morceaux

quantile(bushmeat$nombre, na.rm = TRUE)

sort(bushmeat$nombre, decreasing = TRUE)

bushmeat %>% 
  filter(nombre > 20)  #or any number you deem worthy of a closer look

bushmeat %>% 
  filter(nombre > 20, etat %ni% c("MR", "MRF", "MRD"))  #or any number you deem worthy of a closer look

###make sure your endroits are correct
#if the endroit is incorrect it gets an NA

possible_endroits <- c("R", "M") #this should NEVER be NA!

table(bushmeat$endroit %in% possible_endroits) # If you have any false, there is a problem
bushmeat[!bushmeat$endroit %in% possible_endroits,] #check out where your fake codes are 

to_check <- bind_rows(to_check, bushmeat[!bushmeat$endroit %in% possible_endroits,])
to_check$problem[is.na(to_check$problem)] <- "endroits"

bushmeat$endroit[!bushmeat$endroit %in% possible_endroits] <- NA

###sex can be either F M or NA
#if incorrect it gets an NA

possible_sex <- c("F", "M", NA)

table(bushmeat$sex %in% possible_sex) # If you have any false, there is a problem
bushmeat[!bushmeat$sex %in% possible_sex,] #check out where your fake codes are 

to_check <- bind_rows(to_check, bushmeat[!bushmeat$sex %in% possible_sex,])
to_check$problem[is.na(to_check$problem)] <- "sex"

bushmeat$sex[!bushmeat$sex %in% possible_sex] <- NA

###now we do all the obtenir stuff, replacing non-codes with NAs

possible_ob_via <- c("C", "A", "D",
                     "CH", "AH", "DH", NA)

table(bushmeat$'obtenu via' %in% possible_ob_via) # If you have any false, there is a problem
bushmeat[!bushmeat$'obtenu via' %in% possible_ob_via,] #check out where your fake codes are 

to_check <- bind_rows(to_check, bushmeat[!bushmeat$'obtenu via' %in% possible_ob_via,])
to_check$problem[is.na(to_check$problem)] <- "obtenu via"

bushmeat$'obtenu via'[!bushmeat$'obtenu via' %in% possible_ob_via] <- NA

###for obtenu comment it's simple derivation of via

possible_ob_com <- c("C", "A", "D", NA)

table(bushmeat$'obtenu comment' %in% possible_ob_com) # If you have any false, there is a problem
bushmeat[!bushmeat$'obtenu comment' %in% possible_ob_com,] #check out where your fake codes are 

to_check <- bind_rows(to_check, bushmeat[!bushmeat$'obtenu comment' %in% possible_ob_com,])
to_check$problem[is.na(to_check$problem)] <- "obtenu comment"

bushmeat$'obtenu comment'[!bushmeat$'obtenu comment' %in% possible_ob_com] <- NA

###for obentu ou its either local forest/village V or hors H

possible_ob_ou <- c("V", "H", NA)

table(bushmeat$'obtenu ou' %in% possible_ob_ou) # If you have any false, there is a problem
bushmeat[!bushmeat$'obtenu ou' %in% possible_ob_ou,] #check out where your fake codes are 

to_check <- bind_rows(to_check, bushmeat[!bushmeat$'obtenu ou' %in% possible_ob_ou,])
to_check$problem[is.na(to_check$problem)] <- "obtenu ou"

bushmeat$'obtenu ou'[!bushmeat$'obtenu ou' %in% possible_ob_ou] <- NA

###for obentu ou comment et via there cannot be only one na

bushmeat %>%
  filter(is.na(`obtenu via`) &
           !is.na(`obtenu comment`) |
           is.na(`obtenu via`) &
           !is.na(`obtenu ou`) | 
           is.na(`obtenu comment`) &
           !is.na(`obtenu via`) |
           is.na(`obtenu comment`) &
           !is.na(`obtenu ou`) |
           is.na(`obtenu ou`) &
           !is.na(`obtenu via`) |
           is.na(`obtenu ou`) &
           !is.na(`obtenu comment`))

to_check <- bind_rows(to_check,
                      bushmeat %>%
                        filter(is.na(`obtenu via`) &
                                 !is.na(`obtenu comment`) |
                                 is.na(`obtenu via`) &
                                 !is.na(`obtenu ou`) | 
                                 is.na(`obtenu comment`) &
                                 !is.na(`obtenu via`) |
                                 is.na(`obtenu comment`) &
                                 !is.na(`obtenu ou`) |
                                 is.na(`obtenu ou`) &
                                 !is.na(`obtenu via`) |
                                 is.na(`obtenu ou`) &
                                 !is.na(`obtenu comment`)))

to_check$problem[is.na(to_check$problem)] <- "obtenus not consistent"

###all but that aren't real codes get NAs
#G is for garder

possible_but <- c("M", "V", "S", "C", "G", NA)

table(bushmeat$but %in% possible_but) # If you have any false, there is a problem
bushmeat[!bushmeat$but %in% possible_but,] #check out where your fake codes are 

to_check <- bind_rows(to_check, bushmeat[!bushmeat$but %in% possible_but,])
to_check$problem[is.na(to_check$problem)] <- "but"

bushmeat$but[!bushmeat$but %in% possible_but] <- NA

###do you have any crazy seeming prices???
##filter and investigate if so
##especially check that small prices are with morceaux
###and very large prices are with entire

quantile(bushmeat$prix, na.rm = TRUE)

sort(bushmeat$prix, decreasing = TRUE)
sort(bushmeat$prix, decreasing = FALSE)

View(bushmeat %>% 
  filter(prix > 30000 | prix < 1000))  

###tue via gets NA if not a real code

possible_tue_via <- c("F", "P", "M", "C", NA)

table(bushmeat$'tue via' %in% possible_tue_via) # If you have any false, there is a problem
bushmeat[!bushmeat$'tue via' %in% possible_tue_via,] #check out where your fake codes are 

to_check <- bind_rows(to_check, bushmeat[!bushmeat$'tue via' %in% possible_tue_via,])
to_check$problem[is.na(to_check$problem)] <- "tue via"

bushmeat$'tue via'[!bushmeat$'tue via' %in% possible_tue_via] <- NA

###tue quand gets NA if not a real code

possible_tue_quand <- c("N", "J", NA)

table(bushmeat$'tue quand' %in% possible_tue_quand) # If you have any false, there is a problem
bushmeat[!bushmeat$'tue quand' %in% possible_tue_quand,] #check out where your fake codes are 

to_check <- bind_rows(to_check, bushmeat[!bushmeat$'tue quand' %in% possible_tue_quand,])
to_check$problem[is.na(to_check$problem)] <- "tue quand"

bushmeat$'tue quand'[!bushmeat$'tue quand' %in% possible_tue_quand] <- NA

###tue ou gets NA if not a real code
##we only used secondary forest FS in the first session, since cut it...
##...so I am putting all FS into NA

possible_tue_ou <- c("FO", "FF", "M", "R", "P", NA)

table(bushmeat$'tue ou' %in% possible_tue_ou) # If you have any false, there is a problem
bushmeat[!bushmeat$'tue ou' %in% possible_tue_ou,] #check out where your fake codes are 

to_check <- bind_rows(to_check, bushmeat[!bushmeat$'tue ou' %in% possible_tue_ou,])
to_check$problem[is.na(to_check$problem)] <- "tue ou"

bushmeat$'tue ou'[!bushmeat$'tue ou' %in% possible_tue_ou] <- NA

###cote is very simple

possible_cote <- c("N", "S", NA)

table(bushmeat$cote %in% possible_cote) # If you have any false, there is a problem
bushmeat[!bushmeat$cote %in% possible_cote,] #check out where your fake codes are 

to_check <- bind_rows(to_check, bushmeat[!bushmeat$cote %in% possible_cote,])
to_check$problem[is.na(to_check$problem)] <- "cote"

bushmeat$cote[!bushmeat$cote %in% possible_cote] <- NA

###are any of your KM outlandish???
###in the thousands are clearly meters...
###be careful with things around 20, 30 km...was there a road used?

quantile(bushmeat$km, na.rm = TRUE)

sort(bushmeat$km, decreasing = TRUE)
sort(bushmeat$km, decreasing = FALSE)

max(bushmeat$km[!is.na(bushmeat$km)])
min(bushmeat$km[!is.na(bushmeat$km)])

View(bushmeat %>% 
  select(village,
         session, date, heure, especes, km, everything()) %>%
  filter(km > 10 | km < 1))

to_check <- bind_rows(to_check, bushmeat %>% 
                        filter(km > 10 | km < 1))

to_check$problem[is.na(to_check$problem)] <- "extreme KM"

#sort to check nicely

to_check <- to_check %>% 
  arrange(session,village,date)

###BIG PROBLEM THAT R CAN NOT DEAL WITH
###IF M = 0 OR NA, WE CAN'T TELL R TO REMOVE ONLY THE BUSHMEAT FROM MATIN OR SOIR
####THUS WE CANNOT CUT BY DAYS BECAUSE WE WOULD LOSE DATA THAT WE SHOULD KEEP

urgent_check <- effort_check %>%
 filter(problem == "maison NA" 
        | problem == "maison 0") 

urgent_check <- urgent_check %>%
  select(uniqueID, village, session, date, `matin/soir`,
         ME,MPE,MV,M,problem)

urgent_check$'ACTION NEEDED' <- "delete all bushmeat during this time!!!"

###save

bushmeat_name <- paste0("bushmeat_cleaned_", today())
effort_name <- paste0("effort_cleaned_", today())
bushmeat_check_name <- paste0("bushmeat_to_check_", today())
effort_check_name <- paste0("effort_to_check_", today())
urgent_check_name <- paste0("URGENT_CHECK_", today())

saveRDS(bushmeat, file = paste0("./outputs/cleaned/", bushmeat_name,".rds"))
saveRDS(effort, file = paste0("./outputs/cleaned/", effort_name,".rds"))

write_csv(bushmeat, path = paste0("./outputs/cleaned/", bushmeat_name,".csv"))
write_csv(effort, path = paste0("./outputs/cleaned/", effort_name,".csv"))

write_csv(to_check, path = paste0("./outputs/cleaned/", bushmeat_check_name,".csv"))
write_csv(effort_check, path = paste0("./outputs/cleaned/", effort_check_name,".csv"))
write_csv(urgent_check, path = paste0("./outputs/cleaned/", urgent_check_name,".csv"))

