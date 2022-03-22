##script running algorithim for estimating offtake

##### clean up

rm(list = ls())

#create not in
#`` what you need for filtering special column names

'%ni%' <- Negate('%in%')

###load packages
library("plyr") #ALWAYS HAVE TO LOAD BEFORE DPLYR, OR ELSE IT MASKS DPLYR
library("tidyverse")
library("lubridate")
library("janitor")
library("brms")
library("gridExtra")
library("grid") #for adding title to grid.arrange
library("rstan") #for plotting posteriors
library("coda") #for function HPDinterval #masks traceplot for rstan
library("scales") #for ggplot scale fixing

##get meta

species_meta <- readRDS("./inputs/meta/species_meta_traits.rds")

village_meta <- readRDS("./inputs/meta/project_villages.rds")

work_days <- read_csv("./inputs/meta/work_days_months.csv",
                      locale = readr::locale(encoding = "latin1")) %>%
  mutate_if(is.character,funs(factor(.))) %>%
  filter(session < 15)

work_days$date <- dmy(work_days$date)

##bushmeat and effort data

bushmeat_EN <- readRDS("./inputs/bushmeat_transects.rds")
bushmeat <- readRDS("./inputs/bushmeat_transects_FR.rds")

p2_offtake <- readRDS("./inputs/offtake_transects.rds")

###get effort for p2 transects

effort_EN <- readRDS("./inputs/effort_transects.rds")
effort <- readRDS("./inputs/effort_transects_FR.rds")

##1. clean things up before algorithim

##create a copy of the bushmeat data to manipulate
##why do this? because we need to change nombre NA to 1

bushmeat_off <- bushmeat

####now let's check if we have any NA's in terms of nombre
##if YES we need to deal with them because we count things below
#we remain conservative when assigning numbers to unknowns...
#...our lowest possible number is 1, so this is what will assign the unknown NAs

table(is.na(bushmeat_off$nombre)) 

bushmeat_off$nombre[is.na(bushmeat_off$nombre)] <- 1

#there should now be no TRUE
table(is.na(bushmeat_off$nombre)) 

###see how much bushmeat you have each session and also with only entire animals

bushmeatE <- bushmeat_off %>%
  filter(etat=="E" | etat=="ED" | etat=="EF")

#create empty list of appropriate length for offtake per session
bushmeat_session_list <- vector("list", length(unique(bushmeat_off$session)))

for (i in seq_along(unique(bushmeat_off$session))) {
  
  #populate the list
  bushmeat_session_list[[i]] <- bushmeat_off %>%
    filter(session == unique(bushmeat_off$session)[i])
  
  #name it 
  names(bushmeat_session_list)[i] <- paste0("bushmeat", 
                                            unique(bushmeat_off$session)[i])
  
  #extract the objects with their names
  assign(names(bushmeat_session_list)[i],
         bushmeat_session_list[[i]])
  
}

###create a string of unique events of all bushmeat

unique_events <- unique(paste0(bushmeat_off$village, "_", 
                               bushmeat_off$date, "_",
                               bushmeat_off$especes))

unique_eventsE <- unique(paste0(bushmeatE$village, "_", 
                                bushmeatE$date, "_",
                                bushmeatE$especes))

#create empty list of appropriate length for unique events
unique_events_session_list <- vector("list", length(bushmeat_session_list)) 

for (i in seq_along(unique(bushmeat_off$session))) {
  
  #populate it
  unique_events_session_list[[i]] <- unique(paste0(bushmeat_session_list[[i]]$village, "_", 
                                                   bushmeat_session_list[[i]]$date, "_",
                                                   bushmeat_session_list[[i]]$especes))
  
  #name it 
  names(unique_events_session_list)[i] <- paste0("unique_events", 
                                                 unique(bushmeat_off$session)[i])
  
  #extract the objects with their names
  assign(names(unique_events_session_list)[i],
         unique_events_session_list[[i]])
  
}

#now make a dataframe in which later you can check your algorithim's logic with

#nrows is number of sessions + 2 (total & E)
logic_check <- data.frame(matrix(ncol = 3, nrow = length(unique(bushmeat_off$session)) + 2))
colnames(logic_check) <- c("observations", "unique_events", "animals")
rownames(logic_check) <- c("all", "entire", paste0("session", unique(bushmeat_off$session)))

#use a mapping function to fill in the observations
logic_check$observations <- c(nrow(bushmeat_off), nrow(bushmeatE),
                              seq_along(unique(bushmeat_off$session)) %>%
                                map(function (x) nrow(bushmeat_session_list[[x]])) %>%
                                unlist())

#same same for unique events
logic_check$unique_events <- c(length(unique_events), length(unique_eventsE),
                                 seq_along(unique(bushmeat_off$session)) %>%
                                   map(function (x) length(unique_events_session_list[[x]])) %>%
                                   unlist())


###lastly let's check if we currently have any NA's in terms of etat
#they can arise with E/EF/ED with nombre > 1...
#this is no problem! function deals with them..

table(is.na(bushmeat_off$etat)) 

##2. Make offtake-estimation algorithim function
#Chris Beirne & I wrote this together, he really drove the beginning
#b stands for your bushmeat choice, and u for your unique events
#note I have a script with fake data trying really hard to break this...
#...and it holds up, always delivering the estimate we expect

generate_offtake <- function(b,u) {
  
  ###create an empty list to fill up  
  tmp_take <- list()
  
  
  for (i in seq_along(u))
  {
    
    tmp <- b[b$village == strsplit(u[i], "_")[[1]][1] &
               b$date == strsplit(u[i], "_")[[1]][2] &
               b$especes == strsplit(u[i], "_")[[1]][3],]
    
    
    ###active this code if you have NAs!
    
    na.tmp <- tmp[is.na(tmp$etat)==T,]
    tmp <- tmp[is.na(tmp$etat)==F,]
    
    
    ##the count starts at 0
    
    count <- 0
    
    ##make your estimated animals from moitie et gigots called MIG
    #first just count your total moities and gigots
    
    tmpMI <- sum((tmp[tmp$etat=="MI" | tmp$etat=="MID" |  tmp$etat=="MIF",])$nombre)
    tmpG <- sum((tmp[tmp$etat=="G" | tmp$etat=="GD" |  tmp$etat=="GF",])$nombre)
    
    #step A: make all your gigots possible as moitie
    #(2 moities make a gigot, and if there is an extra, you toss it out)
    sa <- floor(tmpG/2)
    
    #step B: keep your extra gigot (if you have one) on the side
    #(the difference 'picks' up the gigot you tossed out above)
    sb <- ceiling(tmpG/2) - floor(tmpG/2)
    
    #step C: make as many betes as possible...
    #by using your total moities (including the gigots you just converted)
    #which excludes your extra gigot and moitie (if you have them)
    #(two moities make a bete, and if there is an extra you toss it out)
    sc <- floor((tmpMI + sa)/2)
    
    #step D: keep your extra moitie (if you have one) on the side
    #(the difference 'picks' up the moitie you tossed out above)
    sd <- ceiling((tmpMI + sa)/2) - floor((tmpMI + sa)/2)
    
    #step E: determine whether you have one additional bete..
    ##due to either an extra gigot, extra moitie, or both
    ##(you may not have one)
    ##you divide by two and round up so that 0 + 0, 0 + 1, and 1 + 1 all equal 1
    se <- ceiling(sb/2 + sd/2)
    
    #create MIG!! estimated betes from moties and gigots together
    ##add the many betes as possible to your potential additional bete
    MIG <- sc + se 
    
    ###all entire animals (regardless of state) add 1 to the count
    ##if you filter bushmeat to ONLY include E, ED, and EF..
    ##..and then comment the logic dealing with other etats...
    ##...the length of offtake must equal the length of the filtered bushmeat
    
    if(length(tmp$etat[tmp$etat=="E" | tmp$etat=="ED" |  tmp$etat=="EF"])>0)
    {
      #the below lines was before E was sometimes > 1 in phase 1
      #count <- count + nrow(tmp[tmp$etat=="E" | tmp$etat=="ED" |  tmp$etat=="EF", ])
      count <- count + sum(tmp$nombre[tmp$etat=="E" | tmp$etat=="ED" |  tmp$etat=="EF"])
    }
    
    ###if there is one or more head, and this number is greater than or equal to...
    ###the number of skins, &
    ###the number of animals estimated via MIG
    ##then we add the number of heads to the count
    
    if(sum(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"]) > 0 &
       sum(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"]) >= 
       sum(tmp$nombre[tmp$etat=="P" | tmp$etat=="PD" |  tmp$etat=="PF"]) &
       sum(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"]) >= MIG
    ) 
    {
      count <- count + sum(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"])
    }
    
    ###if then is one or more skins & more than heads, &
    #more or equal than the number of animals estimated via MIG
    ###we add the skins
    
    if(sum(tmp$nombre[tmp$etat=="P" | tmp$etat=="PD" |  tmp$etat=="PF"]) > 0 &
       sum(tmp$nombre[tmp$etat=="P" | tmp$etat=="PD" |  tmp$etat=="PF"]) > 
       sum(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"]) &
       sum(tmp$nombre[tmp$etat=="P" | tmp$etat=="PD" |  tmp$etat=="PF"]) >= MIG
    ) 
    {
      count <- count + sum(tmp$nombre[tmp$etat=="P" | tmp$etat=="PD" |  tmp$etat=="PF"])
    }
    
    ###if then is one or more MIG & more than heads, &
    #more than skins, &
    ##we add MIG
    
    if(MIG > 0 &
       MIG > sum(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"]) &
       MIG > sum(tmp$nombre[tmp$etat=="P" | tmp$etat=="PD" |  tmp$etat=="PF"])
    )
    {
      count <- count + MIG
    }
    
    
    ##if there are no heads or skins or halves or gigots...
    ###...but there are some morceaux we simply add 1
    
    if(length(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"]) == 0 &
       length(tmp$nombre[tmp$etat=="P" | tmp$etat=="PD" |  tmp$etat=="PF"]) == 0 & 
       length(tmp$nombre[tmp$etat=="MI" | tmp$etat=="MID" |  tmp$etat=="MIF"]) == 0 &
       length(tmp$nombre[tmp$etat=="G" | tmp$etat=="GD" |  tmp$etat=="GF"]) == 0 &
       length(tmp$nombre[tmp$etat %in% c( "MR", "MRF", "MRD")]) > 0  
    ) 
    {
      count <- count + 1
    }
    
    ##if we have no heads, halves, or bits, but DO have etat = NA, we add 1 to our count
    #reactive this code if you have NAs
    ##I don't understand why we put the tmp <- na.tmp???
    
    if(length(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"]) == 0 &
       length(tmp$nombre[tmp$etat=="P" | tmp$etat=="PD" |  tmp$etat=="PF"]) == 0 &
       MIG == 0 &
       length(tmp$nombre[tmp$etat %in% c("MR", "MRF", "MRD")]) == 0 &
       nrow(na.tmp) > 0  )
    {
      count <- count + 1
      tmp <- na.tmp
    }
    
    if(length(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"]) == 0 &
       length(tmp$nombre[tmp$etat=="MI" | tmp$etat=="MID" |  tmp$etat=="MIF"]) == 0 &
       length(tmp$nombre[tmp$etat %in% c("G", "GF", "GD",
                                         "MR", "MRF", "MRD",
                                         "P", "PF", "PD")]) == 0 &
       nrow(na.tmp) > 0  )
    {
      count <- count + 1
      tmp <- na.tmp
    }
    
    ####fill up your offtake!
    
    tmp_take[[i]] <- data.frame(village=rep(tmp$village[1], times=count),
                                date=tmp$date[1],
                                uniqueID=tmp$uniqueID[1],
                                session=tmp$session[1],
                                especes=tmp$especes[1])
    
    
    
  }
  
  return(tmp_take)
  
}

##3.Estimate offtake and check logic

##make your offtakes

offtake <- do.call("rbind", generate_offtake(bushmeat_off, unique_events))

offtakeE <- do.call("rbind", generate_offtake(bushmeatE, unique_eventsE))

####check how well your offtake worked with the logic check table

logic_check$animals <- logic_check$animals <- c(nrow(offtake), nrow(offtakeE),
                                                unique(bushmeat_off$session) %>%
                                                  map(function (x) nrow(offtake %>% filter(session == x))) %>%
                                                  unlist())

logic_check

#session sums must equal all
identical(
  colSums(as.vector(logic_check %>%
                      filter(rownames(logic_check) == "all"))),
  
  colSums(logic_check %>% 
            filter(rownames(logic_check) %ni% c("all", "entire")))
)


#obs and animals must be equal in entire
identical(
  (logic_check %>%
     filter(rownames(logic_check) == "entire"))$observations,
  (logic_check %>%
     filter(rownames(logic_check) == "entire"))$animals
)

#all good, change column names to english

offtake <- as_tibble(offtake) %>% 
  dplyr::rename(villageday = uniqueID,
         species = especes) %>% 
  mutate(date = dmy(date))

write_csv(offtake, "./outputs/offtake_transects.csv")


