##summarize data for AJE
##first made 19.02.20 by GF

##### clean up

rm(list = ls())

#create not in
'%ni%' <- Negate('%in%')

###load packages
library("plyr")
library("raster")  #for clamp function, note this masks dplyr's select
library("tidyverse")
library("lubridate")
library("janitor")
library("stringi")
library("corrplot")

##get meta

species_meta <- readRDS("./inputs/meta/species_meta_traits.rds")

village_meta <- readRDS("./inputs/meta/project_villages.rds")

work_days <- read_csv("./inputs/meta/work_days_months.csv",
                      locale = readr::locale(encoding = "latin1")) %>%
  mutate_if(is.character,funs(factor(.))) %>%
  filter(session < 15)

work_days$date <- dmy(work_days$date)

phunter_meta <- readRDS("./inputs/meta/phunter_meta.rds")

#get bushmeat transect data data
#QC as follows (see data_issues.csv in metadata)
#village C8 all bushmeat cut
#C2 <= bushmeat session 2 cut
#E4 <= bushmeat session 12 cut (carto<= session 8)
#E10 <= bushmeat session 12 cut (carto<= session 12)
#E2 cut all cartography
#session 15 shortened by covid, cut from main analyses

p2_bushmeat <- readRDS("./inputs/bushmeat_transects.rds")
p2_bushmeat_FR <- readRDS("./inputs/bushmeat_transects_FR.rds")

p2_offtake <- readRDS("./inputs/offtake_transects.rds")

###get effort for p2 transects

p2_eff <- readRDS("./inputs/effort_transects.rds")
p2_eff_FR <- readRDS("./inputs/effort_transects_FR.rds")

##cartography

carto_huntmeat <- readRDS("./inputs/carto_huntmeat.rds") 

kde95 <- readRDS("./inputs/kde95.rds")

gun_in <- readRDS("./inputs/carto_gunhunts.rds") 
trap_in <- readRDS("./inputs/carto_traphunts.rds")

all_in <- bind_rows(
gun_in %>% 
  filter(guntrap != "both") ,
trap_in
)

table(all_in$guntrap)
table(all_in$spatial_data)

huntcount <- left_join(
all_in %>% 
  group_by(village) %>% 
  filter(spatial_data == "yes") %>% 
  dplyr::summarize(input_gps_hunts = n()),
kde95) 

huntcount <- huntcount %>% 
  dplyr::select(village,
         gps_hunts, 
         input_gps_hunts,
         kde95) 

#participating hunter ages (asked for by reviewer)

phunter_meta %>% 
  filter(carto_data == "yes") %>% 
  summarize(mean_age = mean(age,na.rm = TRUE),
            sd_age = sd(age,na.rm = TRUE),
            min_age = min(age,na.rm = TRUE),
            max_age = max(age,na.rm = TRUE))


####1. BASIC SUMMARIES OF TRANSECTS AND CARTOGRAPHY

#transect dates

min(p2_eff$date)
max(p2_eff$date)
n_transects <- nrow(p2_eff)
p2_eff_days <- length(unique(p2_eff$date))

#bushmeat observations
nrow(p2_bushmeat)

#number of species
length(unique(p2_bushmeat$species))

#estimated offtake represented
nrow(p2_offtake)

#estimated offtake of fully protected species
nrow(
  left_join(p2_offtake, species_meta) %>%
    filter(protection == "Intégralement Protégée")
)  +
  nrow(
    left_join(carto_huntmeat, species_meta) %>%
      filter(protection == "Intégralement Protégée")
  )

###hunter self-follows

#dates
min(all_in$start_date)
max(all_in$end_date)

#n villages and hunters
length(unique(all_in$village))
length(unique(all_in$hunter))

#n hunts
nrow(all_in)
table(all_in$guntrap)

#n animals killed gunhunts
#we don't need our fancy algorithm, because here
#the hunters showed what they got directly upon return
#only 28 times was an animal cut up before brought back
#and each row corresponds to 1 animals (for example MI == 2, G == 4)..
#...EXCEPT the two heads, which we should remove

carto_huntmeat %>%
  filter(state %ni% c("E", "EF", "ED")) %>%
  print(n = Inf)
nrow(
  carto_huntmeat %>%
    filter(state != "T")
)

carto_offtake <- carto_huntmeat %>%
  filter(state != "T") %>%
  dplyr::select(colnames(p2_offtake))

###2. ESTIMATE OFFTAKE INTEGRATION TRANSECTS AND CARTO
##compare diff between p2offtake and carto_offtake

cartoff <- carto_offtake
transoff <- p2_offtake

###first filter transoff to include only carto villages and sessions

unique(cartoff$session)
unique(cartoff$village)

transoff <- transoff %>%
  filter(session %in% unique(cartoff$session) &
           village %in% unique(cartoff$village))

table(transoff$session %ni% cartoff$session)
table(transoff$village %ni% cartoff$village)

#now we are only interested in the transect days..
#... on the days and villages in which carto huntmeat was recorded
#..and only interested in the carto days...
#...on the days which transects were walked in THAT village
transoff <- transoff %>%
  filter(villageday %in% cartoff$villageday)

cartoff <- cartoff %>%
  filter(villageday %in% p2_eff$villageday)

table(cartoff$villageday %ni% p2_eff$villageday)
table(transoff$villageday %ni% cartoff$villageday)

#how many animals counted by species by village day in the carto?
##how many in the transect?

cartcount <- cartoff %>%
  group_by(villageday,species) %>%
  dplyr::summarise(total_cart = n())

transcount <- transoff %>%
  group_by(villageday,species) %>%
  dplyr::summarise(total_trans = n())

#some thoughts as of 17.06.20
##we want to know for the species counted in the carto..
###...how many were counted in the transects?
##of course we can't know to individual...
## (i.e., a CBL in carto could be 'missed', but if another one counted..
##....from a non-carto hunter it wouldn't appear 'missed')
#thus the percentage 'captured' by transects will be OVERESTIMATED
#...this is just the reality (better than underestimated?)
#for the transects retain only the species counted also in carto
#first need to make a key

cartcount$key <- paste0(cartcount$villageday, "_", cartcount$species)
transcount$key <- paste0(transcount$villageday, "_", transcount$species)

table(transcount$key %in% cartcount$key)

transcount <- transcount %>%
  filter(key %in% unique(cartcount$key))
table(transcount$key %in% cartcount$key)

##so a very rough percentage of capture is just the percentage..
#..of carto counts captured by transects

round((sum(transcount$total_trans)/
         sum(cartcount$total_cart)),2)

#let's now do it by species
##calculate % of each carto species by village by day counted in trans
###note that % is sometimes > 100% (1)..
#..because trans can count other animals not in carto

capture_rate <- left_join(cartcount, transcount) %>%
  dplyr::select(key, everything()) %>%
  replace(is.na(.), 0) %>%
  mutate(rate = total_trans/total_cart)

#% of times transects overcounted

(nrow(capture_rate %>%
        filter(rate > 1)) /
    nrow(capture_rate))*100

###this shows the "leftover" rates
#we thus expect the diff to be this if we limit the %
#check below

sum((capture_rate %>%
       filter(rate > 1))$rate) -
  nrow(capture_rate %>%
         filter(rate > 1))

crtmp <- capture_rate

crtmp$rate <- raster::clamp(crtmp$rate, lower = 0, upper = 1)

sum(capture_rate$rate) -
  sum(crtmp$rate)

#all good, so let's limit the %

capture_rate$rate <- raster::clamp(capture_rate$rate, lower = 0, upper = 1)

#notice the rough # counted percentage is far different than
##the mean capture rate

(sum(capture_rate$total_trans))/(sum(capture_rate$total_cart))
mean(capture_rate$rate)

##could this be explained by the times transects captured more?
#YES the TWO NUMBERS NOW SIMILAR
#~1% diff from each other, and ~4% lower than mean capture rate above

crtmp2 <- capture_rate %>%
  filter (total_trans <= total_cart)

(sum(crtmp2$total_trans))/(sum(crtmp2$total_cart))
mean(crtmp2$rate)

#so I think our most informative metric of capture is the mean %
##let's look at this by species and village
#flesh out more details first

capture_rate$village <- sub('\\_.*', '', capture_rate$villageday)
capture_rate$date <-sub('.*\\_', '', capture_rate$villageday)
capture_rate$date <- ymd(capture_rate$date)

capture_rate <- left_join(capture_rate, work_days[,c("date", "session")],
                          by = "date") %>%
  dplyr::select(key,villageday,village,
                date,session,species,
                total_cart,total_trans,rate)

capture_rate$key <- as.factor(capture_rate$key)
capture_rate$villageday <- as.factor(capture_rate$villageday)
capture_rate$village <- as.factor(capture_rate$village)

#summary function let's get means and SD

summarise_rate <- function (rate_df, group_var)
{
  group_var <- enquo(group_var)   # Create quosure
  tmp_rate <- rate_df  %>%
    group_by(!!group_var) %>% # Use !! to unquote the quosur
    dplyr::summarize(n = n(),
              median = median(rate),
              mean = mean(rate),
              sd = sd(rate))
  tmp_rate$se <- tmp_rate$sd/sqrt(tmp_rate$n)
  tmp_rate$ci95 <- tmp_rate$se*1.96
  tmp_rate$lowCI <- tmp_rate$mean - tmp_rate$ci95
  tmp_rate$highCI <- tmp_rate$mean + tmp_rate$ci95
  return(tmp_rate)
}

###general summaries

mean_capture <- summarise_rate(capture_rate)

village_capture <- summarise_rate(capture_rate,village) %>%
  arrange(desc(mean))

species_capture <- summarise_rate(capture_rate,species) %>%
  arrange(desc(mean))

session_capture <- summarise_rate(capture_rate,session)

mean_capture
village_capture
species_capture
session_capture

#round off the global capture rate (gcr)

mean_gcr <- round(mean_capture$mean,3)
ci95_gcr <- round(mean_capture$ci95,3)
low_gcr <- round(mean_capture$lowCI,3)
high_gcr <- round(mean_capture$highCI,3)

#effort by village

eff_village_tmp <- p2_eff %>%
  group_by(village) %>%
  dplyr::summarise(n_days = length(unique(date)))

eff_village_tmp %>%
  arrange(desc(n_days))

mean(eff_village_tmp$n_days)

###estimate yearly offtake (eyo)

eyo_species_village <-  p2_offtake %>%
  group_by(village,species) %>%
  dplyr::summarise(offtake_counted = n()) %>%
  arrange(desc(offtake_counted))

offtake_counted_species_village_session <- p2_offtake %>%
  group_by(village,species,session) %>%
  dplyr::summarise(offtake_counted = n()) %>%
  arrange(desc(offtake_counted))

eyo_species_village <- left_join(eyo_species_village,
                                 eff_village_tmp) %>%
  mutate(yearly_count = round(((offtake_counted/n_days)*365),0),
         eyo = round(yearly_count/mean_gcr,0),
         low_eyo = round(yearly_count/high_gcr,0),
         high_eyo = round(yearly_count/low_gcr,0)) %>%
  arrange(desc(eyo))

sum(eyo_species_village$eyo)
sum(eyo_species_village$low_eyo)
sum(eyo_species_village$high_eyo)

###eyo by species and village and total

summarize_eyo <- function(x) {
  x %>%
    dplyr::summarize(offtake_counted = sum(offtake_counted),
              n_days = mean(n_days),
              yearly_count = sum(yearly_count),
              eyo = sum(eyo),
              low_eyo = sum(low_eyo),
              high_eyo = sum(high_eyo)) %>%
    arrange(desc(eyo))
}

#by species

eyo_species <- summarize_eyo(eyo_species_village %>%
                               group_by(species)) %>%
  dplyr::select(-n_days)
eyo_species %>% print(n = Inf)

#by village

eyo_village <- summarize_eyo(eyo_species_village %>%
                               group_by(village))
eyo_village

#total

eyo_all <- summarize_eyo(eyo_species_village %>%
                           ungroup()) %>%
  mutate(species = "ALL") %>%
  dplyr::select(species,everything(), -n_days)

eyo_all

##how would village estimates change if we used village-specific capture rates?
##mean estimate differs be a couple hundred animals per village

eyo_vcr <- left_join(
  eyo_village,
  village_capture %>%
    dplyr::select(village,
           vcr = mean)
) %>%
  drop_na() %>%
  mutate(veyo = round(yearly_count/vcr, 0),
         diff = veyo - eyo)

eyo_vcr
sum(eyo_vcr$diff)

##how about species, if we used species-specific capture rates?
#CBL especially differs (most common species), others by a few hundred

common_sc <- species_capture %>%
  filter(n >= 10) %>%
  dplyr::select(species,
         scr = mean)
eyo_scr <- left_join(
  eyo_species %>%
    filter(species %in% common_sc$species),
  common_sc
) %>%
  mutate(seyo = round(yearly_count/scr, 0),
         diff = seyo - eyo)

eyo_scr
sum(eyo_scr$diff)

###3. BUSHMEAT DYNAMICS

#how obtained influences inference

p2_bushmeat %>% 
  group_by(how_obtained) %>% 
  dplyr::summarize(obs = n())

#because when bushmeat given or bought, it's usually eaten

p2_bushmeat %>% 
  filter(how_obtained %in% c("A", "D")) %>% 
  group_by(use) %>% 
  dplyr::summarize(obs = n())

##toss out redundant animals for dynamics

bushmeat_keyed <- p2_bushmeat %>% 
  mutate(animal_key = as.factor(paste(villageday,time,species,
                                      how_killed,when_killed,habitat,
                                      side_road,km, sep = "_"))) %>% 
  dplyr::select(animal_key,everything())
  
for_dynamics_whole <- bushmeat_keyed %>% 
  filter(state %in% c("E", "EF", "ED"))

for_dynamics_not_whole <- bushmeat_keyed %>% 
  filter(state %ni% c("E", "EF", "ED"))

toss_extras <- for_dynamics_not_whole  %>% 
  distinct(animal_key, .keep_all = TRUE)

#extra obs of..
nrow(for_dynamics_not_whole) - nrow(toss_extras)

#animals that were counted more than once
key_overcount <- for_dynamics_not_whole %>% 
    group_by(animal_key) %>% 
    filter(n() > 1) %>% 
    distinct(animal_key) %>% 
    pull(animal_key) %>% 
    droplevels()

length(key_overcount) 

#because animals that you tossed extra parts of may have..
#been used for multiple purposes (e.g., one gigot to eat, one to sell)..
#...nullify their use for dynamics
toss_extras_use_fixed <- bind_rows(
toss_extras %>% 
  filter(animal_key %in% key_overcount) %>% 
  mutate(use = NA),
toss_extras %>% 
  filter(animal_key %ni% key_overcount)
)

for_dynamics_all <- bind_rows(for_dynamics_whole, toss_extras_use_fixed) %>% 
  arrange(village,date,time)

#note how similar this is to your offtake algo!

nrow(for_dynamics_all) - eyo_all$offtake_counted

#OK, remember that how obtained changes inference so keep only hunted animals

for_dynamics_hunted <- for_dynamics_all %>% 
  filter(how_obtained == "C")

#side note of interest: 769 total animals we would NOT have observed..
#..if only relying on counting what was hunted

for_dynamics_not_hunted <- for_dynamics_all %>% 
  filter(how_obtained != "C")

#OK functions to get dynamics

#get total observations (not paying attention to criteria for dynamics)
get_obs <- function (x) {
  x  %>%
    dplyr::summarise(observations = n()) 
}

#summarize dynamics

summarize_bushmeat <- function (x) {
  x  %>%
    dplyr::summarise(#observations = n(),
              "% Full" = round(mean(state == "E" |
                                      state == "EF" |
                                      state == "ED",
                                    na.rm = TRUE),2)*100,
              "% Butchered" = round(mean(state == "MI" |
                                           state == "MIF" |
                                           state == "MID" |
                                           state == "G" |
                                           state == "GF" |
                                           state == "GD" |
                                           state == "MR" |
                                           state == "MRF" |
                                           state == "MRD" |
                                           state == "TR" |
                                           state == "TF" |
                                           state == "TD",
                                         na.rm = TRUE),2)*100,
              "% Skin" = round(mean(state == "P" |
                                      state == "PF" |
                                      state == "PD",
                                    na.rm = TRUE),2)*100,
              "% Decomposed" = round(mean(state == "ED" |
                                            state == "MID" |
                                            state == "GD" |
                                            state == "MRD" |
                                            state == "PD" |
                                            state == "TD",
                                          na.rm = TRUE),2)*100,
              "% Smoked" = round(mean(state == "EF" |
                                        state == "MIF" |
                                        state == "GF" |
                                        state == "MRF" |
                                        state == "PF" |
                                        state == "TF",
                                      na.rm = TRUE),2)*100,
              "% Consumption" = round(mean(use == "M", na.rm = TRUE),2)*100,
              "% Sale" = round(mean(use == "V", na.rm = TRUE),2)*100,
              "% Ceremony" = round(mean(use == "C", na.rm = TRUE),2)*100,
              "% Medicine" = round(mean(use == "S", na.rm = TRUE),2)*100,
              "% Gun" = round(mean(how_killed == "F", na.rm = TRUE),2)*100,
              "% Trap" = round(mean(how_killed == "P", na.rm = TRUE),2)*100,
              "% Night" = round(mean(when_killed == "N", na.rm = TRUE),2)*100,
              "% Day" = round(mean(when_killed == "J", na.rm = TRUE),2)*100,
              "% Open forest" = round(mean(habitat == "FO", na.rm = TRUE),2)*100,
              "% Closed forest" = round(mean(habitat == "FF", na.rm = TRUE),2)*100,
              "% Swamp" = round(mean(habitat == "M", na.rm = TRUE),2)*100,
              "% River" = round(mean(habitat == "R", na.rm = TRUE),2)*100,
              "% Plantation" = round(mean(habitat == "P", na.rm = TRUE),2)*100,
              "% North" = round(mean(side_road == "N", na.rm = TRUE),2)*100,
              "% South" = round(mean(side_road== "S", na.rm = TRUE),2)*100,
              #"% Hunted" = round(mean(how_obtained == "C", na.rm = TRUE),2)*100,
              #"% Bought" = round(mean(how_obtained == "A", na.rm = TRUE),2)*100,
              #"% Given" = round(mean(how_obtained == "D", na.rm = TRUE),2)*100,
              "% Village" = round(mean(where_obtained == "V", na.rm = TRUE),2)*100,
              "% Elsewhere" = round(mean(where_obtained == "H", na.rm = TRUE),2)*100,
              "% Kitchen" = round(mean(location == "M", na.rm = TRUE),2)*100,
              "% Roadside" = round(mean(location == "R", na.rm = TRUE),2)*100,
              "% Female" = round(mean(sex == "F", na.rm = TRUE),2)*100,
              "% Male" = round(mean(sex == "M", na.rm = TRUE),2)*100,
              "Mean km from village" = round(mean(km, na.rm = TRUE),2)
    ) #%>%
    #arrange(desc(observations))
}

species_summary <- summarize_bushmeat(for_dynamics_hunted  %>%
                                        group_by(species))

all_summary <- summarize_bushmeat(for_dynamics_hunted) %>%
  mutate(species = "ALL") %>%
  dplyr::select(species,everything())

village_summary <- summarize_bushmeat(for_dynamics_hunted  %>%
                                        group_by(village))

vs_summary <- summarize_bushmeat(for_dynamics_hunted  %>%
                                        group_by(village,species))

####add observations

species_obs <- get_obs(p2_bushmeat %>%
                         group_by(species)) 

village_obs <- get_obs(p2_bushmeat %>%
                         group_by(village)) 

vs_obs <- get_obs(p2_bushmeat %>%
                         group_by(village,species)) 

all_obs <- get_obs(p2_bushmeat) %>%
  mutate(species = "ALL") %>%
  dplyr::select(species,everything())

species_summary <- left_join(
  species_summary,
  species_obs,
  by = "species")  %>% 
  dplyr::select(species,observations,everything()) %>% 
  arrange(desc(observations))

all_summary <- left_join(
  all_summary,
  all_obs,
  by = "species")  %>% 
  dplyr::select(species,observations,everything()) 

#joins are capricious with village and vs

village_summary_tmp <- left_join(
  village_summary,
  village_obs,
  by = "village") 

village_summary_tmp2 <- village_summary_tmp %>% 
  ungroup() %>% 
  dplyr::select(village,observations,everything()) 

vs_summary_tmp <- left_join(
  vs_summary,
  vs_obs,
  by = c("village", "species"))

vs_summary_tmp2 <-  vs_summary_tmp %>% 
  ungroup() %>% 
  dplyr::select(village,species,observations,everything()) 

village_summary <- village_summary_tmp2
vs_summary <- vs_summary_tmp2

##for price we only want it with WHOLE animals for SALE when HUNTED
#round to nearest 500 (less than that not meaningful)

summarize_price <- function (x) {
  x %>%
    filter(state == "E" & use == "V" & how_obtained == "C"|
             state == "ED" & use == "V" & how_obtained == "C"|
             state == "EF" & use == "V" & how_obtained == "C") %>%
    dplyr::summarise(exact_price = round(mean(price, na.rm = TRUE), 0)) %>%
    mutate("Mean price (FCFA)" = plyr::round_any(as.numeric(exact_price), 500)) %>%
    dplyr::select(-exact_price)
}

all_price <- summarize_price(p2_bushmeat) %>%
  mutate(species = "ALL") %>%
  dplyr::select(species,everything())

species_price <- summarize_price(p2_bushmeat %>%
                                   group_by(species))


village_price <-  summarize_price(p2_bushmeat %>%
                                    group_by(village))

vs_price <-  summarize_price(p2_bushmeat %>%
                                    group_by(village,species))


##add the price
species_summary <- left_join(
  species_summary,
  species_price,
  by = "species"
)

all_summary <- left_join(
  all_summary,
  all_price,
  by = "species"
)

village_summary <- left_join(
  village_summary,
  village_price,
  by = "village"
)

vs_summary <- left_join(
  vs_summary,
  vs_price,
  by = c("village","species")
)


##add offtake estimations
make_eyo_pretty <- function (x) {
  x$eyo_range <- paste0(x$eyo, " (",
                        x$low_eyo, "-",
                        x$high_eyo, ")")
}

eyo_species$eyo_range <- make_eyo_pretty(eyo_species)
eyo_all$eyo_range <- make_eyo_pretty(eyo_all)

eyo_village$eyo_range <- make_eyo_pretty(eyo_village)
eyo_species_village$eyo_range <- make_eyo_pretty(eyo_species_village)

species_summary <- left_join(
  species_summary,
  eyo_species,
  by = "species")

all_summary <- left_join(
  all_summary,
  eyo_all,
  by = "species")


#make pretty

make_summary_pretty <- function (x) {
  x %>%
    dplyr::select(species,
                  "Observations" = observations,
                  "Estimated observed offtake (EOO)" = offtake_counted,
                  "Annual EOO" = yearly_count,
                  "Mean estimated annual offtake (EAO)" = eyo,
                  "Low CI estimated annual offtake" = low_eyo,
                  "High CI estimated annual offtake" = high_eyo,
                  "EAO with CI" = eyo_range,
                  everything())
}

species_summary <- make_summary_pretty(species_summary)
all_summary <- make_summary_pretty(all_summary)


species_summary <- left_join(
  species_summary,
  species_meta %>%
    dplyr::select(species,english,IUCN,protection)
) %>%
  dplyr::select(
    "Common name" = english,
    IUCN,
    "Gabonese status" = protection,
    everything(), -species)

species_summary$`Gabonese status` <- recode(species_summary$`Gabonese status`,
                                            `Intégralement Protégée` = "Fully protected",
                                            `Non Protégée` = "Not protected",
                                            `Partiellement Protégée` = "Partially protected"
)

all_summary <- all_summary %>%
  mutate("Common name" = "All species",
         IUCN = NA,
         "Gabonese status" = NA) %>%
  dplyr::select(`Common name`,IUCN,
         `Gabonese status`,
         everything(), -species)

identical(colnames(species_summary),colnames(all_summary))

bushmeat_dynamics <- bind_rows(all_summary,species_summary) %>% 
  arrange(desc(`Mean estimated annual offtake (EAO)`))

View(bushmeat_dynamics)

write_csv(bushmeat_dynamics, "./outputs/bushmeat_dynamics.csv")

###shiny things

village_summary
vs_summary
eyo_village
eyo_species_village
offtake_counted_species_village_session

write_csv(village_summary, "./outputs/village_summary.csv")
write_csv(vs_summary, "./outputs/vs_summary.csv")
write_csv(eyo_village, "./outputs/eyo_village.csv")
write_csv(eyo_species_village, "./outputs/eyo_species_village.csv")
write_csv(offtake_counted_species_village_session, "./outputs/offtake_counted_species_village_session.csv")

###4. DUIKER RANK

#general order (code along expected hunted)

duiker_code <- c("CBL", "CPE", "CVB",
                 "CBA", "CFN", "CDJ")

spp_code <- species_meta %>% 
  dplyr::select(species,english)

##make expected ranks
##most hunted as density from fa 2005 (expect more dense more hunted)
#distance village using kg from carto data (expecte heaver species farther)
#% trapped home range sizes from yasuoka 2015 (expect larger home range more trapped)
#% open forest habitat types yasuoka 2015 and o'brien 2019

duiker_kg <- carto_huntmeat %>% 
  filter(species %in% duiker_code & state == "E") %>% 
  group_by(species) %>% 
  dplyr::summarize(n = n(),
            mean_kg = mean(kg, na.rm = T)) %>% 
  arrange(desc(mean_kg))


duiker_expected_rank <- tibble(species = factor(duiker_code, levels = duiker_code),
                               rank = "expected",
                               most_hunted = c(1,2,3,4,5,6),
                               farthest = c(6,2,4,3,5,1),
                               percent_trapped = c(6,4,3,2,5,1),
                               percent_open_forest = c(5,3,1,2,6,4),
                               fit = NA)


#get ranks from data

duiker_data <- left_join(
species_summary %>% 
  dplyr::select(`Common name`,
        `Mean estimated annual offtake (EAO)`,
          `Mean km from village`,
         `% Trap`,
         `% Open forest`),
spp_code,
by = c(`Common name` = "english")
) %>% 
  dplyr::select(species, `Common name`, everything()) %>% 
  filter(species %in% duiker_code) %>% 
  droplevels() 

duiker_data

rank_duiker <- function (x) {
  duiker_data %>% 
    dplyr::arrange(desc({{x}})) %>% 
    rowid_to_column("rank") %>% 
    dplyr::select(species,rank) 
}

most_hunted <- rank_duiker(`Mean estimated annual offtake (EAO)`) %>% 
  dplyr::rename("most_hunted" = rank)

farthest <- rank_duiker(`Mean km from village`) %>% 
  dplyr::rename("farthest" = rank)

percent_trapped <- rank_duiker(`% Trap`) %>% 
  dplyr::rename("percent_trapped" = rank)

percent_open_forest <- rank_duiker(`% Open forest`) %>% 
  dplyr::rename("percent_open_forest" = rank)

duiker_data_rank <- left_join(
left_join(
left_join(most_hunted,farthest),
percent_trapped 
),
percent_open_forest
) %>% 
  mutate(rank = "data",
         species = factor(species, levels = duiker_code),
         fit = NA) %>% 
  dplyr::select(species,rank,everything()) %>% 
  arrange(species)

#let's compare!
#fit shows the absolute value of how many positions different than expected
#so the higher the value for fit the least expected

duiker_difference_rank <- tibble(duiker_data_rank[,3:6] -
duiker_expected_rank[,3:6]) %>% 
  mutate(species = duiker_expected_rank$species,
         rank = "difference") %>% 
  dplyr::select(species,rank,everything())

duiker_difference_rank$fit = rowSums(abs(duiker_difference_rank[,3:6]))

duiker_ranks <- bind_rows(duiker_expected_rank, 
          duiker_data_rank, 
          duiker_difference_rank)

duiker_ranks <- left_join(duiker_ranks,spp_code) %>% 
  dplyr::select("code" = species,
         "common_name" = english,
         everything())

write_csv(duiker_data, "./outputs/duiker_data.csv")
write_csv(duiker_ranks, "./outputs/duiker_ranks.csv")

#plot them

dat <- as.data.frame(duiker_ranks)

# Remove duiker

dat$short <- NA
dat$short[dat$common_name=="Blue duiker"] <- "Blue" 
dat$short[dat$common_name=="Peter's duiker"] <- "Peter's" 
dat$short[dat$common_name=="White-bellied duiker"] <- "White-bellied" 
dat$short[dat$common_name=="Bay duiker"] <- "Bay" 
dat$short[dat$common_name=="Black-fronted duiker"] <- "Black-fronted" 
dat$short[dat$common_name=="Yellow-backed duiker"] <- "Yellow-backed" 

sps <- unique(dat$common_name)
cols <- colnames(dat)[4:7]
labs<- c("A","B","C","D")
titles<- c("Most hunted", "Farthest from village", 
           "% Trapped", "% Open forest")

pdf(file = "outputs/figures/duiker_tangle.pdf",
    width = 8,
    height = 6)

par(mfrow=c(2,2))
par(mar=c(3,8,2,8))

j<- 1
i <- 1
for(j in 1:length(cols))
{
  plot(c(0.99,2.01), c(-0.8,-6.1), type="n", xaxt="n", yaxt="n",
       xlab="", ylab="", bty="n", main=titles[j])
  
  for(i in 1:length(sps))
  {
    lines(c(1,2), c(dat[,j+3][dat$common_name==sps[i] & dat$rank=="expected"],
                    dat[,j+3][dat$common_name==sps[i] & dat$rank=="data"])*-1, 
          lwd=4, col=rgb(0.4,0.4,0.4))
  }
  
  # Left axis
  axis(2, at=(dat[,j+3][dat$rank=="expected"])*-1, labels=dat[,"short"][dat$rank=="expected"], las=2)
  
  # Right axis
  axis(4, at=(dat[,j+3][dat$rank=="data"])*-1, labels=dat[,"short"][dat$rank=="data"], las=2)
  axis(1, at=c(1,2), labels=c("Expected", "Observed"))
  #par(xpd=T)
  #text(0.1, -0.9, labs[j], cex=1.2)
  #par(xpd=F)
  
}  
dev.off()  

#5. DUIKER B:M ratio

medium_duiker_code <- c("CMN", "CBA", "CPE", "CFN", "CVB")

vb <- eyo_species_village %>% 
  group_by(village) %>% 
  filter(species == "CBL") %>% 
  dplyr::summarize(b = sum(eyo)) %>% 
  arrange(desc(b))

vm <- eyo_species_village %>% 
  group_by(village) %>% 
  filter(species %in% medium_duiker_code) %>% 
  dplyr::summarize(m = sum(eyo)) %>% 
  arrange(desc(m))

#number of household per village

village_bm <- left_join(vb,vm, by = "village") %>% 
  mutate(bm = b/m) %>% 
  arrange(desc(bm))

village_households <- p2_eff %>% 
  group_by(village) %>% 
  dplyr::summarize(n_household = median(homes_total))

#look at relationship between number of household and B:M ratio

village_bm <-left_join(village_bm,village_households) %>% 
  dplyr::select(village,n_household,everything())

hist(village_bm$n_household)
hist(village_bm$bm)

#looks like no relationship

plot(village_bm$n_household,
     village_bm$bm, pch = 19,
     xlab ="Number of households",
     ylab = "B:M duiker ratio")

#let's do some simple indicators

village_indicators <- village_bm %>% 
  dplyr::select(village,n_household,bm)

#briefly also offtake by n_households

eyo_households <- left_join(
eyo_village %>% 
  dplyr::select(village,eyo),
village_households
) %>% 
  dplyr::select(village,n_household,eyo)

village_indicators <- left_join(
village_indicators,
eyo_households %>% 
  dplyr::select(village,eyo),
by = "village"
)

plot(village_indicators$n_household,
     village_indicators$eyo, pch = 19,
     xlab ="Number of households",
     ylab = "Estimated annual offtake")

#and kg 

biomass_households <- left_join(
eyo_species_village, 
species_meta %>% 
  dplyr::select(species,bodymass_kg)
)

#village species combos with no estimated kg
nrow(biomass_households %>% 
  filter(is.na(bodymass_kg)))

#% of the dataset
nrow(biomass_households %>% 
       filter(is.na(bodymass_kg)))/
nrow(biomass_households)

biomass_households <- biomass_households %>% 
  filter(!is.na(bodymass_kg)) %>% 
  mutate(eykg = round(eyo*bodymass_kg,0)) %>% 
  dplyr::select(village,species,eykg) %>% 
  group_by(village) %>% 
  dplyr::summarize(eykg = sum(eykg)) %>% 
  arrange(desc(eykg))

village_indicators <- left_join(
  village_indicators,
  biomass_households,
  by = "village"
)

##and % sold

p_sold_village <- for_dynamics_hunted %>% 
  group_by(village) %>% 
  dplyr::summarize(p_sold = round(mean(use == "V", na.rm = TRUE),2)*100)

village_indicators <- left_join(
village_indicators,
p_sold_village
)

#plot the simple indicators

indicator_plot <- function(choose_x,choose_y,
                           choose_xlab,choose_ylab) {
  plot(choose_x, choose_y,
       xlab = choose_xlab,
       ylab = choose_ylab,
       pch = 19)  
}

indicator_plot(village_indicators$n_household, village_indicators$bm, 
               "Number of households",
               "Blue to medium duiker offtake ratio")

indicator_plot(village_indicators$n_household, village_indicators$eyo, 
               "Number of households",
               "Estimated annual offtake")

indicator_plot(village_indicators$n_household, village_indicators$eykg, 
               "Number of households",
               "Estimated annual biomass (kg)")

#biomass is highly correlated with offtake, as expected

indicator_plot(village_indicators$eyo, village_indicators$eykg, 
               "Estimated annual offtake",
               "Estimated annual biomass (kg)")

##what about hunting territory for the 9 villages with it mapped?
#add average offtake and distance walked per hunt

village_indicators <- left_join(
village_indicators,
kde95 %>% 
  dplyr::select(-gps_hunts)
) %>% 
  mutate(offtake_dens = eyo/kde95,
         kg_dens = eykg/kde95)

#add average offtake and distance walked per hunt

all_hunts <- all_in

hunt_carto_summs <- all_hunts %>%
  mutate(offtake_km = offtake/total_km_walked,
         kg_km = kg/total_km_walked) %>% 
  group_by(village) %>% 
  dplyr::summarize(n_hunts = n(),
            offtake_hunt = mean(offtake, na.rm = T),
            offtake_km = mean(offtake_km, na.rm = T),
            kg_hunt = mean(kg, na.rm = T),
            kg_km = mean(kg_km, na.rm = T),
            max_km = mean(max_km_village, na.rm = T))

#and kg per animal

kg_animal_village <- left_join(
carto_huntmeat, 
all_hunts %>% 
  dplyr::select(hunt,village)
) %>% 
  group_by(village) %>% 
  filter(state == "E") %>% 
  dplyr::summarize(kg_animal = mean(kg, na.rm = T))

#join the above

village_indicators <- left_join(
left_join(
village_indicators,
hunt_carto_summs %>% 
  dplyr::select(-n_hunts)
),
kg_animal_village 
)

#add bm per hunt

length(unique(carto_huntmeat %>% 
                filter(species %in% c(medium_duiker_code, "CBL")) %>% 
              pull(hunt)))


left_join(
  carto_huntmeat, 
  all_hunts %>% 
    dplyr::select(hunt,village)
) %>% 
  filter(species %in% c(medium_duiker_code, "CBL")) %>% 
  group_by(species, hunt, village) %>% 
  dplyr::summarize(count = n()) %>% 
  pivot_wider(names_from = species, values_from = count) %>% 
  replace(is.na(.), 0) %>% 
  mutate(bmh = CBL / (CBA + CPE + CFN + CVB + CMN))

#keep what you want and clean it up
#drop p_sold as missing in E4

vic <- village_indicators %>% 
  dplyr::select(village,eykg,eyo,kde95,offtake_dens,kg_dens,
         offtake_hunt,offtake_km,kg_hunt,kg_km,
         kg_animal,
         max_km,
         bm
         )

#top plot

vip <- vic

colnames(vip) <- c("Village", "$kg~year^-1", "$Animals~year^-1",
                  "Catchment size", "$Animals~year^-1~km^-2","$kg~year^-1~km^-2",
                  "$Animals~hunt^-1", "$Animals~km^-1",
                  "$kg~hunt^-1", "$kg~km^-1","$kg~animal^-1",
                  "km village", "B:M offtake")

write_csv(vip, "outputs/all_village_indicators.csv")

vip_all <- vip %>% 
  select_if(~ !any(is.na(.)))

vip_carto <- vip %>% 
  drop_na()

vip_cut <- vip %>% 
  filter(Village %ni% c("E4", "E5", "E10")) %>% 
  drop_na()

vip_noE5 <- vip_carto %>% 
  filter(Village != "E5") %>% 
  drop_na()

vip_noE10 <- vip_carto %>% 
  filter(Village != "E10") %>% 
  drop_na()

vip_noE4 <- vip_carto %>% 
  filter(Village != "E4") %>% 
  drop_na()


make_corr <- function (x) {
  cor(x %>% 
      dplyr::select(-Village))
}  


#plot the correlation

#set colours

rgb2hex <- function(x){
  hex <- as.hexmode(x)
  hex <- as.character(hex)
  #   hex <- substr(hex,2,3)
  hex <- matrix(hex, nrow = nrow(x), ncol = ncol(x))
  hex <- apply(hex,1, function(x){paste0(x,collapse = '')})
  hex <- paste0('#',hex)
  hex
}

col2hex <- function(col){
  rgb <- col2rgb(col)
  hex <- rgb2hex(t(rgb))
  hex
}

colorInterpolate <- function(val, color=cln, valRange=range(val)){
  appx <- approx(valRange, seq(0, 1, length.out = length(valRange)), val, yleft = 0, yright = 1)
  cc <- colorRamp(color)(appx$y)
  cc <- rgb2hex(floor(cc))
  cc
}


cln <- c("#67a9cf","#f7f7f7","#ef8a62")
ColorRamp <- colorInterpolate(1:100, color = cln)

#if you need levels

#min <- -1
#max <- 1
#ColorLevels <- seq(min, max, length=length(ColorRamp))

#plot the correlations

vic_corrplot <- function (x) {
  corrplot(x, 
           #method = "color",
           addgrid.col = NA,
           col = ColorRamp,
           type = "lower", 
           #order = "hclust", 
           cl.pos = "b", 
           #tl.pos = "d", 
           tl.srt = 45, #rotation
           tl.col = "black", 
           tl.offset = 1,
           #tl.cex = 0.9,
           diag = FALSE)
}


#make your plot

pdf(file = "outputs/figures/indicator_corr_plot.pdf",
    width = 9.5,
    height = 7)

vic_corrplot(make_corr(vip_carto))

#add lines and labels
lines(c(0.5,0.5), c(0.5, 11.5), lwd=2, lty=1, col = "darkgrey") #yborder
lines(c(0.5, 5.5), c(7.5, 7.5), lwd=2, lty=1, col = "darkgrey") #village metrics
lines(c(0.5, 11.5), c(1.5, 1.5), lwd=2, lty=1, col = "darkgrey") #hunt metrics

par(xpd=T)
lines(c(-3.8,-3.8), c(1.9,7.1), lwd=2)#, col="darkgrey")
text(-4.1,4.5, "Hunt-level", srt =90)

lines(c(-3.8,-3.8), c(7.9, 11.1), lwd=2)#, col="darkgrey")
text(-4.1,9.5, "Village-level", srt =90)
par(xpd=F)

par(xpd=T)
text(6,-1.1,"Correlation")

dev.off()

#note removing E4 changes things

vic_corrplot(make_corr(vip_noE5))
vic_corrplot(make_corr(vip_noE10))
vic_corrplot(make_corr(vip_noE4))

## MINIMUM DENSITIES COMPARED TO KOERNER DATA
#drop E5 (ceremonial hunts exaggerate hunting area)

dtmp <- eyo_species_village %>% 
  filter(village %in% kde95$village & 
           village != "E5") %>% 
  droplevels() %>% 
  dplyr::select(village,species,eyo,low_eyo,high_eyo)

odens <- bind_rows(
dtmp %>% 
  filter(species %in%  c("CPO", "MJG", "HOC", "CBL"),
         ) %>% 
  group_by(village,species) %>% 
  arrange(village,species),
dtmp %>% 
  filter(species %in%  medium_duiker_code) %>% 
  group_by(village) %>% 
  dplyr::summarize(eyo = sum(eyo),
            low_eyo = sum(low_eyo),
            high_eyo = sum(high_eyo)) %>% 
  mutate(species = "CMN") %>% 
  dplyr::select(village, species, everything()) %>% 
  arrange(village) 
) %>% 
  arrange(village, species)


odens <- left_join(odens,kde95) %>% 
  mutate(mean_mindens = round(eyo/kde95,2),
         low_mindens = round(low_eyo/kde95,2),
         high_mindens = round(high_eyo/kde95,2))


kdens <- tibble(species = c("HOC", "CPO", "MJG",
                            "CMN", "CBL"),
                near_kdens = c(6.01,4.54,2.14,1.20,1.31),
                int_kdens = c(11.19,8.13,4.34,2.67,2.18),
                far_kdens = c(17.42,13.17,8.10,7.33,3.43))


dens <- left_join(
left_join(odens,kdens) %>% 
  dplyr::select(village,kde95,species,everything()),
species_meta %>% 
  dplyr::select(species,english)) 

dens <- dens %>% 
  ungroup() %>% 
  dplyr::select(village, kde95,species,
         english, everything()) %>% 
  mutate(diff_mean_near = mean_mindens - near_kdens,
           diff_mean_int = mean_mindens - int_kdens) %>% 
  arrange(species)

dens %>% 
  print(n = Inf)

#get b:m for both methods as well
#and make data frame for duiker koerner and duiker offtake  

slice_kdens <- function (x) {
    kdens %>% 
      filter(species == x) %>% 
      dplyr::select(-species) %>% 
      slice() %>% 
      as.numeric()  
}

blue_kdens <- slice_kdens("CBL")
medium_kdens <-slice_kdens("CMN")

dk <- tibble(range = c("near", "int", "far"),
             bdens = blue_kdens,
             mdens = medium_kdens,
bm = round(blue_kdens/ medium_kdens , 2))

do <- dens %>% 
  filter(species %in% c("CBL", "CMN")) %>% 
  dplyr::select(village,species,mean_mindens) %>% 
  pivot_wider(names_from = species, 
               values_from = mean_mindens) %>% 
  dplyr::select(village,bdens = CBL, mdens = CMN)

dtmp <- village_indicators %>% 
  filter(village %in% do$village) %>% 
  dplyr::select(village,kde95,bm) 

do <- left_join(do,dtmp) %>% 
  dplyr::select(village,kde95,bdens,mdens,bm) %>% 
  arrange(kde95)

##add vanvliet densities 

duiker_kg

dv <- tibble(bdens = 24.06 / 4.23,
             bdens75 = 24.06 / (4.23*0.75) ,
             bdensc = 24.06 / 4.39,
             pdens = 19.12/(17.8), #CPE
             pdens75 = 19.12/(17.8*0.75),
             pdensc = 19.12/(14.8),
             adens = 12.94/(17.8),#CBA
             adens75 = 12.94/(17.8*0.75),
             adensc = 12.94/(14.2)) %>% 
  mutate(mdens = pdens + adens,
         mdens75 = pdens75 + adens75,
         mdensc = pdensc + adensc)

#plot the differences across methods

#keep bms for all villages
#me is a fake column to have no x-axis
all_bm <- village_indicators %>% 
  mutate(me = 1) %>% 
  dplyr::select(village,me,bm)

do$me <- 1

minb <- round(min(c(do$bdens, dk$bdens)),0)
maxb <- round(max(c(do$bdens, dk$bdens)),0)
minm <- round(min(c(do$mdens, dk$mdens)),0)
maxm <- round(max(c(do$mdens, dk$mdens)),0)
minbm <- round(min(c(all_bm$bm, dk$bm)),0)
maxbm <- round(max(c(all_bm$bm, dk$bm)),0)

b_lab <- expression(paste("Blue duiker offtake "," km"^"-2",""))
m_lab <- expression(paste("Medium-sized duiker offtake "," km"^"-2",""))
k_leg <- expression(bold(paste("Living animals ", "km"^"-2", " (Koerner et al., 2017)")))
v_leg <- expression(bold(paste("Offtake ", "km"^"-2", " (van Vliet and Nasi 2008a)")))

dev.off()

pdf(file = "outputs/figures/duiker_densities.pdf",
    width = 9.4,
    height = 5.8)

par(mfrow=c(1,3))
par(mgp=c(3.5,1,0))
par(mar=c(5,6,4,2)+0.1)
plot(NULL,
     xlim = c(0.96,1.04),
     cex.lab = 1.8, cex.axis=1.6,
     ylim = c(minb,maxb),
     xaxt = "n",
     xlab = "",
     ylab = b_lab)
abline(h = dk$bdens[1], lty = 3, lwd = 3, col = "palegreen4")
abline(h = dk$bdens[2], lty = 2, lwd = 3, col = "palegreen4")
abline(h = dk$bdens[3], lty = 1, lwd = 3, col = "palegreen4")
abline(h = dv$bdensc, lty = 1, lwd = 3, col = "plum3")
abline(h = dv$bdens, lty = 2, lwd = 3, col = "plum3")
abline(h = dv$bdens75, lty = 3,lwd = 3, col = "plum3")
points(do$bdens ~ jitter(do$me), pch = 19, cex = 1.8)
box( col = 'black')

plot(NULL,
     xlim = c(0.96,1.04),
     cex.lab = 1.8, cex.axis=1.6,
     ylim = c(minb,maxb),
     xaxt = "n",
     xlab = "",
     ylab = m_lab)
abline(h = dk$mdens[1], lty = 3, lwd = 3, col = "palegreen4")
abline(h = dk$mdens[2], lty = 2, lwd = 3, col = "palegreen4")
abline(h = dk$mdens[3], lty = 1, lwd = 3, col = "palegreen4")
abline(h = dv$mdensc, lty = 1, lwd = 3, col = "plum3")
abline(h = dv$mdens, lty = 2, lwd = 3, col = "plum3")
abline(h = dv$mdens75, lty = 3, lwd = 3, col = "plum3")
points(do$mdens ~ jitter(do$me), pch = 19, cex = 1.8)
box( col = 'black')

#add legend (xpd fits everyting in, post 4 means left-aligned)
plot.new()
text(-0.51,1, xpd = TRUE, pos = 4, cex = 1.4, k_leg)
legend(#x = "topleft",
  -0.11,0.98,
  box.lty=0,
  #title ="whateves",
  #title.adj = 9, 
  legend=c("Near (< 6 km)", 
           "Intermediate (6-15 km)",
           "Far (> 15 km)"),
  col=c(rep("palegreen4",3)), 
  lty=c(3,2,1),
  lwd = c(rep(3,3)),
  cex=1.4
)
text(-0.51,0.78, xpd = TRUE, pos = 4, cex = 1.4, v_leg)
legend(-0.11,0.76,
       #x = "left",
       box.lty=0,
       #title = "whateves",
       #title.adj = 9, 
       legend=c("75% of literature weight",
                "Reported mean weight",
                "Mean weight this study"),
       col=c(rep("plum3",3)), 
       lty=c(3,2,1),
       lwd = c(rep(3,3)),
       cex=1.4
)

dev.off()

