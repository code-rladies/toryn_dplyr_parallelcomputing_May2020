#### Load necessary packages into your library ####
# navigate to cheatsheet for dplyr by 
# Help > Cheatsheets > Data Transformation with dplyr

#install.packages(c("dplyr","lubridate","readr","tidyr"))
library(dplyr)        # functions for manipulated variables in a dataframe
library(lubridate)    # contains functions for handling date/time data
library(readr)        # efficient functions for reading and writing data
library(tidyr)        # functions for changing shape of data

#### Summary of stopping distance for speeds greater than 15 MPH ####

summary(cars$dist[cars$speed>=15])      # using indexing

summary(subset(cars,speed >= 15, dist)) # using base function subset

cars %>%                                # using pipe operator
  filter(speed >= 15) %>% 
  select(dist) %>% 
  summary()

#### Intro to spread and gather ####
df_wide <- data.frame(row = LETTERS[1:3], a = 1:3, b = 4:6, c = 7:9)
df_wide
df_long <- pivot_longer(df_wide, -row, names_to = "lowercase",
                        values_to = "numbers")
df_long

df_long %>% 
  pivot_wider(row, names_from = lowercase, values_from = numbers)

#### Read in datasets ####
purchase <- read_csv("Data/approved_data_purchase_v5.csv",na = c("", "NA","NULL")) # NULL is not a default NA character string
zipcodes <- read_csv("Data/zipcodes.csv")

#### Explore variables in Purchase ####
str(purchase)      # how did R read in the data?
# same as clicking blue triangle in the environment tab
summary(purchase)  # 5 number summary of numeric and lengths of characters
unique(purchase$la_event_type_cat) # see the major category types
table(purchase$la_event_type_cat)

# remove instances related to parking and with a year in the future
# also remove variables not of interest ID and user variables (mostly NAs)
purchase <- purchase %>% 
  filter(la_event_type_cat!="PARKING",year(event_date_time)<2019) %>% 
  select(-primary_act_id,-secondary_act_id,-purch_party_lkup_id,-c(fin_mkt_nm:dist_to_ven))

#### Creating a new variable with mutate ####
purchase <- purchase %>% 
  mutate(total_sale = tickets_purchased_qty*trans_face_val_amt,
         days_to_event = difftime(event_dt,sales_ord_tran_dt,units = "days"),
         sale_hour = hour(sales_ord_create_dttm))

# playing with string variables
purchase <- purchase %>% 
  unite(combined_cat , c("major_cat_name","minor_cat_name"),remove = F) %>% # combine major and minor cat name into a new category keeping the original variables
  separate(onsale_dt, c("onsale_year","onsale_month","onsale_day"),"-",remove=F) # separate the onsale date into 3 variables: Year, Month, Day

#### Create a contingency table of days_to_event for major_cat_name by delivery_type_cd ####

#Wide Data
purchase %>% 
  select(major_cat_name,delivery_type_cd,days_to_event) %>% # select the three variables for the table
  group_by(major_cat_name, delivery_type_cd) %>%             # group by major and minor categories
  summarise(mean_days = mean(as.numeric(days_to_event))) %>%          # summarize days_to_event
  pivot_wider(names_from = major_cat_name, 
              values_from = mean_days, 
              values_fill = list(mean_days=0)) %>%  # put major category across columns (key)
  View()                                                        # open in Viewer

#### Create a summarised dataset by zip code for plotting ####

purchase_latlong <- purchase %>% 
  group_by(venue_postal_cd_sgmt_1) %>% 
  select(tickets_purchased_qty,
         trans_face_val_amt,
         total_sale,
         days_to_event) %>% # select will also select any grouping variables
  summarize_all(.funs = list(a=mean)) %>% # applies mean to all the variables selected except the grouping variable
  ungroup() # I usually ungroup() at the end, because grouped tibbles can have unexpected behavior

#Join to the Lat/Long data
# this is a one to one join
purchase_latlong <- left_join(purchase_latlong,zipcodes,
                              by=c("venue_postal_cd_sgmt_1"="ZIP")) # left join keeps everything in purchase_latlong, but not zipcodes

# remove observations with NAs for lat/long
purchase_latlong <- purchase_latlong %>% 
  filter(!is.na(LAT),!is.na(LNG))

#### Create a summarised dataset by state for plotting ####

purchase_state <- purchase %>%  
  add_count(venue_state) %>% 
  group_by(venue_state) %>%
  select(tickets_purchased_qty,
         trans_face_val_amt,
         total_sale,
         days_to_event,n) %>% 
  summarise_all(.funs = list(mean=mean))           

purchase_state <- purchase_state %>% 
  rename(count = n_mean)

# remove Canadian provinces and DC
purchase_state <- purchase_state %>% 
  filter(!(venue_state %in% c("ALBERTA","BRITISH COLUMBIA", "DISTRICT OF COLUMBIA",
                              "MANITOBA","ONTARIO","PRINCE EDWARD ISLAND","QUEBEC",
                              "SASKATCHEWAN")))


#### Save the data ####
#write_csv(purchase_latlong,path="purchase_latlong.csv")
#write_csv(purchase_state,path="purchase_state.csv")
