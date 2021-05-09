# Exploratory Data Analysis
# Rebecca Lewis
# Project 2

library("tidyverse")
library("ggplot2")
library("directlabels")

datapath <- '../Data'

#read in data
datafile <- file.path(datapath, 'analysis_dataset.csv')
df <- read.csv(datafile, header=TRUE, na.strings=c("","NA"))

## view info about data
str(df)
summary(df)

################Further cleanup on action year and month###########################################
#remove decimal place and 0 from year and monht
df$action_month <- as.factor(str_sub(as.character(df$action_month),1,-3))
df$action_year <-  as.factor(str_sub(as.character(df$action_year),1,-3))

str(df)
summary(df)

#what are the other years in the action year
df %>% filter(as.character(action_year) > '2020')
#some are for the tag year between 2015 and 2020 but were returned in 2021

df %>% filter(as.character(action_year) > '2021')
#these are ok because they have tag years

df %>% filter(as.character(action_year) < '2015')
#2014 deliveries were for the tag year of 2015 so they are ok, view the ones below

df %>% filter(as.character(action_year) < '2014')

#for items with actions in the 1900's i'm going to assume their action was during the tag_year
df$action_year[as.character(df$action_year) < '2014'] <- df$tag_year[as.character(df$action_year) < '2014']
df$action_year <- droplevels(df$action_year)

str(df)
summary(df)

#view action month levels
levels(df$action_month)
#they all make sense


#convert zip to factor
df$zip = as.factor(df$zip)

str(df)
summary(df)

######################### Univariate Analysis###################################################
# volume - plot with absolute value to see general batch size
ggplot(data = df) +
  geom_histogram(aes(x = abs(volume)), fill = 'cornflower blue', color='white', binwidth = 80) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(title = 'Volume of Tag Deliveries and Returns', subtitle= 'For 2015-2020', x='Number of Tags in the Delivery/Return (abs)', y='Count')


#outliers present; over 1000; 

df %>% filter(volume > 1000)
#this makes sense as it is the organization that manages the tag program.  See how many returns are in
#the same year for this location

df %>% filter(clinic == 'Louisiana SPCA' & tag_year == '2015')
#there are multiple deliveries and returns so this data is valid.


#clinics
p <- ggplot(df) +
  geom_bar(aes(x=clinic), fill = 'cornflower blue', color='white') +
  labs(title = 'Trips to Each Clinic', subtitle= 'For 2015-2020', x='Clinics', y='Count')

p + coord_flip()

#The annual rabies drive and louisiana spca skew this data.  The louisiana spca is on campus so the trips
#are irrelevant.  The annual rabies drive is a special event.  I'm going to subset the data and view again

clinic_subset <- df %>% 
  filter(!clinic %in% c('Louisiana SPCA', 'Annual Rabies Vaccination Drive')) 
#%>%
 # group_by(clinic) %>% arrange(desc(n))

ggplot(clinic_subset) +
  geom_bar(aes(x=fct_rev(fct_infreq(clinic))), fill = 'cornflower blue', color='white') +
  labs(title = 'Trips to Each Clinic', subtitle= 'For 2015-2020; Excluding Louisiana SPCA and Vaccination Drive', x='Clinics') + 
  coord_flip()



#tag year
#show summary of tags delivered and returned for each year (issued)
year_subset <- df %>% 
  filter(as.character(tag_year) >= '2015' & as.character(tag_year) <= '2020') %>%
  group_by(tag_year) %>% summarize(trips = n(), tags=sum(volume))

ggplot(year_subset, aes(x=tag_year, y=trips)) +
  geom_bar(stat="identity", fill = 'cornflower blue', color='white') +
  scale_x_continuous(breaks=c(2015,2016,2017,2018,2019,2020)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_text(aes(label=tags), vjust=1.6, color="white", size=3.5) +
  labs(title = 'Trips Each Tag Year From 2015-2020', subtitle= 'Tag year refers to the issue year for the rabies vaccination. Total volume of tags issued for that year are shown on the bars in white.', x='Tag Year', y='Trips')

#action
ggplot(df, aes(x=action)) +
  geom_bar(stat="count", aes(fill = action)) +
  labs(title = 'Number of Deliveries vs Returns', subtitle= 'From 2015-2020', y='Count', x=element_blank(), fill=element_blank())

#action_date
df$action_date <- as.Date(df$action_date)

date_subset <- df %>% 
  filter(action_date >= '2014-01-01') %>%
  group_by(action_date, tag_year) %>% summarize(tags=sum(volume))

ggplot(date_subset, aes(x=action_date,y=tags)) +
  geom_line() + 
  xlab("") +
  scale_x_date(date_breaks = "4 months", date_labels = "%b %y")+
  facet_wrap(~tag_year, scales = "free_x" ) +
  labs(title='Tags Deliveries and Returns', subtitle="Grouped by Tag Year", y="Number of Tags", x='Action Date')

#action year
action_year_subset <- df %>% 
  filter(action_date >= '2014-01-01') %>%
  group_by(action_year) %>% summarize(trips = n(), tags=sum(volume))

ggplot(action_year_subset, aes(x=action_year, y=trips)) +
  geom_bar(stat="identity", fill = 'cornflower blue', color='white') +
  scale_x_discrete(breaks=c(2014,2015,2016,2017,2018,2019,2020,2021)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_text(aes(label=tags), vjust=1.6, color="white", size=3.5) +
  labs(title = 'Trips associated with the Year of Delivery/Return with Summary of Tags Issued', subtitle= 'Action year refers to the year the tag was delivered or returned regardless of Tag Year.', x='Action Year', y='Trips')

#action month
action_month_subset <- df %>% 
  filter(action_date >= '2014-01-01') %>%
  group_by(action_month) %>% summarize(trips = n(), tags=sum(volume))

ggplot(action_month_subset, aes(x=action_month, y=trips)) +
  geom_bar(stat="identity", fill = 'cornflower blue', color='white') +
  scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_text(aes(label=tags), vjust=1.6, color="white", size=3.5) +
  labs(title = 'Trips associated with the Month of Delivery/Return with Summary of Tags Issued', subtitle= 'Action year refers to the year the tag was delivered or returned regardless of Tag Year.', x='Action Month', y='Trips')

#city
city_subset <- df %>% 
  group_by(city) %>% summarize(trips = n(), tags=sum(volume))

ggplot(city_subset, aes(x=fct_rev(fct_infreq(city)), y=trips)) +
  geom_bar(stat="identity", fill = 'cornflower blue', color='white') +
  #scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_text(aes(label=tags), hjust=-.2, color="black", size=3.5) +
  labs(title = 'Trips by City with Volume Summarized', x='City', y='Trips') +
  coord_flip()

#need to fix Metairie/METAIRIE
df$city = str_to_title(df$city)

city_subset <- df %>% 
  group_by(city) %>% summarize(trips = n(), tags=sum(volume))

ggplot(city_subset, aes(x=fct_rev(fct_infreq(city)), y=trips)) +
  geom_bar(stat="identity", fill = 'cornflower blue', color='white') +
  #scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_text(aes(label=tags), hjust=.5, vjust=-.5, color="black", size=3.5, angle=-90) +
  labs(title = 'Delivery/Return Trips per City with Summary of Tags Issued', subtitle= 'For 2015-2020 Tags', x='City', y='Trips') +
  coord_flip()

#which clinic is New Orleans - Metairie
subset(df, df$city == 'New Orleans - Metairie')
#these belong to a mobile vet clinic that serves new orleans and metairie

blank_city <- subset(df, is.na(df$city))
blank_city$clinic <- droplevels(blank_city$clinic)
levels(blank_city$clinic)

#look up cities for clinics and set
df$city[df$clinic == c("All Cats Mobile Clinic","Batt Veterinary Services")] <- "Metairie"
df$city[df$clinic == c("Doskey Mobile Veterinary Care")] <- "Gretna"
df$city[df$clinic == c("Jester Veterinarian Clinic")] <- "Westwego"
df$city[df$clinic == c("Manhattan Animal Clinic, LLC")] <- "Harvey"
df$city[df$clinic == c("Plaquemines")] <- "Plaquemines"

df$city[df$clinic == c("Louisiana SPCA", "The Inner Pup")] <- "New Orleans"
df$city[df$clinic == c("The Street Dog Coalition","The SULA Foundation")] <- "New Orleans"
df$city[df$clinic == c("Villalobos","Thrive Affordable Vet Care (Midcity)")] <- "New Orleans"

city_subset <- df %>% 
  group_by(city) %>% summarize(trips = n(), tags=sum(volume))

ggplot(city_subset, aes(x=fct_rev(fct_infreq(city)), y=trips)) +
  geom_bar(stat="identity", fill = 'cornflower blue', color='white') +
  #scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_text(aes(label=tags), hjust=.5, vjust=-.5, color="black", size=3.5, angle=-90) +
  labs(title = 'Delivery/Return Trips per City with Summary of Tags Issued', subtitle= 'For 2015-2020 Tags', x='City', y='Trips') +
  coord_flip()

#bivariate
#1. replot clinics and facet wrap by city/zip
clinic_subset <- df %>% 
  filter(!clinic %in% c('Louisiana SPCA', 'Annual Rabies Vaccination Drive')) 

ggplot(clinic_subset) +
  geom_bar(aes(x=fct_rev(fct_infreq(clinic))), fill = 'cornflower blue', color='white') +
  labs(title = 'Deliveries/Returns to Each Clinic by City', subtitle= 'For 2015-2020', x='Clinics') + 
  coord_flip() +
  facet_wrap(~city)
#too many

#2. replot clinics with fill = action
clinic_subset <- df %>% 
  filter(!clinic %in% c('Louisiana SPCA', 'Annual Rabies Vaccination Drive')) %>%
  group_by(clinic, action) %>% summarise(count=n(), tags=sum(abs(volume))) %>%
  mutate(perc=count/sum(count))

#view clinics with more tags rturned than issued - this is where we could improve the tags that were delivered initially
high_return_clinics <- clinic_subset %>% 
  filter(perc > .5 & action=="Returned") %>%
  select(clinic)

return_clinic_subset <- clinic_subset %>% filter(clinic %in% high_return_clinics$clinic)

#number of trips 
ggplot(return_clinic_subset, aes(x=clinic, y=count, fill=action)) +
  geom_bar(stat="identity", color='white') +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust=.5), color="white", size=3.2, angle=0) +
  labs(title = 'Clinics with a More Return Trips than Delivery Trips', subtitle= 'For 2015-2020', x='Clinics', y=element_blank(), fill=element_blank()) + 
  coord_flip()

#qty of tags
ggplot(clinic_subset, aes(x=clinic, y=tags, fill=action)) +
  geom_bar(stat="identity", color='white') +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust=.5), color="white", size=3.2, angle=-90) +
  labs(title = 'Number of Tags by Clinic with Delivery vs. Returns', subtitle= 'For 2015-2020', x='Clinics', y=element_blank(), fill=element_blank()) + 
  coord_flip()

#3. tags issued (sum volume) each year with fill = action
clinic_issued_summary <- df %>% 
  filter(!clinic %in% c('Louisiana SPCA', 'Annual Rabies Vaccination Drive')) %>%
  group_by(clinic) %>% summarise(tags=sum(volume)) %>%
  mutate(issue_perc=tags/sum(tags)) %>%
  arrange(desc(tags))

top_10_issued <- clinic_issued_summary %>%
  slice_max(n=10, order_by=tags) %>% 
  droplevels()

bottom_10_issued <- clinic_issued_summary %>% 
  slice_min(n=10, order_by=tags) %>% 
  droplevels()

ggplot(top_10_issued, aes(x=reorder(clinic, tags), y=tags)) +
  geom_bar(stat="identity", fill = 'cornflower blue',color='white') +
  geom_text(aes(label=scales::percent(issue_perc)), position = position_stack(vjust=.5), color="white", size=3.2, angle=0) +
  labs(title = 'Top 10 Clinics Issuing Tags', subtitle= 'For Tag Year 2015-2020', x=element_blank(), y="Number of Tags Issued", fill=element_blank()) + 
  coord_flip()

ggplot(bottom_10_issued, aes(x=reorder(clinic, tags), y=tags)) +
  geom_bar(stat="identity", fill = 'cornflower blue',color='white') +
  geom_text(aes(label=scales::percent(issue_perc)), position = position_stack(vjust=.5), color="white", size=3.2, angle=0) +
  labs(title = 'Bottom 10 Clinics Issuing Tags', subtitle= 'For Tag Year 2015-2020', x=element_blank(), y=element_blank(), fill=element_blank()) + 
  coord_flip()

#plaquemins is causing an issue viewing the other clinics.  Removing from the clinic subset and taking the bottom 10 again
bottom_10_issued <- clinic_issued_summary %>% 
  filter(!clinic == 'Plaquemines') %>%
  slice_min(n=10, order_by=tags) %>% 
  droplevels()

ggplot(bottom_10_issued, aes(x=reorder(clinic, -tags), y=tags)) +
  geom_bar(stat="identity", fill = 'cornflower blue',color='white') +
  geom_text(aes(label=scales::percent(issue_perc)), position = position_stack(vjust=.5), color="white", size=3.2, angle=0) +
  labs(title = 'Bottom 10 Clinics Issuing Tags', subtitle= 'For Tag Year 2015-2020', x=element_blank(), y="Number of Tags Issued", fill=element_blank()) + 
  coord_flip()
#many of these are not in orleans parish so it makes sense that they are among the bottom 10 clinics

#view top 10 and bottom 10 clinics by year
top_issued_tagyear_subset <- df %>% 
  filter(clinic %in% top_10_issued$clinic) %>%
  group_by(clinic, tag_year) %>% summarise(tags=sum(volume)) %>%
  mutate(issue_perc=tags/sum(tags)) %>% 
  droplevels()

ggplot(top_issued_tagyear_subset %>% slice_max(n=10, order_by=tags), aes(x=clinic, y=tags, fill= factor(as.character(clinic)))) +
  geom_col(show.legend = FALSE) +
  #geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust=.5), color="white", size=3.2, angle=-90) +
  labs(title = 'Top 10 Clinics Tags Issued by Year', subtitle= 'For 2015-2020', x=element_blank(), y="Number of Tags Issued", fill=element_blank()) + 
  scale_x_discrete(limits = rev(levels(top_issued_tagyear_subset$clinic))) +
  facet_wrap(~tag_year) +
  coord_flip()

ggplot(top_issued_tagyear_subset %>% slice_max(n=10, order_by=tags), aes(x=tag_year, y=tags, color= factor(as.character(clinic)))) +
  geom_line(show.legend = FALSE) +
  scale_x_continuous(breaks=2015:2020, labels=c('2015','2016','2017','2018','2019','2020'), expand = expansion(mult = c(.05, .3))) +
  #geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust=.5), color="white", size=3.2, angle=-90) +
  labs(title = 'Top 10 Clinics Tags Issued by Year', subtitle= 'For 2015-2020', x='Tag Year', y='Number of Tags', fill=element_blank()) +
  geom_dl(aes(label = clinic), method = list("last.qp"), cex = 0.8) 


bottom_issued_tagyear_subset <- df %>% 
  filter(clinic %in% bottom_10_issued$clinic) %>%
  group_by(clinic, tag_year) %>% summarise(tags=sum(volume)) %>%
  mutate(issue_perc=tags/sum(tags)) %>% 
  droplevels()

ggplot(bottom_issued_tagyear_subset %>% slice_min(n=10, order_by=tags), aes(x=clinic, y=tags, fill= factor(as.character(clinic)))) +
  geom_col(show.legend = FALSE) +
  #geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust=.5), color="white", size=3.2, angle=-90) +
  labs(title = 'Top 10 Clinics Tags Issued by Year', subtitle= 'For 2015-2020', x=element_blank(), y=element_blank(), fill=element_blank()) + 
  scale_x_discrete(limits = rev(levels(bottom_issued_tagyear_subset$clinic))) +
  facet_wrap(~tag_year) +
  coord_flip()

ggplot(bottom_issued_tagyear_subset %>% slice_max(n=10, order_by=tags), aes(x=tag_year, y=tags, color= factor(as.character(clinic)))) +
  geom_line() +
  #geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust=.5), color="white", size=3.2, angle=-90) +
  labs(title = 'Bottom 10 Clinics Tags Issued by Year', subtitle= 'For 2015-2020', x='Tag Year', y='Number of Tags', fill=element_blank()) 

#4. find all clinics who issued multiple years
clinic_year_count <- df %>% distinct(clinic, tag_year) %>%
  filter(!clinic %in% c('Louisiana SPCA', 'Annual Rabies Vaccination Drive')) %>%
  group_by(clinic, tag_year) %>% summarise(count=n()) %>%
  pivot_wider(names_from = tag_year, values_from = count) %>%
  summarise(total = sum(c_across(where(is.numeric))))

regular_clinics <- clinic_year_count %>% filter(total==6)

regular_issued_summary <- df %>% 
  filter(clinic %in% regular_clinics$clinic) %>%
  group_by(clinic) %>% summarise(tags=sum(volume)) %>%
  mutate(issue_perc=tags/sum(tags)) %>%
  arrange(desc(tags))

bottom_10_regular <- regular_issued_summary %>% 
  slice_min(n=10, order_by=tags) %>% 
  droplevels()

bottom_regular_tagyear_subset <- df %>% 
  filter(clinic %in% bottom_10_regular$clinic) %>%
  group_by(clinic, tag_year) %>% summarise(tags=sum(volume)) %>%
  mutate(issue_perc=tags/sum(tags)) %>% 
  droplevels()

ggplot(bottom_regular_tagyear_subset %>% slice_max(n=10, order_by=tags), aes(x=tag_year, y=tags, color= factor(as.character(clinic)))) +
  geom_line(show.legend = FALSE) +
  scale_x_continuous(breaks=2015:2020, labels=c('2015','2016','2017','2018','2019','2020'), expand = expansion(mult = c(.05, .3))) +
  #geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust=.5), color="white", size=3.2, angle=-90) +
  labs(title = 'Bottom 10 Clinics Tags Issued by Year', subtitle= 'For 2015-2020', x='Tag Year', y='Number of Tags', fill=element_blank()) +
  geom_dl(aes(label = clinic), method = list("last.qp"), cex = 0.8) 

ggplot(bottom_regular_tagyear_subset %>% slice_min(n=10, order_by=tags), aes(x=clinic, y=tags, fill= factor(as.character(clinic)))) +
  geom_col(show.legend = FALSE) +
  #geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust=.5), color="white", size=3.2, angle=-90) +
  labs(title = 'Bottom 10 Clinics by Tags Issued Each Year', subtitle= 'For 2015-2020', x=element_blank(), y="Number of Tags Issued", fill=element_blank()) + 
  scale_x_discrete(limits = rev(levels(bottom_regular_tagyear_subset$clinic))) +
  facet_wrap(~tag_year) +
  coord_flip()

#save dataframe for modeling
write.csv(df, "../Data/modeling_dataset.csv", row.names = FALSE)

bottom_modeling_subset <- df %>% 
  filter(clinic %in% bottom_10_regular$clinic) %>%
  group_by(clinic, tag_year, action_date) %>% summarise(tags=sum(volume)) %>%
  mutate(issue_perc=tags/sum(tags)) %>% 
  droplevels()

top_modeling_subset <- df %>% 
  filter(clinic %in% top_10_issued$clinic) %>%
  group_by(clinic, tag_year, action_date) %>% summarise(tags=sum(volume)) %>%
  mutate(issue_perc=tags/sum(tags)) %>% 
  droplevels()


write.csv(bottom_modeling_subset, "../Data/bottom_clinic_dataset.csv", row.names = FALSE)
write.csv(top_modeling_subset, "../Data/top_clinic_dataset.csv", row.names = FALSE)
