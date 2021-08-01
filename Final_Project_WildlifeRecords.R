library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(scales)
library(flextable)
library(gridExtra)
library(maps)
library(mapdata)
library(viridis)
library(ggrepel)

install.packages("ggrepel")
install.packages("viridis")
install.packages("flextable")
install.packages("data.table")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("devtools")
install.packages("maps")
install.packages("mapdata")
install.packages("ggmap")
install.packages("tinytex")
Sys.getenv("R_ENVIRON")

#uploading raw data files as CSV files
X2014Records <- read_csv("2014Records.csv")
X2016Records <- read_csv("2016Records.csv")
X2018Records <- read_csv("2018Records.csv")
X2019Records <- read_csv("2019Records.csv")
Intakebytown_mappingdata <- read_csv("Top intake Towns WRARI csv.csv")

#starting with 2014 data 
#getinfo and explore 2014 data 
summary(X2014Records)
X2014Records

#clean 2014 data
Records_14 <- rename(X2014Records, date = "Intake Date", species_class = "Species Class", species = "Species", specific_location = "Specific Location", city = "City Found", cause_for_admission = "Cause for Admission")
#extract important columns
Records_14 <- select(Records_14, c(1,2,3,7,8,9))
#save cleaned records14 file to R environment for later use
saveRDS(Records_14, file = "Records_14.Rds")

#change date format for 2014 
Records_14 <- Records_14 %>%
  mutate(date = mdy(date))

#Add month column from date column for 2014
Records_14$month <- month(Records_14$date)

#save copy of Records14 as RDS under new variable
saveRDS(Records_14, file = "Records_14.Rds")
Records_14RDS <- readRDS(file = "Records_14.Rds")


#remove NA for 2014 data
Records_14 <- Records_14 %>%
  filter(!is.na(date))


#plotting the intakes each month against months in the year 2014
updated_14_intakebymonth <- ggplot(Records_14) + aes(x = month(month, label = TRUE)) +
  geom_bar(fill = "#0099f9") +
  labs(x = "Month", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5), plot.caption =  element_text(face = "bold", hjust = 0.5, vjust = -0.6)) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title.x = element_text(vjust =-0.55)) +
  theme(axis.title.y = element_text(vjust =0.55)) +
  labs(title = "Monthly Wildlife Admissions in 2014", caption = "Figure 1: The number of monthly admissions to the wildlife clinic in 2014.")
updated_14_intakebymonth

#save updated intake2014monthly plot
saveRDS(updated_14_intakebymonth, file = "updated_14_intakebymonth.RDS")

#calculate total intakes per month in tibble for 2014 intakes
#monthly total caluclations for intakes admission 
Monthlystats_14 <- Records_14 %>%
  group_by(month) %>%
  tally()

#count monthly intakes in 2014 
Monthlystats_14 <- Records_14 %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  na.omit("Monthlystats_14") %>%
  top_n(12) 
Monthlystats_14

#statistics of monthly 2014 admissions 
mean(Monthlystats_14$count)
#mean of monthly intakes = 177, meaning the average monthly intake in 2014 is 177 intakes per month
min(Monthlystats_14$count)
#minimum intake per month = 41, meaning the lowest number of monthly intakes in 2014 was 41 intakes in a month
max(Monthlystats_14$count)
#maximum intake per month = 458, meaning the maximum number of intakes in a month in 2014 was 458 intakes in a month
median(Monthlystats_14$count)
#median intake per month = 111, meaning of the range for monthly intakes in 2014 the middle number of intakes is 111 intakes in a month

#tibble of monthly intakes, convert numeric month format to abbreviated month 
Monthlystats_14$month <- (month.abb[Monthlystats_14$month])
Monthlystats_14
#convert monthly stats 14 tibble into data frame
Monthlystats_14_table <- data.frame(Monthlystats_14)
#convert monthly stats 14 data frame into table
setDT(Monthlystats_14_table)
#check if this was sucessful in turning data frame into data table 
is.data.table(Monthlystats_14_table)
#rename columns in data table
setnames(Monthlystats_14_table, c("month","count"), c("Month", "Admissions"))
#use flextable package
Monthlystats_14_tableflex <- flextable(Monthlystats_14_table)
#format table
Monthlystats_14_tableflex <- theme_vanilla(Monthlystats_14_tableflex)
Monthlystats_14_tableflex <- border_outer(Monthlystats_14_tableflex)
Monthlystats_14_tableflex <- border_inner_v(Monthlystats_14_tableflex)
Monthlystats_14_tableflex <- set_caption(Monthlystats_14_tableflex, "Table 1: The number of monthly admissions to the wildlife clinic in 2014.")
Monthlystats_14_tableflex
saveRDS(Monthlystats_14_tableflex, file = "Monthlystats_14_tableflex.RDS")

#begin analysis for determining top reasons for admission
#check class for cause for admission column before replacing values
class(Records_14$cause_for_admission)

#replace values to fix spelling and grammar errors
#replace "orphaned" with "Orphaned" to match other variables of the same kind
Records_14$cause_for_admission <- gsub('orphaned', 'Orphaned', Records_14$cause_for_admission)
Records_14

#replace "injured" with "Unknown trauma" to match other variables of the same kind
Records_14$cause_for_admission <- gsub('Injured', 'Unknown trauma', Records_14$cause_for_admission)
Records_14

#top reasons for admission 2014 
admissions_2014 <- Records_14 %>%
  group_by(date, cause_for_admission) %>%
  tally()
admissions_2014

#count frequency of reasons for admission in 2014
admissions_2014 <- na.omit("admissions_2014") 
admissions_2014_count <- Records_14 %>%
  group_by(cause_for_admission) %>%
  summarise(count = n())
admissions_2014_count

#count top 5 common reasons for admission in 2014
admissions_2014 <- na.omit("admissions_2014") 
admissions_2014 <- Records_14 %>%
  group_by(cause_for_admission) %>%
  summarise(count = n()) %>%
  na.omit("admissions_2014") %>%
  top_n(5) 
admissions_2014

#plot top 5 common reasons for admission in 2014
#plot in descending order on x axis
#\n seperates label or title into next line
#added formatting code to axis, title, caption and labels
top5_admissions14_plot <- ggplot(data = admissions_2014, aes(x = reorder(cause_for_admission, -count), y = count)) + 
  geom_bar(stat = "identity", fill = "#69b3a2") +
  labs(x = "Reason for Admission", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5), 
        plot.caption = element_text(face = "bold", hjust = 0.5, vjust = -0.6)) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title.x = element_text(vjust =-0.55)) +
  theme(axis.title.y = element_text(vjust =0.55)) +
  labs(title = "Top Five Reasons for Admission in 2014", caption = "Figure 2: The total number of admissions for each of the \n top five reasons for admission to the wildlife clinic in 2014.")
top5_admissions14_plot
saveRDS(top5_admissions14_plot, file = "top5admissions14_plot.RDS")
save(top5admissions14_plot, file = "top5admissions14_plot.RData")

#making the top5reasonsforadmission2014 in a table format
#make table with top 5 reasons, count, and percentage of total 
#arrange data frame into descending order
admissions_2014 <- admissions_2014 %>%
  arrange(desc(count))

#add percent to admissions14 table
admissions_2014 <- admissions_2014 %>%
  mutate(admissions_2014, Percent_total_intakes = count / sum (admissions_2014_count$count) * 100) 


#convert admissions 14 tibble into data frame
top5_rzn14_table <- data.frame(admissions_2014)
#convert admissions 14 data frame into table
setDT(top5_rzn14_table)

#check if this was sucessful in turning data frame into data table 
is.data.table(top5_rzn14_table)

#rename columns in data table
setnames(top5_rzn14_table, c("cause_for_admission","count", "Percent_total_intakes"), 
         c("Reason Admitted", "Number of Admissions", "Percent of Total Admissions"))
top5_rzn14_table

#round percent column to whole numbers
top5_rzn14_table$`Percent of Total Admissions` <- round(top5_rzn14_table$`Percent of Total Admissions`)
top5_rzn14_table

#use flextable package
top5_rzn14_tableflex <- flextable(top5_rzn14_table)

#format table
#adjust dimensions to autofit
top5_rzn14_tableflex <- top5_rzn14_tableflex %>%
  autofit(add_w = 0, add_h = 0)

#format table
top5_rzn14_tableflex <- theme_vanilla(top5_rzn14_tableflex)
top5_rzn14_tableflex <- border_outer(top5_rzn14_tableflex)
top5_rzn14_tableflex<- set_caption(top5_rzn14_tableflex, "Table 2: The total number of admissions and percentage of total admissions for each of the top five reasons for admission in 2014.")
top5_rzn14_tableflex
saveRDS(top5_rzn14_tableflex, file = "top5_rzn14_tableflex")

#stats for top reasons for admission data (2014)
sum(top5_rzn14_table$`Number of Admissions`)
#top 5 reasons account for 1711 intakes of the 2133 total admissions for the year 2014
#top 5 reasons for intake account for 80% of all intakes that year

#begin analysis for top 5 most common species admitted in 2014 
#count top 5 most common species admitted in 2014

#top species admitted 2014 
topspecies_2014 <- Records_14 %>%
  group_by(species) %>%
  tally()

#count freq of all different species admitted in 2014
topspecies_2014 <- na.omit("topspecies_2014") 
topspecies_2014_count <- Records_14 %>%
  group_by(species) %>%
  summarise(count = n()) 

#count top 5 most common species admitted in 2014
topspecies_2014 <- na.omit("topspecies_2014") 
topspecies_2014 <- Records_14 %>%
  group_by(species) %>%
  summarise(count = n()) %>%
  na.omit("topspecies_2014") %>%
  top_n(5)
topspecies_2014

#plot top 5 species admitted in 2014
#plot in descending order on x axis
#\n seperates label or title into next line
#added formatting code to axis, title, caption and labels

top5_species14_plot <- ggplot(data = topspecies_2014, aes(x = reorder(species, -count), y = count)) + 
  geom_bar(stat = "identity", fill = "#CC3300") +
  labs(x = "Species", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5),  plot.caption = element_text(face = "bold", hjust = 0.5, vjust = -0.8)) +
  theme(axis.text.x = element_text(size = 7, face = "bold", vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title.x = element_text(vjust =-0.55)) +
  theme(axis.title.y = element_text(vjust =0.55)) +
  labs(title = "Top Five Most Frequently Admitted Species in 2014", caption = "Figure 3: The top five most frequently admitted species in 2014\n and the total number of clinic admissions for each.")
saveRDS(top5_species14_plot, file = "top5_species14_plot.RDS")
save(top5_species14_plot, file = "top5_species14_plot.RData")
top5_species14_plot

#making top 5 species table with top 5 species, number admissions (count), and percent total admissions

#arrange tibble into descending order
topspecies_2014 <- topspecies_2014 %>%
  arrange(desc(count))

#add percent to tibble
topspecies_2014 <- topspecies_2014 %>%
  mutate(topspecies_2014, 'Percent of Total Admissions' = count / sum (topspecies_2014_count$count) * 100) 
topspecies_2014

#convert top species tibble into data frame
top5_species14_table <- data.frame(topspecies_2014)

#convert admissions 14 data frame into table
setDT(top5_species14_table)

#check if this was sucessful in turning data frame into data table 
is.data.table(top5_species14_table)

#rename columns in data table
setnames(top5_species14_table, c("species","count", "Percent.of.Total.Admissions"), 
         c("Species", "Number of Admissions", "Percent of Total Admissions"))
top5_species14_table

#round percent column to whole numbers
top5_species14_table$`Percent of Total Admissions` <- round(top5_species14_table$`Percent of Total Admissions`)

#use flextable package
top5_species14_tableflex <- flextable(top5_species14_table)

#format table
#adjust dimensions to autofit
top5_species14_tableflex <- top5_species14_tableflex %>%
  autofit(add_w = 0, add_h = 0)

#format table
top5_species14_tableflex <- theme_vanilla(top5_species14_tableflex)
top5_species14_tableflex <- border_outer(top5_species14_tableflex)
top5_species14_tableflex <- set_caption(top5_species14_tableflex, "Table 3: The number of admissions and percent of total admissions for the top five most frequently admitted species in 2014.")
top5_species14_tableflex
saveRDS(top5_species14_tableflex, file = "top5_species14_tableflex.RDS")
save(top5_species14_tableflex, file = "top5_species14_tableflex.RData")

#stats for top species (2014)

#sum of admissions from all top 5 common species combined
sum(top5_species14_table$'Number of Admissions')
#961 admissions
#961 of 2133 total admissions in 2014 are from top 5 species,
#top 5 species = 45% of all admissions in 2014

#compare admissions from top 5 species with total number of species admitted in 2014
Number_diffspecies_2014_forstats<- Records_14 %>%
  count(species)
#168
#168 different species admitted in 2014
#168 species total. Top 5 most commonly admitted species = almost 50% of all admissions

#Begin 2016 data analysis
#repeat steps above for 2016, clean, # of intakes, top 5 admission reasons, top 5 species admitted

#getinfoabout2016
summary(X2016Records)
X2016Records

#extract important columns only
Records_16<- select(X2016Records, c(1,2,3,8,9))
Records_16
#clean data 2016
Records_16 <- rename(Records_16, date = "Intake Date", species_class = "Species Class", species = "Species", city = "City Found", cause_for_admission = "Cause for Admission")
Records_16

#change date format for 2016 
Records_16 <- Records_16 %>%
  mutate(date = mdy(date))

#Add month column from date column for 2016
Records_16$month <- month(Records_16$date)

#remove NA for 2016 data
Records_16 <- Records_16 %>%
  filter(!is.na(date))

#plotting the intakes each month against months in the year 2016
Intakebymonth_16 <- ggplot(Records_16) + aes(x = month(month, label = TRUE)) +
  geom_bar(fill = "#0099f9") +
  labs(x = "Month", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5), plot.caption =  element_text(face = "bold", hjust = 0.5, vjust = -0.6)) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title.x = element_text(vjust =-0.55)) +
  theme(axis.title.y = element_text(vjust =0.55)) +
  labs(title = "Monthly Wildlife Admissions in 2016", caption = "Figure 4: The number of monthly admissions to the wildlife clinic in 2016.")
Intakebymonth_16
saveRDS(Intakebymonth_16, file = "Intakebymonth_16.RDS")
save(Intakebymonth_16, file = "Intakebymonth_16.RData")

#2016 intake by month data into table 

#calculate total intakes per month in tibble for 2016 intakes
#monthly total calculations for intakes admission 
Monthlystats_16 <- Records_16 %>%
  group_by(month) %>%
  tally()

#count monthly intakes in 2016 
Monthlystats_16 <- Records_16 %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  na.omit("Monthlystats_16") %>%
  top_n(12)

#statistics of monthly 2016 admissions 
mean(Monthlystats_16$count)
#mean of monthly intakes = 233, meaning the average monthly intake in 2016 is 233 intakes per month
min(Monthlystats_16$count)
#minimum intake per month = 38, meaning the lowest number of monthly intakes in 2016 was 38 intakes in a month
max(Monthlystats_16$count)
#maximum intake per month = 607, meaning the maxmimum number of intakes in a month in 2016 was 607 intakes in a month
median(Monthlystats_16$count)
#median intake per month = 185, meaning of the range for monthly intakes in 2016 the middle number of intakes is 185 intakes in a month


#tibble of monthly intakes, convert numeric month format to abbreviated month 
Monthlystats_16$month <- (month.abb[Monthlystats_16$month])

#convert monthly stats 16 tibble into data frame
Monthlystats_16_table <- data.frame(Monthlystats_16)

#convert monthly stats 16 data frame into table
setDT(Monthlystats_16_table)

#check if this was sucessful in turning data frame into data table 
is.data.table(Monthlystats_16_table)

#rename columns in data table
setnames(Monthlystats_16_table, c("month","count"), c("Month", "Admissions"))

#use flextable package
Monthlystats_16_tableflex <- flextable(Monthlystats_16_table)

#format table
Monthlystats_16_tableflex <- theme_vanilla(Monthlystats_16_tableflex)
Monthlystats_16_tableflex <- border_outer(Monthlystats_16_tableflex)
Monthlystats_16_tableflex <- border_inner_v(Monthlystats_16_tableflex)
Monthlystats_16_tableflex <- set_caption(Monthlystats_16_tableflex, "Table 4: The number of monthly admissions to the wildlife clinic in 2016.")
Monthlystats_16_tableflex
saveRDS(Monthlystats_16_tableflex, file = "Monthlystats_16_tableflex.RDS")
save(Monthlystats_16_tableflex, file = "Monthlystats_16_tableflex.RData")

#begin analysis for determining top reasons for admission 2016

#check class for cause for admission column before replacing values
class(Records_16$cause_for_admission)

#replace values to fix spelling and grammar errors
#replace "Orphaned Juvenile" with "Orphaned" to match other variables of the same kind
Records_16$cause_for_admission <- gsub('Orphaned juvenile', 'Orphaned', Records_16$cause_for_admission)

#replace "Injury" with "Unknown trauma" to match other variables of the same kind
Records_16$cause_for_admission <- gsub('Injury', 'Unknown trauma', Records_16$cause_for_admission)

#replace "HBC" with "Hit by car" to match other variables of the same kind
Records_16$cause_for_admission <- gsub('HBC', 'Hit by car', Records_16$cause_for_admission)

#replace "Cat caught" with "Cat attack" to match other variables of the same kind
Records_16$cause_for_admission <- gsub('Cat caught', 'Cat attack', Records_16$cause_for_admission)

#top reasons for admission 2016 
admissions_2016 <- Records_16 %>%
  group_by(date, cause_for_admission) %>%
  tally()

#count freq of reasons for admission in 2016
admissions_2016 <- na.omit("admissions_2016") 
admissions_2016_count <- Records_16 %>%
  group_by(cause_for_admission) %>%
  summarise(count = n()) 

#count top 5 common reasons for admission in 2016
admissions_2016 <- na.omit("admissions_2016") 
admissions_2016 <- Records_16 %>%
  group_by(cause_for_admission) %>%
  summarise(count = n()) %>%
  na.omit("admissions_2016") %>%
  top_n(5) 
admissions_2016

#plot top 5 common reasons for admission in 2016
#plot in descending order on x axis
#\n seperates label or title into next line
#added formatting code to axis, title, caption and labels
top5_admissions16_plot <- ggplot(data = admissions_2016, aes(x = reorder(cause_for_admission, -count), y = count)) + 
  geom_bar(stat = "identity", fill = "#69b3a2") +
  labs(x = "Reason for Admission", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5), plot.caption = element_text(face = "bold", hjust = 0.5, vjust = -0.6)) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title.x = element_text(vjust =-0.55)) +
  theme(axis.title.y = element_text(vjust =0.55)) +
  labs(title = "Top Five Reasons for Admission in 2016", caption = "Figure 5: The total number of admissions for each of the \n top five reasons for admission to the wildlife clinic in 2016.")
top5_admissions16_plot

#making the top 5 reasons for admission 2016 in a table format
#make table with top 5 reasons, count, and percentage of total 

#arrange data frame into descending order
admissions_2016 <- admissions_2016 %>%
  arrange(desc(count))

#add percent to admissions16 table
admissions_2016 <- admissions_2016 %>%
  mutate(admissions_2016, Percent_total_intakes = count / sum (admissions_2016_count$count) * 100) 
admissions_2016
#convert admissions 16 tibble into data frame
top5_rzn16_table <- data.frame(admissions_2016)

#convert admissions 16 data frame into table
setDT(top5_rzn16_table)

#check if this was sucessful in turning data frame into data table 
is.data.table(top5_rzn16_table)

#rename columns in data table
setnames(top5_rzn16_table, c("cause_for_admission","count", "Percent_total_intakes"), 
         c("Reason Admitted", "Number of Admissions", "Percent of Total Admissions"))

#round percent column to whole numbers
top5_rzn16_table$`Percent of Total Admissions` <- round(top5_rzn16_table$`Percent of Total Admissions`)

#use flextable package
top5_rzn16_tableflex <- flextable(top5_rzn16_table)

#format table
#adjust dimensions to autofit
top5_rzn16_tableflex <- top5_rzn16_tableflex %>%
  autofit(add_w = 0, add_h = 0)

#format table
top5_rzn16_tableflex <- theme_vanilla(top5_rzn16_tableflex)
top5_rzn16_tableflex <- border_outer(top5_rzn16_tableflex)
top5_rzn16_tableflex <- set_caption(top5_rzn16_tableflex, "Table 5: The total number of admissions and percentage of total admissions for each of the top five reasons for admission in 2016.")
top5_rzn16_tableflex

#stats for top reasons for admission data (2016)
sum(top5_rzn16_table$`Number of Admissions`)
#top 5 reasons account for 1064 intakes of the 2797 total admissions for the year 2016
#top 5 reasons for intake account for 38% of all intakes that year

#begin analysis for top 5 most common species admitted in 2016
#count top 5 most common species admitted in 2016

#top species admitted 2016 
topspecies_2016<- Records_16 %>%
  group_by(species) %>%
  tally()

#count freq of all different species admitted in 2016
topspecies_2016 <- na.omit("topspecies_2016") 
topspecies_2016_count <- Records_16 %>%
  group_by(species) %>%
  summarise(count = n())

#count top 5 most common species admitted in 2016
topspecies_2016 <- na.omit("topspecies_2016") 
topspecies_2016 <- Records_16 %>%
  group_by(species) %>%
  summarise(count = n()) %>%
  na.omit("topspecies_2016") %>%
  top_n(5)
topspecies_2016

#plot top 5 species admitted in 2016
#plot in descending order on x axis
#\n seperates label or title into next line
#added formatting code to axis, title, caption and labels

top5_species16_plot <- ggplot(data = topspecies_2016, aes(x = reorder(species, -count), y = count)) + 
  geom_bar(stat = "identity", fill = "#CC3300") +
  labs(x = "Species", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5),  plot.caption = element_text(face = "bold", hjust = 0.5, vjust = -0.8)) +
  theme(axis.text.x = element_text(size = 7, face = "bold", vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title.x = element_text(vjust =-0.55)) +
  theme(axis.title.y = element_text(vjust =0.55)) +
  labs(title = "Top Five Most Frequently Admitted Species in 2016", caption = "Figure 6: The top five most frequently admitted species in 2016\n and the total number of clinic admissions for each.")
saveRDS(top5_species16_plot, file = "top5_species16_plot.RDS")
save(top5_species16_plot, file = "top5_species16_plot.RData")
top5_species16_plot

#making top 5 species table with top 5 species, number admissions (count), and percent total admissions
#arrange tibble into descending order
topspecies_2016 <- topspecies_2016 %>%
  arrange(desc(count))

#add percent to tibble
topspecies_2016 <- topspecies_2016 %>%
  mutate(topspecies_2016, 'Percent of Total Admissions' = count / sum (topspecies_2016_count$count) * 100) 
topspecies_2016

#convert top species tibble into data frame
top5_species16_table <- data.frame(topspecies_2016)

#convert admissions 16 data frame into table
setDT(top5_species16_table)

#check if this was sucessful in turning data frame into data table 
is.data.table(top5_species16_table)

#rename columns in data table
setnames(top5_species16_table, c("species","count", "Percent.of.Total.Admissions"), 
         c("Species", "Number of Admissions", "Percent of Total Admissions"))

#round percent column to whole numbers
top5_species16_table$`Percent of Total Admissions` <- round(top5_species16_table$`Percent of Total Admissions`)

#use flextable package
top5_species16_tableflex <- flextable(top5_species16_table)

#format table
#adjust dimensions to autofit
top5_species16_tableflex <- top5_species16_tableflex %>%
  autofit(add_w = 0, add_h = 0)

#format table
top5_species16_tableflex <- theme_vanilla(top5_species16_tableflex)
top5_species16_tableflex <- border_outer(top5_species16_tableflex)
top5_species16_tableflex <- set_caption(top5_species16_tableflex, "Table 6: The number of admissions and percent of total admissions for the top five most frequently admitted species in 2016.")
top5_species16_tableflex

#stats for top species (2016)

#sum of admissions from all top 5 common species combined
sum(top5_species16_table$'Number of Admissions')
#1069 admissions
#1069 of 2797 total admissions in 2016 are from top 5 species,
#top 5 species = 38% of all admissions in 2016

#compare admissions from top 5 species with total number of species admitted in 2016
Number_diffspecies_2016_forstats<- Records_16 %>%
  count(species)
Number_diffspecies_2016_forstats
#272
#272 different species admitted in 2016
#272 species total. Top 5 most commonly admitted species = almost 40% of all admissions

#2018 analysis begins here

#Begin 2018 data analysis
#repeat steps above for 2018, clean, # of intakes, top 5 admission reasons, top 5 species admitted

#extract important columns only
Records_18 <- select(X2018Records, c(1,2,3,8,9))

#clean data 2018
Records_18 <- rename(Records_18, date = "Intake Date", species_class = "Species Class", species = "Species", city = "City Found", cause_for_admission = "Cause for Admission")

#change date format for 2018
Records_18 <- Records_18 %>%
  mutate(date = mdy(date))

#Add month column from date column for 2018
Records_18$month <- month(Records_18$date)

#remove NA for 2018 data
Records_18 <- Records_18 %>%
  filter(!is.na(date))

#plotting the intakes each month against months in the year 2018
Intakebymonth_18 <- ggplot(Records_18) + aes(x = month(month, label = TRUE)) +
  geom_bar(fill = "#0099f9") +
  labs(x = "Month", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5), plot.caption =  element_text(face = "bold", hjust = 0.5, vjust = -0.6)) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title.x = element_text(vjust =-0.55)) +
  theme(axis.title.y = element_text(vjust =0.55)) +
  labs(title = "Monthly Wildlife Admissions in 2018", caption = "Figure 7: The number of monthly admissions to the wildlife clinic in 2018.")
Intakebymonth_18

#2018 intake by month data into table 

#calculate total intakes per month in tibble for 2018 intakes
#monthly total calculations for intakes admission 
Monthlystats_18 <- Records_18 %>%
  group_by(month) %>%
  tally()

#count monthly intakes in 2018 
Monthlystats_18 <- Records_18 %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  na.omit("Monthlystats_18") %>%
  top_n(12)

#statistics of monthly 2018 admissions 
mean(Monthlystats_18$count)
#mean of monthly intakes = 219, meaning the average monthly intake in 2018 is 219 intakes per month
min(Monthlystats_18$count)
#minimum intake per month = 46, meaning the lowest number of monthly intakes in 2018 was 46  intakes in a month
max(Monthlystats_18$count)
#maximum intake per month = 507, meaning the maxmimum number of intakes in a month in 2018 was 507 intakes in a month
median(Monthlystats_18$count)
#median intake per month = 151, meaning of the range for monthly intakes in 2018 the middle number of intakes is 151 intakes in a month
sum(Monthlystats_18$count)
#2626 total intakes in the year

#tibble of monthly intakes, convert numeric month format to abbreviated month 
Monthlystats_18$month <- (month.abb[Monthlystats_18$month])


#convert monthly stats 18 tibble into data frame
Monthlystats_18_table <- data.frame(Monthlystats_18)

#convert monthly stats 18 data frame into table
setDT(Monthlystats_18_table)

#check if this was sucessful in turning data frame into data table 
is.data.table(Monthlystats_18_table)

#rename columns in data table
setnames(Monthlystats_18_table, c("month","count"), c("Month", "Admissions"))

#use flextable package
Monthlystats_18_tableflex <- flextable(Monthlystats_18_table)

#format table
Monthlystats_18_tableflex <- theme_vanilla(Monthlystats_18_tableflex)
Monthlystats_18_tableflex <- border_outer(Monthlystats_18_tableflex)
Monthlystats_18_tableflex <- border_inner_v(Monthlystats_18_tableflex)
Monthlystats_18_tableflex <- set_caption(Monthlystats_18_tableflex, "Table 7: The number of monthly admissions to the wildlife clinic in 2018.")
Monthlystats_18_tableflex

#begin analysis for determining top reasons for admission 2018

#check class for cause for admission column before replacing values
class(Records_18$cause_for_admission)

#replace values to fix spelling and grammar errors
#replace "Orphaned Juvenile" with "Orphaned" to match other variables of the same kind
Records_18$cause_for_admission <- gsub('Orphaned juvenile', 'Orphaned', Records_18$cause_for_admission)

#replace "injury" with "Unknown trauma" to match other variables of the same kind
Records_18$cause_for_admission <- gsub('Injury', 'Unknown trauma', Records_18$cause_for_admission)

#replace "HBC" with "Hit by car" to match other variables of the same kind
Records_18$cause_for_admission <- gsub('HBC', 'Hit by car', Records_18$cause_for_admission)

#replace "Cat caught" with "Cat attack" to match other variables of the same kind
Records_18$cause_for_admission <- gsub('Cat caught', 'Cat attack', Records_18$cause_for_admission)

#top reasons for admission 2018
admissions_2018 <- Records_18 %>%
  group_by(date, cause_for_admission) %>%
  tally()

#count freq of reasons for admission in 2018
admissions_2018 <- na.omit("admissions_2018") 
admissions_2018_count <- Records_18 %>%
  group_by(cause_for_admission) %>%
  summarise(count = n())
#count top 5 common reasons for admission in 2018
admissions_2018 <- na.omit("admissions_2018") 
admissions_2018 <- Records_18 %>%
  group_by(cause_for_admission) %>%
  summarise(count = n()) %>%
  na.omit("admissions_2018") %>%
  top_n(5) 
admissions_2018

#plot top 5 common reasons for admission in 2018
#plot in descending order on x axis
#\n seperates label or title into next line
#added formatting code to axis, title, caption and labels
top5_admissions18_plot <- ggplot(data = admissions_2018, aes(x = reorder(cause_for_admission, -count), y = count)) + 
  geom_bar(stat = "identity", fill = "#69b3a2") +
  labs(x = "Reason for Admission", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5), plot.caption = element_text(face = "bold", hjust = 0.5, vjust = -0.6)) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title.x = element_text(vjust =-0.55)) +
  theme(axis.title.y = element_text(vjust =0.55)) +
  labs(title = "Top Five Reasons for Admission in 2018", caption = "Figure 8: The total number of admissions for each of the \n top five reasons for admission to the wildlife clinic in 2018.")
top5_admissions18_plot


#making the top5reasonsforadmission2018 in a table format
#make table with top 5 reasons, count, and percentage of total 
#arrange data frame into descending order
admissions_2018 <- admissions_2018 %>%
  arrange(desc(count))
#add percent to admissions18 table
admissions_2018 <- admissions_2018 %>%
  mutate(admissions_2018, Percent_total_intakes = count / sum (admissions_2018_count$count) * 100) 
admissions_2018
#convert admissions 18 tibble into data frame
top5_rzn18_table <- data.frame(admissions_2018)

#convert admissions 18 data frame into table
setDT(top5_rzn18_table)

#check if this was sucessful in turning data frame into data table 
is.data.table(top5_rzn18_table)

#rename columns in data table
setnames(top5_rzn18_table, c("cause_for_admission","count", "Percent_total_intakes"), 
         c("Reason Admitted", "Number of Admissions", "Percent of Total Admissions"))

#round percent column to whole numbers
top5_rzn18_table$`Percent of Total Admissions` <- round(top5_rzn18_table$`Percent of Total Admissions`)

#use flextable package
top5_rzn18_tableflex <- flextable(top5_rzn18_table)

#format table
#adjust dimensions to autofit
top5_rzn18_tableflex <- top5_rzn18_tableflex %>%
  autofit(add_w = 0, add_h = 0)

#format table
top5_rzn18_tableflex <- theme_vanilla(top5_rzn18_tableflex)
top5_rzn18_tableflex <- border_outer(top5_rzn18_tableflex)
top5_rzn18_tableflex <- set_caption(top5_rzn18_tableflex, "Table 8: The total number of admissions and percentage of total admissions for each of the top five reasons for admission in 2018.")
top5_rzn18_tableflex
#stats for top reasons for admission data (2018)
sum(top5_rzn18_table$`Number of Admissions`)
#784
#top 5 reasons account for 784  intakes of the 2626 total admissions for the year 2018
#top 5 reasons for intake account for 29% of all intakes in 2018

#begin analysis for top 5 most common species admitted in 2018
#count top 5 most common species admitted in 2018

#top species admitted 2018
topspecies_2018<- Records_18 %>%
  group_by(species) %>%
  tally()

#count freq of all different species admitted in 2018
topspecies_2018 <- na.omit("topspecies_2018") 
topspecies_2018_count <- Records_18 %>%
  group_by(species) %>%
  summarise(count = n())

#count top 5 most common species admitted in 2018
topspecies_2018 <- na.omit("topspecies_2018") 
topspecies_2018 <- Records_18 %>%
  group_by(species) %>%
  summarise(count = n()) %>%
  na.omit("topspecies_2018") %>%
  top_n(5)
topspecies_2018

#plot top 5 species admitted in 2018
#plot in descending order on x axis
#\n seperates label or title into next line
#added formatting code to axis, title, caption and labels

top5_species18_plot <- ggplot(data = topspecies_2018, aes(x = reorder(species, -count), y = count)) + 
  geom_bar(stat = "identity", fill = "#CC3300") +
  labs(x = "Species", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5),  plot.caption = element_text(face = "bold", hjust = 0.5, vjust = -0.8)) +
  theme(axis.text.x = element_text(size = 7, face = "bold", vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title.x = element_text(vjust =-0.55)) +
  theme(axis.title.y = element_text(vjust =0.55)) +
  labs(title = "Top Five Most Frequently Admitted Species in 2018", caption = "Figure 9: The top five most frequently admitted species in 2018\n and the total number of clinic admissions for each.")
saveRDS(top5_species18_plot, file = "top5_species18_plot.RDS")
save(top5_species18_plot, file = "top5_species18_plot.RData")
top5_species18_plot

#making top 5 species table with top 5 species, number admissions (count), and percent total admissions

#arrange tibble into descending order
topspecies_2018 <- topspecies_2018 %>%
  arrange(desc(count))

#add percent to tibble
topspecies_2018 <- topspecies_2018 %>%
  mutate(topspecies_2018, 'Percent of Total Admissions' = count / sum (topspecies_2018_count$count) * 100) 
topspecies_2018

#convert top species tibble into data frame
top5_species18_table <- data.frame(topspecies_2018)

#convert admissions 18 data frame into table
setDT(top5_species18_table)

#check if this was sucessful in turning data frame into data table 
is.data.table(top5_species18_table)

#rename columns in data table
setnames(top5_species18_table, c("species","count", "Percent.of.Total.Admissions"), 
         c("Species", "Number of Admissions", "Percent of Total Admissions"))

#round percent column to whole numbers
top5_species18_table$`Percent of Total Admissions` <- round(top5_species18_table$`Percent of Total Admissions`)

#use flextable package
top5_species18_tableflex <- flextable(top5_species18_table)

#format table
#adjust dimensions to autofit
top5_species18_tableflex <- top5_species18_tableflex %>%
  autofit(add_w = 0, add_h = 0)

#format table
top5_species18_tableflex <- theme_vanilla(top5_species18_tableflex)
top5_species18_tableflex <- border_outer(top5_species18_tableflex)
top5_species18_tableflex <- set_caption(top5_species18_tableflex, "Table 9: The number of admissions and percent of total admissions for the top five most frequently admitted species in 2018.")
top5_species18_tableflex

#stats for top species (2018)
#sum of admissions from all top 5 common species combined
sum(top5_species18_table$'Number of Admissions')
#1186 admissions
#1186 of 2626 total admissions in 2018 are from top 5 species,
#top 5 species = 45% of all admissions in 2018

#compare admissions from top 5 species with total number of species admitted in 2018
Number_diffspecies_2018_forstats<- Records_18 %>%
  count(species)
Number_diffspecies_2018_forstats
#199
#199 different species admitted in 2018
#199 species total.Top 5 most commonly admitted species = almost 45% of all admissions

#Begin 2019 data analysis
#repeat steps above for 2019, clean, # of intakes, top 5 admission reasons, top 5 species admitted

#extract important columns only
Records_19<- select(X2019Records, c(1,2,3,8,9))

#clean data 2019
Records_19 <- rename(Records_19, date = "Intake Date", species_class = "Species Class", species = "Species", city = "City Found", cause_for_admission = "Cause for Admission")

#change date format for 2019
Records_19 <- Records_19 %>%
  mutate(date = mdy(date))

#Add month column from date column for 2019
Records_19$month <- month(Records_19$date)

#remove NA for 2019 data
Records_19 <- Records_19 %>%
  filter(!is.na(date))

#plotting the intakes each month against months in the year 2019
Intakebymonth_19 <- ggplot(Records_19) + aes(x = month(month, label = TRUE)) +
  geom_bar(fill = "#0099f9") +
  labs(x = "Month", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5), plot.caption =  element_text(face = "bold", hjust = 0.5, vjust = -0.6)) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title.x = element_text(vjust =-0.55)) +
  theme(axis.title.y = element_text(vjust =0.55)) +
  labs(title = "Monthly Wildlife Admissions in 2019", caption = "Figure 10: The number of monthly admissions to the wildlife clinic in 2019.")
Intakebymonth_19

#2019 intake by month data into table 

#calculate total intakes per month in tibble for 2019 intakes
#monthly total calculations for intakes admission 
Monthlystats_19 <- Records_19 %>%
  group_by(month) %>%
  tally()

#count monthly intakes in 2019
Monthlystats_19 <- Records_19 %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  na.omit("Monthlystats_19") %>%
  top_n(12)

#statistics of monthly 2019 admissions 

sum(Monthlystats_19$count)
#3014 total intakes in the year

mean(Monthlystats_19$count)
#mean of monthly intakes = 251, meaning the average monthly intake in 2019 is 251 intakes per month

min(Monthlystats_19$count)
#minimum intake per month = 62 , meaning the lowest number of monthly intakes in 2019 was 62  intakes in a month

max(Monthlystats_19$count)
#maximum intake per month = 537, meaning the maximum number of intakes in a month in 2019 was 537 intakes in a month

median(Monthlystats_19$count)
#median intake per month =184, meaning of the range for monthly intakes in 2019 the middle number of intakes is 184 intakes in a month

#tibble of monthly intakes, convert numeric month format to abbreviated month 
Monthlystats_19$month <- (month.abb[Monthlystats_19$month])

#convert monthly stats 19 tibble into data frame
Monthlystats_19_table <- data.frame(Monthlystats_19)

#convert monthly stats 19 data frame into table
setDT(Monthlystats_19_table)

#check if this was successful in turning data frame into data table 
is.data.table(Monthlystats_19_table)

#rename columns in data table
setnames(Monthlystats_19_table, c("month","count"), c("Month", "Admissions"))

#use flextable package
Monthlystats_19_tableflex <- flextable(Monthlystats_19_table)

#format table
Monthlystats_19_tableflex <- theme_vanilla(Monthlystats_19_tableflex)
Monthlystats_19_tableflex <- border_outer(Monthlystats_19_tableflex)
Monthlystats_19_tableflex <- border_inner_v(Monthlystats_19_tableflex)
Monthlystats_19_tableflex <- set_caption(Monthlystats_19_tableflex, "Table 10: The number of monthly admissions to the wildlife clinic in 2019.")
Monthlystats_19_tableflex

#begin analysis for determining top reasons for admission 2019

#check class for cause for admission column before replacing values
class(Records_19$cause_for_admission)

#replace values to fix spelling and grammar errors
#replace "Orphaned Juvenile" with "Orphaned" to match other variables of the same kind
Records_19$cause_for_admission <- gsub('Orphaned juvenile', 'Orphaned', Records_19$cause_for_admission)

#replace "injury" with "Unknown trauma" to match other variables of the same kind
Records_19$cause_for_admission <- gsub('Injury', 'Unknown trauma', Records_19$cause_for_admission)

#replace "HBC" with "Hit by car" to match other variables of the same kind
Records_19$cause_for_admission <- gsub('HBC', 'Hit by car', Records_19$cause_for_admission)

#replace "Cat caught" with "Cat attack" to match other variables of the same kind
Records_19$cause_for_admission <- gsub('Cat caught', 'Cat attack', Records_19$cause_for_admission)

Records_19$cause_for_admission <- gsub('Possible HBC', 'Hit by car', Records_19$cause_for_admission)
Records_19$cause_for_admission <- gsub('Unknown', 'Unknown trauma', Records_19$cause_for_admission)
Records_19$cause_for_admission <- gsub('FX wing', 'Broken wing', Records_19$cause_for_admission)
Records_19$cause_for_admission <- gsub('Back injury', 'Unknown trauma', Records_19$cause_for_admission)

Records_19$cause_for_admission [grepl("Cat", Records_19$cause_for_admission)] <- "Cat attack"
admissions_2019
#top reasons for admission 2019
admissions_2019 <- Records_19 %>%
  group_by(date, cause_for_admission) %>%
  tally()
admissions_2019

#count freq of reasons for admission in 2019
admissions_2019 <- na.omit("admissions_2019") 
admissions_2019_count <- Records_19 %>%
  group_by(cause_for_admission) %>%
  summarise(count = n())

#count top 5 common reasons for admission in 2019
admissions_2019 <- na.omit("admissions_2019") 
admissions_2019 <- Records_18 %>%
  group_by(cause_for_admission) %>%
  summarise(count = n()) %>%
  na.omit("admissions_2019") %>%
  top_n(5) 
admissions_2019

#plot top 5 common reasons for admission in 2019
#plot in descending order on x axis
#\n seperates label or title into next line
#added formatting code to axis, title, caption and labels
top5_admissions19_plot <- ggplot(data = admissions_2019, aes(x = reorder(cause_for_admission, -count), y = count)) + 
  geom_bar(stat = "identity", fill = "#69b3a2") +
  labs(x = "Reason for Admission", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5), plot.caption = element_text(face = "bold", hjust = 0.5, vjust = -0.6)) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title.x = element_text(vjust =-0.55)) +
  theme(axis.title.y = element_text(vjust =0.55)) +
  labs(title = "Top Five Reasons for Admission in 2019", caption = "Figure 11: The total number of admissions for each of the \n top five reasons for admission to the wildlife clinic in 2019.")
top5_admissions19_plot

#making the top5reasonsforadmission2019 in a table format
#make table with top 5 reasons, count, and percentage of total 

#arrange data frame into descending order
admissions_2019 <- admissions_2019 %>%
  arrange(desc(count))

#add percent to admissions19 table
admissions_2019 <- admissions_2019 %>%
  mutate(admissions_2019, Percent_total_intakes = count / sum (admissions_2019_count$count) * 100) 
admissions_2019

#convert admissions 19 tibble into data frame
top5_rzn19_table <- data.frame(admissions_2019)

#convert admissions 19 data frame into table
setDT(top5_rzn19_table)

#check if this was sucessful in turning data frame into data table 
is.data.table(top5_rzn19_table)

#rename columns in data table
setnames(top5_rzn19_table, c("cause_for_admission","count", "Percent_total_intakes"), 
         c("Reason Admitted", "Number of Admissions", "Percent of Total Admissions"))

#round percent column to whole numbers
top5_rzn19_table$`Percent of Total Admissions` <- round(top5_rzn19_table$`Percent of Total Admissions`)

#use flextable package
top5_rzn19_tableflex <- flextable(top5_rzn19_table)

#format table
#adjust dimensions to autofit
top5_rzn19_tableflex <- top5_rzn19_tableflex %>%
  autofit(add_w = 0, add_h = 0)

#format table
top5_rzn19_tableflex <- theme_vanilla(top5_rzn19_tableflex)
top5_rzn19_tableflex <- border_outer(top5_rzn19_tableflex)
top5_rzn19_tableflex <- set_caption(top5_rzn19_tableflex, "Table 11: The total number of admissions and percentage of total admissions for each of the top five reasons for admission in 2019.")
top5_rzn19_tableflex

#stats for top reasons for admission data (2019)
sum(top5_rzn19_table$`Number of Admissions`)
#784
#top 5 reasons account for 784 intakes of the 3014 total admissions for the year 2019
#top 5 reasons for intake account for 26% of all intakes in 2019

#begin analysis for top 5 most common species admitted in 2019
#count top 5 most common species admitted in 2019

#top species admitted 2019
topspecies_2019<- Records_19 %>%
  group_by(species) %>%
  tally()

#count freq of all different species admitted in 2019
topspecies_2019 <- na.omit("topspecies_2019") 
topspecies_2019_count <- Records_19 %>%
  group_by(species) %>%
  summarise(count = n())

#count top 5 most common species admitted in 2019
topspecies_2019 <- na.omit("topspecies_2019") 
topspecies_2019 <- Records_19 %>%
  group_by(species) %>%
  summarise(count = n()) %>%
  na.omit("topspecies_2019") %>%
  top_n(5)
topspecies_2019

#plot top 5 species admitted in 2019
#plot in descending order on x axis
#\n seperates label or title into next line
#added formatting code to axis, title, caption and labels

top5_species19_plot <- ggplot(data = topspecies_2019, aes(x = reorder(species, -count), y = count)) + 
  geom_bar(stat = "identity", fill = "#CC3300") +
  labs(x = "Species", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5),  plot.caption = element_text(face = "bold", hjust = 0.5, vjust = -0.8)) +
  theme(axis.text.x = element_text(size = 7, face = "bold", vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title.x = element_text(vjust =-0.55)) +
  theme(axis.title.y = element_text(vjust =0.55)) +
  labs(title = "Top Five Most Frequently Admitted Species in 2019", caption = "Figure 12: The top five most frequently admitted species in 2019\n and the total number of clinic admissions for each.")
top5_species19_plot

#making top 5 species table with top 5 species, number admissions (count), and percent total admissions

#arrange tibble into descending order
topspecies_2019 <- topspecies_2019 %>%
  arrange(desc(count))

#add percent to tibble
topspecies_2019 <- topspecies_2019 %>%
  mutate(topspecies_2019, 'Percent of Total Admissions' = count / sum (topspecies_2019_count$count) * 100) 
topspecies_2019

#convert top species tibble into data frame
top5_species19_table <- data.frame(topspecies_2019)

#convert admissions 19 data frame into table
setDT(top5_species19_table)

#check if this was successful in turning data frame into data table 
is.data.table(top5_species19_table)

#rename columns in data table
setnames(top5_species19_table, c("species","count", "Percent.of.Total.Admissions"), 
         c("Species", "Number of Admissions", "Percent of Total Admissions"))

#round percent column to whole numbers
top5_species19_table$`Percent of Total Admissions` <- round(top5_species19_table$`Percent of Total Admissions`)

#use flextable package
top5_species19_tableflex <- flextable(top5_species19_table)

#format table
#adjust dimensions to autofit
top5_species19_tableflex <- top5_species19_tableflex %>%
  autofit(add_w = 0, add_h = 0)

#format table
top5_species19_tableflex <- theme_vanilla(top5_species19_tableflex)
top5_species19_tableflex <- border_outer(top5_species19_tableflex)
top5_species19_tableflex <- set_caption(top5_species19_tableflex, "Table 12: The number of admissions and percent of total admissions for the top five most frequently admitted species in 2019.")
top5_species19_tableflex
#stats for top species (2019)

#sum of admissions from all top 5 common species combined
sum(top5_species19_table$'Number of Admissions')
#1400 admissions
#1400 of 3014 total admissions in 2019 are from top 5 species,
#top 5 species = 46% of all admissions in 2019

#compare admissions from top 5 species with total number of species admitted in 2019
Number_diffspecies_2019_forstats<- Records_19 %>%
  count(species)
Number_diffspecies_2019_forstats
#169
#169 different species admitted in 2019
#169 species total. Top 5 most commonly admitted species = almost 50% of all admissions

#start side by side comparison for all four years
#compare intakes monthly, top species, and top reasons for admission between all 4 years
#monthly intakes all years

#rename plots for monthly intakes and remove some aspects of plot
#2019 plot
Intakebymonth_19_grid <- ggplot(Records_19) + aes(x = month(month, label = TRUE)) +
  geom_bar(fill = "#0099f9") +
  labs(x = "Month", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 9, margin = margin(6, 6, 6, 6), hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 6, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(axis.title.x = element_text(size = 6, vjust =-0.55)) +
  theme(axis.title.y = element_text(size = 6, vjust =0.55)) +
  labs(title = "Monthly Wildlife Admissions in 2019")
Intakebymonth_19_grid

#2018 plot
#plotting the intakes each month against months in the year 2018
Intakebymonth_18_grid <- ggplot(Records_18) + aes(x = month(month, label = TRUE)) +
  geom_bar(fill = "#0099f9") +
  labs(x = "Month", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 9,  margin = margin(6, 6, 6, 6), hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 6, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(axis.title.x = element_text(size = 6, vjust =-0.55)) +
  theme(axis.title.y = element_text(size = 6, vjust =0.55)) +
  labs(title = "Monthly Wildlife Admissions in 2018")
Intakebymonth_18_grid

#2016
Intakebymonth_16_grid <- ggplot(Records_16) + aes(x = month(month, label = TRUE)) +
  geom_bar(fill = "#0099f9") +
  labs(x = "Month", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 9, margin = margin(6, 6, 6, 6), hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 6, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(axis.title.x = element_text(size = 6, vjust =-0.55)) +
  theme(axis.title.y = element_text(size = 6, vjust =0.55)) +
  labs(title = "Monthly Wildlife Admissions in 2016")
Intakebymonth_16_grid

#2014
updated_14_intakebymonth_grid <- ggplot(Records_14) + aes(x = month(month, label = TRUE)) + 
  geom_bar(fill = "#0099f9") +
  labs(x = "Month", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 9, margin = margin(6, 6, 6, 6), hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 6, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(axis.title.x = element_text(size = 6, vjust =-0.55)) +
  theme(axis.title.y = element_text(size = 6, vjust =0.55)) +
  labs(title = "Monthly Wildlife Admissions in 2014") 
updated_14_intakebymonth_grid

Monthlyintakes_allyears <- grid.arrange(updated_14_intakebymonth_grid, Intakebymonth_16_grid, Intakebymonth_18_grid, Intakebymonth_19_grid, nrow = 2, ncol = 2, top = "Monthly Admissions by Year")

#top rzns admission all years
Top5rzns_allyears <- grid.arrange(top5_admissions14_plot, top5_admissions16_plot, top5_admissions18_plot, top5_admissions19_plot, nrow = 2, top = "Top Five Reasons for Admission by Year")
#top species all years
Top5species_allyears <- grid.arrange(top5_species14_plot, top5_species16_plot, top5_species18_plot, top5_species19_plot, nrow = 2, top = "Top Five Common Speices by year")

#merging data
merged_intake <- merge(Records_14, Records_16, by=c ("month"))
merged_intake2 <- merge(Records_18, Records_19, by=c ("month"))
merge <- left_join(merged_intake, merged_intake2)

#plotting the intakes each year
Intakebymonth_all <- ggplot(merge) + aes(x = month(month, label = TRUE)) +
  geom_bar(fill = "#0099f9") +
  labs(x = "Month", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5), plot.caption =  element_text(face = "bold", hjust = 0.5, vjust = -0.6)) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title.x = element_text(vjust =-0.55)) +
  theme(axis.title.y = element_text(vjust =0.55)) +
  scale_y_continuous(label = label_number_si()) +
  labs(title = "Combined Monthly Wildlife Admissions, 2014-2019", caption = "Figure 16: The combined total number of admissions
       to the wildlife clinic by month, 2014-2019.")
Intakebymonth_all

#2014-2018 intake by month data 

#calculate total intakes per month in tibble for all years intakes
#monthly total calculations for intakes admission 
Monthlystats_all <- merge %>%
  group_by(month) %>%
  tally()

#count monthly intakes all years
Monthlystats_all <- merge %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  na.omit("Monthlystats_all") %>%
  top_n(12)

#tibble of monthly intakes, convert numeric month format to abbreviated month 
Monthlystats_all$month <- (month.abb[Monthlystats_all$month])

#convert monthly stats tibble into data frame
Monthlystats_all_table <- data.frame(Monthlystats_all)

#convert monthly stats data frame into table
setDT(Monthlystats_all_table)

#check if this was sucessful in turning data frame into data table 
is.data.table(Monthlystats_all_table)

#rename columns in data table
setnames(Monthlystats_all_table, c("month","count"), c("Month", "Admissions"))

#use flextable package
Monthlystats_all_tableflex <- flextable(Monthlystats_all_table)

#format table
Monthlystats_all_tableflex <- theme_vanilla(Monthlystats_all_tableflex)
Monthlystats_all_tableflex <- border_outer(Monthlystats_all_tableflex)
Monthlystats_all_tableflex <- border_inner_v(Monthlystats_all_tableflex)
Monthlystats_all_tableflex <- set_caption(Monthlystats_all_tableflex, "Table 13: The number of monthly admissions to the wildlife clinic by date, 2014-2018.")
Monthlystats_all_tableflex

#statistics of monthly admissions across all years
sum(Monthlystats_all$count)
#804332 total intakes across the years 2014, 2016, 2018, 2019

mean(Monthlystats_all$count)
#mean of monthly intakes = 67027, meaning across these four years of data the wildlife rehab averaged 67027 intakes

min(Monthlystats_all$count)
#minimum intake per month = 1558 , meaning the month with the least intakes per month across all data was December with a combined 1558 intakes from all four years

max(Monthlystats_all$count)
#maximum intake per month = 278006, meaning the month with the most intakes per month across all data was June, with 278006 total intakes per month combined across all four years

median(Monthlystats_all$count)
#median intake per month = 21875, meaning of the range for monthly intakes across all years of data or the middle number of intakes was 21875 intakes in a month

#rbind all records data to make next plots
#remove extra column in Rec14
Records_14 <- select(Records_14, c(1,2,3,5,6,7))

#rbind data
Allyearsdata <- rbind(Records_14, Records_16, Records_18, Records_19)
#Add Year column to all data combined 
Allyearsdata$Year <- as.character(year(Allyearsdata$date))
Allyearsdata <- select(Allyearsdata, c(7))
#count observations in each year
Allyearsdata <- Allyearsdata %>%
  filter(Year != 2000) %>%
  filter(Year != 2001) %>%
  filter(Year != 2013) %>%
  filter(Year != 2015) %>%
  filter(Year != 2017)
Allyearsdata %>%
  count(Year)

Intakebyyear <- ggplot(Allyearsdata) + aes(x = Year) +
  geom_bar(fill = "#0099f9") +
  labs(x = "Year", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5), plot.caption =  element_text(face = "bold", hjust = 0.5, vjust = -0.6)) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title.x = element_text(vjust =-0.55)) +
  theme(axis.title.y = element_text(vjust =0.55)) +
  labs(title = "Yearly Wildlife Admissions, 2014-2019", caption = "Figure 17: The number of yearly admissions 
       to the wildlife clinic from 2014 to 2019.")
Intakebyyear

#all years top reasons
#rbind data
Allyearsdata1 <- rbind(Records_14, Records_16, Records_18, Records_19)

#Add Year column to all data combined 
Allyearsdata1$Year <- as.character(year(Allyearsdata1$date))
#count observations in each year
Allyearsdata1 <- Allyearsdata1 %>%
  filter(Year != 2000) %>%
  filter(Year != 2001) %>%
  filter(Year != 2013) %>%
  filter(Year != 2015) %>%
  filter(Year != 2017)

#top reasons for admission all years
merge_admissions <- Allyearsdata1 %>%
  group_by(cause_for_admission) %>%
  tally()
merge_admissions
#count top 5 common reasons for admission in all years
merge_admissions <- Allyearsdata1 %>%
  group_by(cause_for_admission) %>%
  summarise(count = n()) %>%
  na.omit("merge_admissions") %>%
  top_n(5) 
merge_admissions

#plot top 5 common reasons for admission
#plot in descending order on x axis
#\n seperates label or title into next line
#added formatting code to axis, title, caption and labels
Merge_admissions_plot <- ggplot(data = merge_admissions, aes(x = reorder(cause_for_admission, -count), y = count)) + 
  geom_bar(stat = "identity", fill = "#69b3a2") +
  labs(x = "Reason for Admission", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5), plot.caption = element_text(face = "bold", hjust = 0.5, vjust = -0.6)) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title.x = element_text(vjust =-0.55)) +
  theme(axis.title.y = element_text(vjust =0.55)) +
  labs(title = "Top Five Reasons for Admission, by Date 2014-2019", caption = "Figure 18: The total number of admissions for each of the \n top five reasons for admission to the wildlife clinic, by date from 2014- 2019.")
Merge_admissions_plot

#all years top species
#rbind data
Allyearsdata2 <- rbind(Records_14, Records_16, Records_18, Records_19)
#replace values to fix spelling and grammar errors
#replace "Eastern Grey Squirrel" with "Eastern Gray Squirrel" to match other variables of the same kind
Allyearsdata2$cause_for_admission <- gsub('Eastern Grey Squirrel', 'Eastern Gray Squirrel', Allyearsdata2$cause_for_admission)
Allyearsdata2$cause_for_admission [grepl("Eastern Grey Squirrel", Allyearsdata2$cause_for_admission)] <- "Eastern Gray Squirrel"
#Add Year column to all data combined 
Allyearsdata2$Year <- as.character(year(Allyearsdata2$date))

#count observations in each year
Allyearsdata2 <- Allyearsdata2 %>%
  filter(Year != 2000) %>%
  filter(Year != 2001) %>%
  filter(Year != 2013) %>%
  filter(Year != 2015) %>%
  filter(Year != 2017)

Allyearsdata2$cause_for_admission [grepl("Eastern Grey Squirrel", Allyearsdata2$cause_for_admission)] <- "Eastern Gray Squirrel"

#top species all years
merge_species <- Allyearsdata2 %>%
  group_by(species) %>%
  tally()


#count top 5 common species in all years
merge_species <- Allyearsdata2 %>%
  group_by(species) %>%
  summarise(count = n()) %>%
  na.omit("merge_species") %>%
  top_n(3)

#plot top 5 common reasons for admission in 2019
#plot in descending order on x axis
#\n seperates label or title into next line
#added formatting code to axis, title, caption and labels
merge_species_plot <- ggplot(data = merge_species, aes(x = reorder(species, -count), y = count)) + 
  geom_bar(stat = "identity", fill = "#CC3300") +
  labs(x = "Species", y = "Number of Admissions") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5), plot.caption = element_text(face = "bold", hjust = 0.5, vjust = -0.6)) +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title.x = element_text(vjust =-0.55)) +
  theme(axis.title.y = element_text(vjust =0.55)) +
  labs(title = "Top Three Most Frequently Admitted Species, 2014 - 2019", caption = "Figure 19: The top three most frequently admitted species and the total \n number of clinic admissions for each, 2014- 2019.")
merge_species_plot

#moving on to map of hot spot locations in RI 
#mapping towns with top admission numbers
#all years top towns species admitted from 
#rbind data from all years
Allyearsdata_towns <- rbind(Records_14, Records_16, Records_18, Records_19)
#top towns species admitted from, group by intake city
merge_admissions_towns <- Allyearsdata_towns %>%
  group_by(city) %>%
  tally()

#count top 10 towns in terms of admissions
merge_admissions_towns <- Allyearsdata_towns %>%
  group_by(city) %>%
  summarise(count = n()) %>%
  na.omit("merge_admissions_towns") %>%
  top_n(10)

#mapping
#making the base map 
#map of states
state <- map_data("state")

#subset for RI only 
rhodeisland <- subset(state, region=="rhode island")

#map of counties
counties <- map_data("county")

#subset for counties in RI 
ri_county <- subset(counties, region=="rhode island")

#learning how to use map packages
#trying to map RI 
ri_map <- map_data("state") %>%
  filter(region == "Rhode Island")
#trying to map counties in RI 
ri_mapcounties <- map_data("county") %>%
  filter(region == "rhode island")

#try map out with counties
ri_mapcounties <- ggplot(data = ri_mapcounties,
                         mapping = aes(x = long, y = lat, group=group)) 

state <- map_data("state")
rhodeisland <- subset(state, region=="rhode island")
counties <- map_data("county")
ri_county <- subset(counties, region=="rhode island")

#trial map, not in final report
ri_mapped <- ggplot(data=rhodeisland, mapping=aes(x=long, y=lat, group=group)) +  
  coord_fixed(1.3) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=ri_county, fill=NA, color="white") +
  geom_polygon(color="black", fill=NA) + 
  ggtitle('Rhode Island Map with Counties') + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
ri_mapped

#RI county map 
#not included in final report, trial map 
ri_mapped <- ggplot(data=rhodeisland, mapping=aes(x=long, y=lat, group=group)) +  
  coord_fixed(1.3) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=ri_county, fill=NA, color="white") +
  geom_polygon(color="black", fill=NA) + 
  ggtitle('Rhode Island Map with Counties') + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
ri_mapped

#trial making base map with new imported data set listing lat and long of top ten towns in RI
#not included in final report
ri_mapped3 <- ggplot() +
  geom_polygon(data=rhodeisland,  aes(x=long, y=lat, group = group),color="white", fill="grey92") +  
  geom_point(data=Intakebytown_mappingdata, aes(x=Lon, y=Lat, size = Intakes), color="black") +
  scale_size(name="", range = c(2, 10)) + 
  guides(size=guide_legend("Intakes per top ten towns 2014-2019")) +
  theme_void() 
ri_mapped3

#replace names in data to help with map labels
Intakebytown_mappingdata$Town <- gsub('Coventry, RI, USA', 'Coventry', 
                                      Intakebytown_mappingdata$Town)
Intakebytown_mappingdata$Town <- gsub('Cranston, RI, USA', 'Cranston', 
                                      Intakebytown_mappingdata$Town)
Intakebytown_mappingdata$Town <- gsub('East Greenwich, RI, USA', 'East Greenwich', 
                                      Intakebytown_mappingdata$Town)
Intakebytown_mappingdata$Town <- gsub('Narragansett, RI, USA', 'Narragansett', 
                                      Intakebytown_mappingdata$Town)
Intakebytown_mappingdata$Town <- gsub('Newport, RI, USA', 'Newport', 
                                      Intakebytown_mappingdata$Town)
Intakebytown_mappingdata$Town <- gsub('North Kingstown, RI, USA', 'North Kingstown', 
                                      Intakebytown_mappingdata$Town)
Intakebytown_mappingdata$Town <- gsub('Providence, RI, USA', 'Providence', 
                                      Intakebytown_mappingdata$Town)
Intakebytown_mappingdata$Town <- gsub('Rhode Island, USA', 'Wakefield', 
                                      Intakebytown_mappingdata$Town)
Intakebytown_mappingdata$Town <- gsub('Warwick, RI, USA', 'Warwick', 
                                      Intakebytown_mappingdata$Town)
Intakebytown_mappingdata$Town <- gsub('Westerly, RI, USA', 'Westerly', 
                                      Intakebytown_mappingdata$Town)

#map of top ten towns for admission numbers 2014-2019
ri_mapped5 <- ggplot() +
  geom_polygon(data=rhodeisland,  aes(x=long, y=lat, group = group),color="white", fill="grey92") +  
  coord_fixed(1.3) +
  geom_point(data=Intakebytown_mappingdata, aes(x=Lon, y=Lat, size = Intakes), color="red") +
  scale_size(name="", range = c(2, 8)) + 
  guides(size=guide_legend("Total Admissions")) +
  geom_text_repel(data=Intakebytown_mappingdata, aes(Lon, Lat, label = Town), size=2.5) +
  theme_void() +
  labs(title =  "Rhode Island Towns with Leading Admission Rates, 2014-2019", 
     caption = "Figure 20: The number of admissions from each of the top ten towns
     in Rhode Island with the highest admission rates.") +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(6, 6, 6, 6), hjust = 0.5), 
        plot.caption =  element_text(face = "bold", hjust = 0.5)) 
ri_mapped5



