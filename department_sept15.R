incidents1 <- read.csv("http://ctrp3viz.s3.amazonaws.com/data/Connecticut_r1.csv", stringsAsFactors=FALSE)
incidents2 <- read.csv("http://ctrp3viz.s3.amazonaws.com/data/Connecticut_r2.csv", stringsAsFactors=FALSE)

library(lubridate)
library(dplyr)
library(stringr)
library(DT)
library(tidyr)
library(trendct)
incidents1$RealDate <- as.POSIXct(as.Date(incidents1$InterventionDateTime, origin="1899-12-30"))
incidents1$RealTime <- format(as.POSIXct((incidents1$InterventionTime) * 86400, origin = "1970-01-01"), "%H:%M")
incidents2$RealDate <- mdy_hm(incidents2$InterventionDateTime)

## To compare this set of data with the prior, we must use the same time frame

incidents1a <- subset(incidents1, RealDate >= "2013-10-01" & RealDate <= "2014-03-31")
incidents2a <- incidents2

# Pulling in department staffing

pers <- read.csv("data/police_dept.csv", stringsAsFactors=FALSE)
pers$sworn <- pers$sworn.male+ pers$sworn.female

# OK which departments had the highest rate of pull overs 

incidents_by_dept14 <- data.frame(table(incidents1a$Department.Name))
colnames(incidents_by_dept14) <- c("name", "incidents2014")
incidents_by_dept14$name <- toupper(incidents_by_dept14$name)

incidents_by_dept15 <- data.frame(table(incidents2a$Department.Name))
colnames(incidents_by_dept15) <- c("name", "incidents2015")
incidents_by_dept15$name <- toupper(incidents_by_dept15$name)

incidents_by_dept1415 <- left_join(incidents_by_dept14, incidents_by_dept15)

incidents_by_dept1415 <- left_join(incidents_by_dept1415, pers)


# Which departments had the highest rate of tickets


# Which departments gave the most tickets versus warnings rate

incidents1a$InterventionDispositionCode <- str_trim(incidents1a$InterventionDispositionCode)
incidents2a$InterventionDispositionCode <- str_trim(incidents2a$InterventionDispositionCode)

# Graph 1. Tickets versus warnings
index <- c("I", "M", "N", "U", "V", "W")

values <- c("Ticket", "Ticket", "Ticket", "Ticket", "Warning", "Warning")

# for all
incidents1a$What <- values[match(incidents1a$InterventionDispositionCode, index)]
incidents2a$What <- values[match(incidents2a$InterventionDispositionCode, index)]

violations14 <- data.frame(table(incidents1a$StatutoryReasonForStop, incidents1a$What))
violations15 <- data.frame(table(incidents2a$StatuteReason, incidents2a$What))

colnames(violations14) <- c("violation", "type", "freq")
violations_df14 <- violations14 %>% spread(type, freq)
violations_df14$perc_ticket <- round((violations_df14$Ticket/(violations_df14$Ticket+violations_df14$Warning))*100, digits=2)
violations_df14$perc_warning <- round((violations_df14$Warning/(violations_df14$Ticket+violations_df14$Warning))*100, digits=2)
datatable(violations_df14)

colnames(violations15) <- c("violation", "type", "freq")
violations_df15 <- violations15 %>% spread(type, freq)
violations_df15$perc_ticket <- round((violations_df15$Ticket/(violations_df15$Ticket+violations_df15$Warning))*100, digits=2)
violations_df15$perc_warning <- round((violations_df15$Warning/(violations_df15$Ticket+violations_df15$Warning))*100, digits=2)
datatable(violations_df15)

warnings14 <- data.frame(table(incidents1a$Department.Name, incidents1a$InterventionDispositionCode))
colnames(warnings14) <- c("Department", "Disposition", "Freq")
warnings_df14 <- warnings14 %>% spread(Disposition, Freq)
warnings_df14$total <- warnings_df14$I + warnings_df14$M + warnings_df14$N + warnings_df14$U + warnings_df14$V + warnings_df14$W
warnings_df14$warnings <- warnings_df14$V + warnings_df14$W
warnings_df14$tickets <- warnings_df14$total - warnings_df14$warnings

warnings_df14$perc_warnings <- round((warnings_df14$warnings/warnings_df14$total)*100, digits=2)
warnings_df14$perc_tickets <- 100 - warnings_df14$perc_warnings
warnings_df_table14 <- warnings_df14[c("Department", "total", "tickets", "warnings", "perc_tickets", "perc_warnings")]
write.csv(warnings_df_table14, "department_warnings.csv")
datatable(warnings_df_table14)

warnings15 <- data.frame(table(incidents2a$Department.Name, incidents2a$InterventionDispositionCode))
colnames(warnings15) <- c("Department", "Disposition", "Freq")
warnings_df15 <- warnings15 %>% spread(Disposition, Freq)
warnings_df15$total <- warnings_df15$I + warnings_df15$M + warnings_df15$N + warnings_df15$U + warnings_df15$V + warnings_df15$W
warnings_df15$warnings <- warnings_df15$V + warnings_df15$W
warnings_df15$tickets <- warnings_df15$total - warnings_df15$warnings

warnings_df15$perc_warnings <- round((warnings_df15$warnings/warnings_df15$total)*100, digits=2)
warnings_df15$perc_tickets <- 100 - warnings_df15$perc_warnings
warnings_df_table15 <- warnings_df15[c("Department", "total", "tickets", "warnings", "perc_tickets", "perc_warnings")]
write.csv(warnings_df_table15, "department_warnings.csv")
datatable(warnings_df_table15)

combined_departments <- warnings_df_table14[c("Department", "perc_tickets")]
colnames(combined_departments) <- c("Department","Percent.tickets.2014")
combined_departments2 <- warnings_df_table15[c("Department", "total", "tickets", "warnings", "perc_tickets")]
combined_departments <- left_join(combined_departments2, combined_departments)

combined_departments$diff <- combined_departments$perc_tickets - combined_departments$Percent.tickets.2014
write.csv(combined_departments, "combined_sept15.csv")

# Race

incidents1a$Race <- paste(incidents1a$SubjectRaceCode, incidents1a$SubjectEthnicityCode)
incidents2a$Race <- paste(incidents2a$SubjectRaceCode, incidents2a$SubjectEthnicityCode)

index_race <- c("A H", "A M", "A N", "B H", "B M", "B N", 
                "I H", "I M", "I N", "W H", "W M", "W N")

values_race <- c("Hispanic", "Middle Eastern", "Asian", "Hispanic", "Middle Eastern", "Black", 
                 "Hispanic", "Middle Eastern", "Indian", "Hispanic", "Middle Eastern", "White")

incidents1a$Def_Race <- values_race[match(incidents1a$Race, index_race)]
incidents2a$Def_Race <- values_race[match(incidents2a$Race, index_race)]

race_df14 <- data.frame(table(incidents1a$Def_Race, incidents1a$What))
colnames(race_df14) <- c("Race", "Type", "Freq")
race_df_spread14 <- race_df14 %>% spread(Type, Freq)
race_df_spread14$perc_ticket <- round((race_df_spread14$Ticket/(race_df_spread14$Ticket+race_df_spread14$Warning))*100, digits=2)
race_df_spread14$perc_warning <- round((race_df_spread14$Warning/(race_df_spread14$Ticket+race_df_spread14$Warning))*100, digits=2)

race_df15 <- data.frame(table(incidents2a$Def_Race, incidents2a$What))
colnames(race_df15) <- c("Race", "Type", "Freq")
race_df_spread15 <- race_df15 %>% spread(Type, Freq)
race_df_spread15$perc_ticket <- round((race_df_spread15$Ticket/(race_df_spread15$Ticket+race_df_spread15$Warning))*100, digits=2)
race_df_spread15$perc_warning <- round((race_df_spread15$Warning/(race_df_spread15$Ticket+race_df_spread15$Warning))*100, digits=2)

race_df15a <- data.frame(table(incidents2a$Department.Name, incidents2a$Def_Race))

colnames(race_df15a) <- c("Department", "Race", "Stops")
race_df_spread15a <- race_df15a %>% spread(Race, Stops)
colnames(race_df_spread15a) <- c("Department", "Asian", "Black", "Hispanic", "Indian", "Middle.Eastern", "White")

race_df_spread15a$Asian.Percent <- round((race_df_spread15a$Asian/(
  race_df_spread15a$Asian+race_df_spread15a$Black+race_df_spread15a$Hispanic+
    race_df_spread15a$Indian+race_df_spread15a$Middle.Eastern+race_df_spread15a$White))*100,2)

race_df_spread15a$Black.Percent <- round((race_df_spread15a$Black/(
  race_df_spread15a$Asian+race_df_spread15a$Black+race_df_spread15a$Hispanic+
    race_df_spread15a$Indian+race_df_spread15a$Middle.Eastern+race_df_spread15a$White))*100,2)

race_df_spread15a$Hispanic.Percent <- round((race_df_spread15a$Hispanic/(
  race_df_spread15a$Asian+race_df_spread15a$Black+race_df_spread15a$Hispanic+
    race_df_spread15a$Indian+race_df_spread15a$Middle.Eastern+race_df_spread15a$White))*100,2)

race_df_spread15a$Indian.Percent <- round((race_df_spread15a$Indian/(
  race_df_spread15a$Asian+race_df_spread15a$Black+race_df_spread15a$Hispanic+
    race_df_spread15a$Indian+race_df_spread15a$Middle.Eastern+race_df_spread15a$White))*100,2)

race_df_spread15a$Middle.Eastern.Percent <- round((race_df_spread15a$Middle.Eastern/(
  race_df_spread15a$Asian+race_df_spread15a$Black+race_df_spread15a$Hispanic+
    race_df_spread15a$Indian+race_df_spread15a$Middle.Eastern+race_df_spread15a$White))*100,2)

race_df_spread15a$White.Percent <- round((race_df_spread15a$White/(
  race_df_spread15a$Asian+race_df_spread15a$Black+race_df_spread15a$Hispanic+
    race_df_spread15a$Indian+race_df_spread15a$Middle.Eastern+race_df_spread15a$White))*100,2)

dept_race15 <- race_df_spread15a[c("Department", "Asian.Percent", "Black.Percent", 
                                   "Hispanic.Percent", "Indian.Percent", "Middle.Eastern.Percent",
                                   "White.Percent")]

dept_race15$Asian.Percent <- paste0(dept_race15$Asian.Percent,"%")
dept_race15$Black.Percent <- paste0(dept_race15$Black.Percent,"%")
dept_race15$Hispanic.Percent <- paste0(dept_race15$Hispanic.Percent,"%")
dept_race15$Indian.Percent <- paste0(dept_race15$Indian.Percent,"%")
dept_race15$Middle.Eastern.Percent <- paste0(dept_race15$Middle.Eastern.Percent,"%")
dept_race15$White.Percent <- paste0(dept_race15$White.Percent,"%")

colnames(dept_race15) <- c("Department", "Asian", "Black", "Hispanic", "Indian", "Middle Eastern", "White")

fancytable(dept_race15, headline = "Traffic stops by race by department", subhead = "Between October 2014 and March 2015", height = 400,
           paging = "false", sourceline = "CT Data Collaborative, CCSU Institute for Municipal Regional Policy", byline = "TrendCT.org", col = 0,
           desc_asc = "desc")
