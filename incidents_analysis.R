library(lubridate)
library(ggplot2)
library(stringr)
library(dplyr)
incidents_all <- read.csv("http://ctrp3viz.s3.amazonaws.com/data/Connecticut_r1.csv", stringsAsFactors=FALSE)

#incidents <- incidents_all[c("Department.Name", "ReportingOfficerIdentificationID", 
                             "Day.of.Week", "SubjectRaceCode", "SubjectEthnicityCode",
                             "SubjectSexCode", "SubjectAge", "ResidentIndicator", 
                             "TownRecidentIndicator", "InterventionLocationName",
                             "InterventionReasonCode", "InterventionTechniqueCode",
                             "TowedIndicator", "StatuteCodeIdentificationID",
                             "StatutoryReasonForStop", "StatutatoryCitationPostStop",
                             "VehicleSearchedIndicator", "VehicleSearchedIndicator",
                             "SearchAuthorizationCode", "ContrabandIndicator",
                             "CustodialArrestIndicator", "InterventionDispositionReasonText",
                             "InterventionDateTime", "InterventionTime", "CreatedDate")]

incidents <- read.csv("data/incidents_slim.csv", stringsAsFactors=FALSE)

# Fix times
incidents$RealDate <- as.POSIXct(as.Date(incidents$InterventionDateTime, origin="1899-12-30"))
incidents$RealTime <- format(as.POSIXct((incidents$InterventionTime) * 86400, origin = "1899-12-30", tz="America/Montserrat"), "%H:%M")
incidents$Hour <- hour(incidents$RealDate)


incidents$Day.of.Week <- as.factor(incidents$Day.of.Week)
levels(incidents$Day.of.Week) <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# Histogram time
c <- ggplot(incidents, aes(x=Hour))
c <- c + geom_histogram(colour="darkred",fill="white", binwidth=1)
c

#Let's break it out by day
c <- ggplot(incidents, aes(x=Hour))
c <- c + geom_histogram(colour="darkred",fill="white", binwidth=1)
c + facet_grid(Day.of.Week ~ .)


# Let's look at rate per department employment
pers <- read.csv("data/police_dept.csv", stringsAsFactors=FALSE)
pers$sworn <- pers$sworn.male+ pers$sworn.female
incidents_by_dept <- data.frame(table(incidents$Department.Name))
colnames(incidents_by_dept) <- c("name", "incidents")
incidents_by_dept$name <- toupper(incidents_by_dept$name)

incidents_by_dept <- left_join(incidents_by_dept, pers)

incidents_by_dept$inc.rate <- round((incidents_by_dept$incidents/incidents_by_dept$sworn), digits=2)

reordered <- incidents_by_dept[order(incidents_by_dept$inc.rate),]

#Most popular day of the month?
incidents$day_num <- day(incidents$RealDate)
c <- ggplot(incidents, aes(x=day_num))
c <- c + geom_histogram(colour="darkred",fill="white", binwidth=1)
c

#Most popular day of week?
c <- ggplot(incidents, aes(x=Day.of.Week))
c <- c + geom_histogram(colour="darkred",fill="white", binwidth=1)
c

#Most popular ticket type

c <- ggplot(incidents, aes(x=StatutoryReasonForStop))
c <- c + geom_histogram(colour="darkred",fill="white", binwidth=1)
c

# looking at infraction fees
fees <- read.csv("data/infractions.csv", stringsAsFactors=FALSE)

# all infractions
ifr_list <- data.frame(table(incidents$StatuteCodeIdentificationID))
ifr_list2 <- data.frame(table(incidents$StatutatoryCitationPostStop))

# Clean up infractions
incidents$final_infr <- "TBD"
incidents$final_infr2 <- "TBD"

inc_list <- 1:nrow(incidents)

for (i in inc_list) {
  ifelse(is.na(incidents$StatutatoryCitationPostStop[i]),
    incidents$final_infr[i] <- incidents$StatuteCodeIdentificationID[i],
    incidents$final_infr[i] <- incidents$StatutatoryCitationPostStop[i])
}
for (i in inc_list) {
  ifelse(nchar(incidents$final_infr[i] > 3),
         incidents$final_infr2[i] <- incidents$final_infr[i],
         incidents$final_infr2[i] <- incidents$StatuteCodeIdentificationID[i])
}


inc_filt <- incidents_all[c("InterventionIdentificationID")]
incidents <- cbind(incidents, inc_filt)
incidents$final_infr2 <- str_to_upper(incidents$final_infr2)
#inc_list_slim <- incidents[c("InterventionIdentificationID", "final_infr2")]
#write.csv(inc_list_slim, "slim_list2.csv")

dash <- subset(incidents, final_infr2=="-")

# Turns out Ridgefield is the worst offender for putting "-" in the 
# statute category with 732! That's 1/10 out of all their tickets.
# Dig a little more and you can see which officer's the most lax in details.
# Well, there's about 900 to clean up out of the dashes.

dash_list <- 1:nrow(dash)
for (i in dash_list) {
  if(grepl("-", dash$final_infr2[i])) {
    dash$final_infr2[i] <- dash$StatuteCodeIdentificationID[i]
  }
}
for (i in dash_list) {
  if(grepl("^-", dash$final_infr2[i])) {
    dash$final_infr2[i] <- "unlisted"
  }
}

incidents <- subset(incidents, final_infr2!="-")
incidents <- rbind(incidents, dash)

# ok, lets look at the blanks
blanks <- subset(incidents, final_infr2=="")

# there are 2542 blanks
# Worst offender is probably Naugutuck with 580 and then Milford 328 and Fairfield 256

blanks$final_infr2 <- blanks$StatuteCodeIdentificationID
blanks_list <- 1:nrow(blanks)
for (i in blanks_list) {
  if(nchar(blanks$final_infr2[i]) < 1) {
    blanks$final_infr2[i] <- blanks$StatutoryReasonForStop[i]
  }
}

incidents <- subset(incidents, final_infr2!="")
incidents <- rbind(incidents, blanks)

#Focus on the N/As

nopes <- subset(incidents, final_infr2=="N/A")

#There are 4386 nopes. Sigh

# Wallingford. So many N/As. 3,336. Then West Haven with 603.
nopes$final_infr2 <- nopes$StatuteCodeIdentificationID

nopes$final_infr2<- gsub(" ", "", nopes$final_infr2)

nopes_list <- 1:nrow(nopes)
for (i in nopes_list) {
  if(nchar(nopes$final_infr2[i]) < 4) {
    nopes$final_infr2[i] <- nopes$StatutoryReasonForStop[i]
  }
}

incidents <- subset(incidents, final_infr2!="N/A")
incidents <- rbind(incidents, nopes)

# Moving on to the Sames

sames <- subset(incidents, final_infr2=="SAME")

# Only 647. Ok. State Police and West Haven are the worst at this

sames$final_infr2 <- sames$StatuteCodeIdentificationID
incidents <- subset(incidents, final_infr2!="SAME")
incidents <- rbind(incidents, sames)

# NAs
nope2 <- subset(incidents, final_infr2=="NA")
nope2$final_infr2 <- nope2$StatuteCodeIdentificationID

nope2_list <- 1:nrow(nope2)
for (i in nope2_list) {
  if(nchar(nope2$final_infr2[i]) < 3) {
    nope2$final_infr2[i] <- nope2$StatutoryReasonForStop[i]
  }
}

incidents <- subset(incidents, final_infr2!="NA")
incidents <- rbind(incidents, nope2)


# Slash
slash <- subset(incidents, final_infr2=="/")
slash$final_infr2 <- slash$StatuteCodeIdentificationID

slash_list <- 1:nrow(slash)
for (i in slash_list) {
  if(nchar(slash$final_infr2[i]) < 4) {
    slash$final_infr2[i] <- slash$StatutoryReasonForStop[i]
  }
}
incidents <- subset(incidents, final_infr2!="/")
incidents <- rbind(incidents, slash)

# nothing?

stat_list <- data.frame(table(incidents$final_infr2))
colnames(stat_list) <- c("NAME", "Tickets")
stat_list$NAME <- as.character(stat_list$NAME)
join1 <- left_join(stat_list, fees)
join2 <- right_join(stat_list, fees)

#write.csv(join1,"join1.csv")
#140 out of 4400 matched. sigh

#write.csv(join2,"join2.csv")
#145 out of 1246 statutes. goddammit

cleaned <- read.csv("data/cleaned-matched-infractions.csv", stringsAsFactors=FALSE)

fees_totals <- fees[c("NAME","TOTAL", "CATEGORY", "DESCRIPTION")]

fees_updated <- left_join(cleaned, fees_totals)
fees_updated$final_infr2 <- fees_updated$NAME2

incidents_up <- left_join(incidents, fees_updated)

# Well, that's gross. only 100k out of 500k matched. Moving on.

# OK, let's just look at warnings...
list_this <- data.frame(table(incidents$InterventionDispositionReasonText))
warnings <- subset(incidents, InterventionDispositionReasonText=="Verbal Warning" | InterventionDispositionReasonText=="Warning Given")

warnings$SubjectAge <- as.numeric(warnings$SubjectAge)
mean(warnings$SubjectAge)
incidents$SubjectAge <- as.numeric(incidents$SubjectAge)
mean(incidents$SubjectAge, na.rm=TRUE)

# OK, age of driver doesn't mean shit
all_gender <- data.frame(table(incidents$SubjectSexCode))
warning_gender <- data.frame(table(warnings$SubjectSexCode))

all_gender$perc <- round((all_gender$Freq/sum(all_gender$Freq))*100, digits=2)
warning_gender$perc <- round((warning_gender$Freq/sum(warning_gender$Freq))*100, digits=2)

#Gender of driver doesn't mean anything, either

# Male vs female and race all?

mfrace_all <- data.frame(table(incidents$SubjectSexCode, incidents$SubjectRaceCode))
library(tidyr)
mfrace_all <- spread(mfrace_all, Var1, Freq)
mfrace_all$perc_f <- round((mfrace_all$F/(mfrace_all$M+mfrace_all$F))*100, digits=2)
mfrace_all$perc_m <- round((mfrace_all$M/(mfrace_all$M+mfrace_all$F))*100, digits=2)

mfrace_all$perc_f_all <- round((mfrace_all$F/sum(mfrace_all$F))*100, digits=2)
mfrace_all$perc_m_all <- round((mfrace_all$M/sum(mfrace_all$M))*100, digits=2)

# Male vs female and race warnings?

warnings_all <- data.frame(table(warnings$SubjectSexCode, warnings$SubjectRaceCode))
library(tidyr)
warnings_all <- spread(warnings_all, Var1, Freq)
warnings_all$perc_f <- round((warnings_all$F/(warnings_all$M+warnings_all$F))*100, digits=2)
warnings_all$perc_m <- round((warnings_all$M/(warnings_all$M+warnings_all$F))*100, digits=2)

warnings_all$perc_f_all <- round((warnings_all$F/sum(warnings_all$F))*100, digits=2)
warnings_all$perc_m_all <- round((warnings_all$M/sum(warnings_all$M))*100, digits=2)

# looking at another column for warnings
#warned_extra <- incidents[grepl("WARN",incidents$final_infr2),]

warned_extra <- incidents[grepl("WARN",incidents$StatuteCodeIdentificationID),]
warned_extra2 <- incidents[grepl("WARN",incidents$StatutatoryCitationPostStop),]

warned_extra3 <- incidents[grepl("Warn",incidents$StatutatoryCitationPostStop),]
warned_extra3b <- incidents[grepl("Warn",incidents$StatuteCodeIdentificationID),]

warned_extra4 <- incidents[grepl("warn",incidents$StatutatoryCitationPostStop),]
warned_extra4b <- incidents[grepl("warn",incidents$StatuteCodeIdentificationID),]

warning_extra5 <- incidents[grepl("Warn",incidents$InterventionDispositionReasonText),]
warning_extra5 <- incidents[grepl("Warn",incidents$InterventionDispositionReasonText),]

# Hm, only 18 departments. Let's take a look at them as a whole

the18 <- subset(incidents, Department.Name=="Brookfield" | Department.Name=="CAPITOL POLICE" | 
                Department.Name=="Coventry" | Department.Name=="Derby" | Department.Name == "East Hampton" |
                Department.Name=="Easton" | Department.Name=="Middlebury" | Department.Name == "Naugatuck" |
                Department.Name=="Plymouth" | Department.Name=="Redding" | Department.Name == "SCSU" |
                Department.Name=="Seymour" | Department.Name=="Suffield" | Department.Name == "Thomaston" |
                Department.Name=="Weston" | Department.Name=="Windsor Locks" | Department.Name == "Winsted" |
                Department.Name=="Wolcott")

the18_non <- the18[!grepl("Warn",the18$InterventionDispositionReasonText),]

the18_inc <- data.frame(table(the18$StatutoryReasonForStop))
colnames(the18_inc) <- c("citation", "total")

the18_warn <- data.frame(table(warning_extra5$StatutoryReasonForStop))
colnames(the18_warn) <- c("citation", "warnings")
the18_df <- left_join(the18_inc, the18_warn)

the18_df$percent_warning <- round((the18_df$warnings/the18_df$total)*100, digits=2)

# Interesting. Ok let's look at age

warning_extra5$SubjectAge <- as.numeric(warning_extra5$SubjectAge)
mean(warning_extra5$SubjectAge)
the18_non$SubjectAge <- as.numeric(the18_non$SubjectAge)
mean(the18_non$SubjectAge, na.rm=TRUE)

median(warning_extra5$SubjectAge)
median(the18_non$SubjectAge, na.rm=TRUE)

# Oh God. The data on age is so messed up. Have to take out the weird ones

warning_extra5b <- subset(warning_extra5, SubjectAge > 0 & SubjectAge < 100)

# That takes out about 600 from warnings
mean(warning_extra5b$SubjectAge)

the18_nonb <- subset(the18_non, SubjectAge > 0 & SubjectAge < 100)
mean(the18_nonb$SubjectAge, na.rm=TRUE)

# That takes out about 60 for ticketed

# Plot it out
c <- ggplot(warning_extra5b, aes(x=SubjectAge))
c <- c + geom_histogram(colour="darkred",fill="white", binwidth=1)
c

c <- ggplot(the18_nonb, aes(x=SubjectAge))
c <- c + geom_histogram(colour="darkred",fill="white", binwidth=1)
c

# Does age change between men and women?

# All tickets
gender18 <- data.frame(table(the18_nonb$SubjectAge,the18_nonb$SubjectSexCode))
colnames(gender18) <- c("age", "gender", "tickets")
ggplot(gender18, aes(age, y=tickets, group=gender, colour=gender)) +
  geom_line() +
  geom_point()

ggplot(gender18, aes(age, tickets, group=gender, fill=gender)) + geom_area(position="fill")

# Warnings
genderw <- data.frame(table(warning_extra5b$SubjectAge,warning_extra5b$SubjectSexCode))
colnames(genderw) <- c("age", "gender", "tickets")
ggplot(genderw, aes(age, y=tickets, group=gender, colour=gender)) +
  geom_line() +
  geom_point()

ggplot(genderw, aes(age, tickets, group=gender, fill=gender)) + geom_area(position="fill")

# OK, age of driver doesn't mean much
all_gender <- data.frame(table(the18_non$SubjectSexCode))
warning_gender <- data.frame(table(warning_extra5$SubjectSexCode))

all_gender$perc <- round((all_gender$Freq/sum(all_gender$Freq))*100, digits=2)
warning_gender$perc <- round((warning_gender$Freq/sum(warning_gender$Freq))*100, digits=2)


# What about race? Have to refer to the data dictionary here...

all_race$perc <- round((all_race$Freq/sum(all_race$Freq))*100, digits=2)
the18$Race <- paste(the18$SubjectRaceCode, the18$SubjectEthnicityCode)

index <- c("A H", "A M", "A N", "B H", "B M", "B N", 
           "I H", "I M", "I N", "W H", "W M", "W N")


values <- c("Hispanic", "Middle Eastern", "Asian", "Hispanic", "Middle Eastern", "Black", 
            "Hispanic", "Middle Eastern", "Indian", "Hispanic", "Middle Eastern", "White")

# for all
the18$Def_Race <- values[match(the18$Race, index)]
all_race <- data.frame(table(the18$Def_Race))
colnames(all_race) <- c("race", "incidents")
all_race$percent_incidents <- (all_race$incidents/sum(all_race$incidents))*100
all_race$percent_incidents <- round(all_race$percent_incidents, digits=2)

# for all minus warnings
the18_non$Race <- paste(the18_non$SubjectRaceCode, the18_non$SubjectEthnicityCode)
the18_non$Def_Race <- values[match(the18_non$Race, index)]
all_race_non <- data.frame(table(the18_non$Def_Race))
colnames(all_race_non) <- c("race", "tickets")
all_race_non$percent_tickets <- (all_race_non$tickets/sum(all_race_non$tickets))*100
all_race_non$percent_tickets <- round(all_race_non$percent_tickets, digits=2)

# now just for those who got warnings
warning_extra5$Race <- paste(warning_extra5$SubjectRaceCode, warning_extra5$SubjectEthnicityCode)
warning_extra5$Def_Race <- values[match(warning_extra5$Race, index)]
all_race_warn <- data.frame(table(warning_extra5$Def_Race))
colnames(all_race_warn) <- c("race", "warnings")
all_race_warn$percent_warnings <- (all_race_warn$warnings/sum(all_race_warn$warnings))*100
all_race_warn$percent_warnings <- round(all_race_warn$percent_warnings, digits=2)

all <- left_join(all_race, all_race_non)
all <- left_join(all, all_race_warn)
kagle(all)


#Let's see which day of the year had the most tickets

incidents$calendar <- floor_date(incidents$RealDate, "day")

cal_days <- data.frame(table(incidents$calendar))

incidents$cal_month <- floor_date(incidents$RealDate, "month")

cal_months <- data.frame(table(incidents$cal_month))

incidents$cal_week <- floor_date(incidents$RealDate, "week")
cal_week <- data.frame(table(incidents$cal_week))

incidents$cal_days <- days_in_month(incidents$RealDate)
incidents$day_num <- day(incidents$RealDate)

day_num <- data.frame(table(incidents$day_num))


longmonth <- subset(incidents, cal_days==31)
midmonth <- subset(incidents, cal_days==30)
shortmonth <- subset(incidents, cal_days==28)


##31 DAY MONTH ANALYSIS HERE
longmonth_end <- subset(longmonth, (day_num==25 | day_num==26 | day_num==27 | 
                                      day_num==28 | day_num==29 | day_num==30 | day_num==31))

last_seven_long <- nrow(longmonth_end)/7

longmonth_start <- subset(longmonth, (day_num==1 | day_num==2 | day_num==3 | 
                                      day_num==4 | day_num==5 | day_num==6 | day_num==7))

first_seven_long <- nrow(longmonth_start)/7

longmonth_mid <- subset(longmonth, (day_num!=1 & day_num!=2 & day_num!=3 & 
                                        day_num!=4 & day_num!=5 & day_num!=6 & day_num!=7 &
                                      day_num!=25 & day_num!=26 & day_num!=27 & 
                                      day_num!=28 & day_num!=29 & day_num!=30 & day_num!=31))

mid_seven_long <- nrow(longmonth_mid)/17

##30 DAY MONTH ANALYSIS HERE
midmonth_end <- subset(midmonth, (day_num==24 | day_num==25 | day_num==26 | 
                                      day_num==27 | day_num==28 | day_num==29 | day_num==30))

last_seven_mid <- nrow(midmonth_end)/7

midmonth_start <- subset(midmonth, (day_num==1 | day_num==2 | day_num==3 | 
                                        day_num==4 | day_num==5 | day_num==6 | day_num==7))

first_seven_mid <- nrow(midmonth_start)/7

midmonth_mid <- subset(midmonth, (day_num!=1 & day_num!=2 & day_num!=3 & 
                                      day_num!=4 & day_num!=5 & day_num!=6 & day_num!=7 &
                                      day_num!=24 & day_num!=25 & day_num!=26 & 
                                      day_num!=27 & day_num!=28 & day_num!=29 & day_num!=30))

mid_seven_mid <- nrow(midmonth_mid)/16

##FEBRUARY ANALYSIS HERE
shortmonth_end <- subset(shortmonth, (day_num==22 | day_num==23 | day_num==24 | 
                                    day_num==25 | day_num==26 | day_num==27 | day_num==28))

last_seven_short <- nrow(shortmonth_end)/7

shortmonth_start <- subset(shortmonth, (day_num==1 | day_num==2 | day_num==3 | 
                                      day_num==4 | day_num==5 | day_num==6 | day_num==7))

first_seven_short <- nrow(shortmonth_start)/7

shortmonth_mid <- subset(shortmonth, (day_num!=1 & day_num!=2 & day_num!=3 & 
                                    day_num!=4 & day_num!=5 & day_num!=6 & day_num!=7 &
                                    day_num!=22 & day_num!=23 & day_num!=24 & 
                                    day_num!=25 & day_num!=26 & day_num!=27 & day_num!=28))

mid_seven_short <- nrow(shortmonth_mid)/14

avg_last <- (last_seven_short+last_seven_mid+last_seven_long)/3
avg_mid <- (mid_seven_short+mid_seven_mid+mid_seven_long)/3
avg_start<- (first_seven_short+first_seven_mid+first_seven_long)/3


# Most popular and second-most popular type of charge for top 10 charges?

first <-subset(incidents, calendar=="2014-08-29")
sort(table(first$StatutoryReasonForStop), decreasing=TRUE)

second <-subset(incidents, calendar=="2014-05-23")
sort(table(second$StatutoryReasonForStop), decreasing=TRUE)

third <-subset(incidents, calendar=="2014-09-12")
sort(table(third$StatutoryReasonForStop), decreasing=TRUE)

fourth <-subset(incidents, calendar=="2014-09-04")
sort(table(fourth$StatutoryReasonForStop), decreasing=TRUE)

fifth <-subset(incidents, calendar=="2014-07-03")
sort(table(fifth$StatutoryReasonForStop), decreasing=TRUE)

sixth <-subset(incidents, calendar=="2014-05-20")
sort(table(sixth$StatutoryReasonForStop), decreasing=TRUE)

seventh <-subset(incidents, calendar=="2014-05-24")
sort(table(seventh$StatutoryReasonForStop), decreasing=TRUE)

eighth<-subset(incidents, calendar=="2014-09-05")
sort(table(eighth$StatutoryReasonForStop), decreasing=TRUE)

ninth<-subset(incidents, calendar=="2014-05-21")
sort(table(ninth$StatutoryReasonForStop), decreasing=TRUE)

tenth<-subset(incidents, calendar=="2014-09-18")
sort(table(tenth$StatutoryReasonForStop), decreasing=TRUE)

# OK, just looking at the most-common types of tickets

ticket_types <- data.frame(sort(table(incidents$StatutoryReasonForStop), decreasing=TRUE))

# Hm, who gives the most speed-related tickets?

speeding <- subset(incidents, StatutoryReasonForStop=="Speed Related")
top_dept_speeding <- data.frame(sort(table(speeding$Department.Name), decreasing=TRUE))

# Ok, who gives out "other"?
other <- subset(incidents, StatutoryReasonForStop=="Other")
top_dept_other <- data.frame(sort(table(other$Department.Name), decreasing=TRUE))

# Who is the most strict with cell phones?

cell <- subset(incidents, StatutoryReasonForStop=="Cell Phone")
top_dept_cell <- data.frame(sort(table(cell$Department.Name), decreasing=TRUE))


# What's up with the special statute?

special_statute <- subset(incidents, final_infr2=="14-298")
44934/595951
44934/594970

160621/595951
8 percent of all tickets
48 percent of all s

Violation of exhaust emission standards/periodic inspection requirements (1st offense)
Operating motor vehicle other than motorcycle WITHOUT A LICENSE (1st offense)
