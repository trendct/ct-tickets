traffic <- read.csv("data/traffic_count.csv", stringsAsFactors=FALSE)

traffic_towns <- tapply(traffic$ADT, traffic$Town_Name, mean)

towns <- data.frame(traffic_towns)

towns$town <- row.names(towns)
towns <- towns[c("town", "traffic_towns")]
colnames(towns) <- c("town", "ADT Average")
roads <- data.frame(table(traffic$Town_Name))
colnames(roads) <- c("town", "roads")

library(stringr)
library(dplyr)
library(ctnamecleaner)
library(tidyr)

towns <- left_join(towns, roads)
colnames(towns) <- c("town", "ADT", "roads")
towns$ADT <- round(towns$ADT, digits=2)
types <- data.frame(table(traffic$Town_Name, traffic$FClass))
colnames(types) <- c("town", "ftype", "number")
types$ftype <- as.character(types$ftype)
types$ftype <- gsub("1", "f1", types$ftype)
types$ftype <- gsub("2", "f2", types$ftype)
types$ftype <- gsub("3", "f3", types$ftype)
types$ftype <- gsub("4", "f4", types$ftype)
types$ftype <- gsub("5", "f5", types$ftype)
types$ftype <- gsub("6", "f6", types$ftype)
types$ftype <- gsub("7", "f7", types$ftype)
types <- spread(types, ftype, number)

towns <- left_join(towns, types)
towns$town <- str_trim(towns$town)
dmvt <- read.csv("data/dmvt.csv", stringsAsFactors=FALSE)
dmvt$town <- str_to_title(dmvt$Town)
dmvt$DVMT <- gsub(",", "", dmvt$DVMT)
dmvt$DVMT <- as.numeric(dmvt$DVMT)
dmvt <- dmvt[,-1]
dmvt <- dmvt[,-1]

towns <- left_join(towns, dmvt)

area <- read.csv("data/area.csv", stringsAsFactors=FALSE)
colnames(area) <- c("town", "sqmiles")
area$town <- str_trim(area$town)
towns <- left_join(towns, area)
towns <- ctpopulator(town, towns)
towns$density <- towns$pop2013/towns$sqmiles

sales <- read.csv("data/salestax.csv", stringsAsFactors=FALSE)
salestx <- subset(sales, NAICS.Industry.Code=="All NAICS Codes")
sales2014 <- subset(salestx, Calendar.Year==2014)
retail <- tapply(sales2014$Retail.Sales.of.Goods, sales2014$Municipality, sum)
write.csv(retail, "data/temp_retail.csv")
retail <- read.csv("data/temp_retail.csv", stringsAsFactors=FALSE)
file.remove("data/temp_retail.csv")
colnames(retail) <- c("town", "retail.tax")
retail <- retail[-1,]
retail$town <- str_trim(retail$town)
towns <- left_join(towns, retail)


retail$town <- rownames(retail)
colnames(retail) <- c("retail.sales", "town")
retail <- retail[c("town", "retail.sales")]
retail$town <- str_trim(retail$town)
towns2 <- left_join(retail, towns)


taxes <- tapply(sales2014$Total.Tax.Due.at.6.35., sales2014$Municipality, sum)
write.csv(taxes, "data/temp_taxes.csv")
taxes <- read.csv("data/temp_taxes.csv", stringsAsFactors=FALSE)
file.remove("data/temp_taxes.csv")
colnames(taxes) <- c("town", "total.tax")
taxes <- taxes[-1,]
taxes$town <- str_trim(taxes$town)

towns <- left_join(towns, taxes)

taxes <- taxes[c("town", "total.tax")]

commuters <- read.csv("data/commuters.csv", stringsAsFactors=FALSE)

taxes <- data.frame(taxes)
taxes$town <- rownames(taxes)
colnames(taxes) <- c("total.tax", "town")


commuters_filtered <- commuters[c("name", "B08006001...Total.", "B08006002...Car..truck..or.van.")]
commuters_filtered <- commuters_filtered[-1,]
commuters_filtered <- commuters_filtered[-1,]

colnames(commuters_filtered) <- c("town","commute_total", "drivers")
commuters_filtered$town <- sub(" town,.*", "", commuters_filtered$town)
commuters_filtered$percent.drivers <- round((commuters_filtered$drivers/commuters_filtered$commute_total)*100, digits=2)
commuters_filtered$town <- str_to_upper(commuters_filtered$town)

towns <- left_join(towns, commuters_filtered)

towns$percent.drivers.all <- round((towns$drivers/towns$pop2013)*100, digits=2)
  
income <- read.csv("data/medianincome.csv", stringsAsFactors=FALSE)
income <- income[,-1]
income <- income[-1,]
colnames(income) <- c("town", "median.income", "error")
income <- income[c("town", "median.income")]
income$town <- sub(" town,.*", "", income$town)
income$town <- str_to_upper(income$town)

towns <- left_join(towns, income)

# Bringing in the tickets
incidents <- read.csv("http://ctrp3viz.s3.amazonaws.com/data/Connecticut_r1.csv", stringsAsFactors=FALSE)
warnings <- data.frame(table(incidents$Department.Name, incidents$InterventionDispositionCode))
colnames(warnings) <- c("Department", "Disposition", "Freq")
warnings_df <- warnings %>% spread(Disposition, Freq)
warnings_df$total <- warnings_df$I + warnings_df$M + warnings_df$N + warnings_df$U + warnings_df$V + warnings_df$W
warnings_df$warnings <- warnings_df$V + warnings_df$W
warnings_df$tickets <- warnings_df$total - warnings_df$warnings

warnings_df$perc_warnings <- round((warnings_df$warnings/warnings_df$total)*100, digits=2)
warnings_df$perc_tickets <- 100 - warnings_df$perc_warnings
warnings_df_table <- warnings_df[c("Department", "total", "tickets", "warnings", "perc_tickets", "perc_warnings")]

#exclude Stamford because of unreliable data

warnings_df_table <- filter(warnings_df_table, Department!="Stamford")

tix <- warnings_df_table

colnames(tix) <- c("town", "stops", "tickets", "warnings", "perc_tickets", "perc_warnings")
tix$town <- str_to_upper(tix$town)

tix_df <- left_join(towns, tix)

tix_df <- na.omit(tix_df)


#referring to old dataframe, incidents_by_dept
dept_staff <- incidents_by_dept[c("name", "sworn.male", "sworn.female", "sworn", "total")]
colnames(dept_staff) <- c("town", "sworn.male", "sworn.female", "sworn", "dept.total")

tix_df <- left_join(tix_df, dept_staff)

tix_only <- tix_df[,25]

colnames(tix_df) <- c("town", "Average daily traffic", "Number of roads", 
                      "Number of interstate roads", "Number of arterials: Freeways and expressways",
                      "Number of arterials: Other", "Minor arterial roads", "Major collector roads",
                      "Minor collector roads", "Local roads", "Daily vehicle miles traveled", 
                      "Miles of road", "Mileage", "Town square miles", "Town population", 
                      "Population per square mile", "Retail tax 2014", "Total tax 2014",
                      "Total commuters", "Total drivers", "Drivers as a percent of commuters", 
                      "Drivers as a percent of total population", "Town median income", 
                      "Total traffic stops in 2014", "Total tickets issued in 2014", "Total warnings issued in 2014", 
                      "Percent of tickets given out of all stops", 
                      "Percent of warnings given out of all stops", "Sworn male officers", 
                      "Sworn female officers", "Total sworn officers", "Total police department employees")


tix_list <- 2:ncol(tix_df)
for (i in tix_list) {
  row_name <- colnames(tix_df[i])
  corre <- cor(tix_only, tix_df[,i], use="pairwise.complete.obs")
  loop_array <- data.frame(row_name, corre)
  if (i == 2) {
    array <- loop_array } else 
    { array <- rbind(array, loop_array) }
}

library(ggplot2)

for (i in tix_list) {
title <- paste("tickets vs ", array$row_name[i-1], ": ", round(array$corre[i-1], digits=2))
p <- ggplot(tix_df, aes(tix_df[,25], tix_df[,i]))
p + geom_point() + ggtitle(title) + ylab(array$row_name[i-1])

filename <- paste("tickets_", array$row_name[i-1], "_chart.png")
ggsave(filename, width=8, height=5, dpi=100)

}

write.csv(array, "data/tickets_array.csv")

#ok, for warnings now
warn_only <- tix_df[,26]
for (i in tix_list) {
  row_name <- colnames(tix_df[i])
  corre <- cor(warn_only, tix_df[,i], use="pairwise.complete.obs")
  loop_array <- data.frame(row_name, corre)
  if (i == 2) {
    array <- loop_array } else 
    { array <- rbind(array, loop_array) }
}

library(ggplot2)

for (i in tix_list) {
  title <- paste("warnings vs ", array$row_name[i-1], ": ", round(array$corre[i-1], digits=2))
  p <- ggplot(tix_df, aes(tix_df[,26], tix_df[,i]))
  p + geom_point() + ggtitle(title) + ylab(array$row_name[i-1])
  
  filename <- paste("warnings_", array$row_name[i-1], "_chart.png")
  ggsave(filename, width=8, height=5, dpi=100)
  
}

write.csv(array, "data/warnings_array.csv")



#ok for traffic stops now
stops_only <- tix_df[,24]
for (i in tix_list) {
  row_name <- colnames(tix_df[i])
  corre <- cor(stops_only, tix_df[,i], use="pairwise.complete.obs")
  loop_array <- data.frame(row_name, corre)
  if (i == 2) {
    array <- loop_array } else 
    { array <- rbind(array, loop_array) }
}

library(ggplot2)

for (i in tix_list) {
  title <- paste("traffic stops vs ", array$row_name[i-1], ": ", round(array$corre[i-1], digits=2))
  p <- ggplot(tix_df, aes(tix_df[,24], tix_df[,i]))
  p + geom_point() + ggtitle(title) + ylab(array$row_name[i-1])
  
  filename <- paste("stops_", array$row_name[i-1], "_chart.png")
  ggsave(filename, width=8, height=5, dpi=100)
  
}

write.csv(array, "data/stops_array.csv")

#FAKE CORRELATOR

dunks <- read.csv("data/dunks.csv", stringsAsFactors=FALSE)
dunks <- ctnamecleaner(Towns, dunks)
colnames(dunks) <- c("town", "dunkinrate","dunks", "real.town")
dunks$town <- str_to_upper(dunks$town)
fake <- left_join(tix, dunks)
fake2 <- ctpopulator(town, dept_staff)
fake <- left_join(fake, fake2)
fake$deptrate <- (round((fake$dept.total/fake$pop2013)*10000, digits=2))
fake$dunkrate <- (round(fake$pop2013/fake$dunks, digits=2))
fake$deptrate2 <- (round((fake$pop2013/fake$dept.total)*10000, digits=2))

fake <- na.omit(fake)
fake$tixrate <- (round((fake$tickets/fake$pop2013)*10000, digits=2))
fake$tixrate2 <- round(fake$pop2013/fake$tickets, digits=2)
cor(fake$deptrate2, fake$dunkrate)
cor(fake$deptrate, fake$dunkinrate)



trendchart(newdata, headline = "Towns with most roads in Connecticut", subhead = "", src = "Department of Transportation",
           byline = "TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "legselect")

fancytable(towns, headline = "Traffic count by town", subhead = "", height = 400,
           paging = "false", sourceline = "Department of Transportation", byline = "TrendCT.org", col = 1,
           desc_asc = "desc")