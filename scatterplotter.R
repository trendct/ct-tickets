html_start <- "<!DOCTYPE HTML>
<html>
<head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
    <title>"

html2 <- "</title>
  <link href='http://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,700' rel='stylesheet' type='text/css'>
  <script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js\"></script>
    <script type=\"text/javascript\" src=\"http://code.highcharts.com/highcharts.js\"> </script>  
    <link rel=StyleSheet href=\"http://projects.ctmirror.org/libs/universal.css\" type=\"text/css\">
    <link rel=StyleSheet href=\"libs/style.css\" type=\"text/css\">
</head>
<body>

    <div class=\"interactiveContainer\">
        <div class=\"graphicHeader\">"

java1 <- "<script>

$(function () {
$(\'#container\').highcharts({
chart: {
type: \'scatter\'
},
title: {
text: \'\',
x: -20,
style: {
display: \'none\'
}
},
subtitle: {
text: \'\',
x: -20,
style: {
display: \'none\'
}
},
plotOptions: {
scatter: {
color: \"rgba(35,100,35,0.6)\",
                marker: {
                    radius: 4,
                    symbol: \"circle\"
                }
            }
        },
        credits: {
            enabled: false
        },
        exporting: {
            enabled: false
        },
        xAxis: {
            min: 0,
            tickPixelInterval: 100,
            title : {
                text: \'"

java2 <- "
                style: {
fontFamily: \"Source Sans Pro, Arial\",
color: \"#444\",
fontSize: 17,
fontWeight: \"100\"
}
},
labels: {
valueSuffix: \"\",
                style: {
                    fontFamily: \"Source Sans Pro, Arial\",
                    color: \"#aaa\",
                    fontSize: 13,
                    fontWeight: \"100\"
                },
                formatter: function() {
                    return \"\" + comma(this.value);
                }
            }
        },
        yAxis: {
            min: 0,
            tickPixelInterval: 50,
            title: {
                text: \'"

java3 <- "\',
                style: {
fontFamily: \"Source Sans Pro, Arial\",
color: \"#444\",
fontSize: 17,
fontWeight: \"100\"
}
},
labels: {
style: {
fontFamily: \"Source Sans Pro, Arial\",
color: \"#aaa\",
fontSize: 13,
fontWeight: \"100\"
                },
                formatter: function() {
                    return comma(this.value);
                }
            },
            lineColor: \"#eee\"
        },
        tooltip: {
            shadow: false,
            backgroundColor: \"#ffffff\",
            borderColor: \"#444444\",
            borderWidth: 2,
            useHTML: true,
            headerFormat: \'<div class=\"tooltipHead\">{point.key}</div>\',
            formatter: function() {
                return \"<div class=\'toolTipper\'><b>\" + this.series.name + \"</b><br><div class=\'kicker\'>"

java4 <- "</div>\" + comma(this.y) + \"</div><div class=\'tooltipbottom\'><div class=\'kicker\'>"

java5 <- "</div>\" + comma(this.key) + \"</div>\";
            },
   //          formatter: function() {
//  return \"<h2>\" + this.series.name + \"</h2><br><b>Density:</b> \" +  Math.round(this.y) + \" per sq. mile<br><b>Times won per 1,000 people:</b> \" +  comma(this.x);
// },
style: {
fontFamily: \"Lato, Arial\",
color: \"#333\",
fontSize: 14,
opacity: 1,
fontWeight: \"400\"
}
},
legend: {
layout: \'vertical\',
align: \'right\',
verticalAlign: \'middle\',
borderWidth: 0,
enabled: false,
itemStyle: {
fontFamily: \"Source Sans Pro, Arial\",
                color: \"#444\",
                fontSize: 13,
                fontWeight: \"100\"
            }
        },
        series: data
    });


function comma(val){
  while (/(\\d+)(\\d{3})/.test(val.toString())){
    val = val.toString().replace(/(\\d+)(\\d{3})/, \'$1\'+\',\'+\'$2\');
  }
  return val;
}
});
"

#loop goes here

#double <- tix_df[c("town", "tickets", "pop2013")]

#df <- double



tix_df$town <- str_to_title(tix_df$town)
xaxis <- "Tickets"

for (i in tix_list) {
  
  title <- paste(xaxis, " and ", array$row_name[i-1], ": ", round(array$corre[i-1], digits=2), sep="")
  array_name <- array$row_name[i-1]
  explainer <- ""
  sourceline <- ""
  byline <- ""
  
#html <- paste(html_start, title, html2, title, "</div>\n<div class=\"explainer\">", sep="")
#html <- paste(html, explainer, "</div>\n<div id=\"container\"></div>\n<div class=\"sourceline\">", sep="")
#html <- paste(html, sourceline, "</div>\n<div class=\"smallByline\">", byline, "</div>\n", sep="")

html_for_quiz <- "<!DOCTYPE HTML>
<html>
<head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
    <title>For quiz</title>
  <link href=\'http://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,700\' rel=\'stylesheet\' type=\'text/css\'>
  <script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js\"></script>
    <script type=\"text/javascript\" src=\"http://code.highcharts.com/highcharts.js\"> </script>  
    <link rel=StyleSheet href=\"http://projects.ctmirror.org/libs/universal.css\" type=\"text/css\">
    <link rel=StyleSheet href=\"libs/style.css\" type=\"text/css\">
</head>
<body>

    <div class=\"interactiveContainer\">
<div id=\"container\"></div>
<div class=\"sourceline\"></div>
<div class=\"smallByline\"></div>"

#html <- paste(html, java1, sep="")

html <- paste(html_for_quiz, java1, sep="")
html <- paste(html, xaxis, "\', ")
html <- paste(html, java2, array$row_name[i-1], sep="")
html <- paste(html, java3, array$row_name[i-1], sep="")
html <- paste(html, java4, xaxis, sep="")
html <- paste(html, java5, sep="")

#Column to pivot on (tickets, column 25)
    dif <- data.frame(tix_df$town, tix_df[,25])
    dif <- cbind(dif, tix_df[,i])

    df_length <- 1:nrow(dif)
          data_array <- ""
          for (i in df_length) {
            name <- dif[i,1]
            data1 <- dif[i,2]
            data2 <- dif[i,3]
            data_array <- paste(data_array, "{\n\"name\" : \"", name, "\",\n\"data\" : [[", data1, ",", data2, "]]\n},", sep="")            
          }

data_array <- gsub('.{1}$', '', data_array)

data <- paste("var data = [ \n", data_array, "  ];
</script>
</body>
</html>")
array_name_file <- gsub(" ", "", array_name)
html <- paste(html, data, sep="")

filename <- paste("/Users/andrewtran/Documents/Github/ct-tickets/scatter_charts/", xaxis, array_name_file, "_chart.html", sep="")
write(html, filename)

}

# WARNINGS

xaxis <- "Warnings"

for (i in tix_list) {
  
  title <- paste(xaxis, " and ", array$row_name[i-1], ": ", round(array$corre[i-1], digits=2), sep="")
  array_name <- array$row_name[i-1]
  explainer <- ""
  sourceline <- ""
  byline <- ""
  
  #html <- paste(html_start, title, html2, title, "</div>\n<div class=\"explainer\">", sep="")
  #html <- paste(html, explainer, "</div>\n<div id=\"container\"></div>\n<div class=\"sourceline\">", sep="")
  #html <- paste(html, sourceline, "</div>\n<div class=\"smallByline\">", byline, "</div>\n", sep="")
  
  html_for_quiz <- "<!DOCTYPE HTML>
  <html>
  <head>
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
  <title>For quiz</title>
  <link href=\'http://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,700\' rel=\'stylesheet\' type=\'text/css\'>
  <script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js\"></script>
  <script type=\"text/javascript\" src=\"http://code.highcharts.com/highcharts.js\"> </script>  
  <link rel=StyleSheet href=\"http://projects.ctmirror.org/libs/universal.css\" type=\"text/css\">
  <link rel=StyleSheet href=\"libs/style.css\" type=\"text/css\">
  </head>
  <body>
  
  <div class=\"interactiveContainer\">
  <div id=\"container\"></div>
  <div class=\"sourceline\"></div>
  <div class=\"smallByline\"></div>"
  
  #html <- paste(html, java1, sep="")
  
  html <- paste(html_for_quiz, java1, sep="")
  html <- paste(html, xaxis, "\', ")
  html <- paste(html, java2, array$row_name[i-1], sep="")
  html <- paste(html, java3, array$row_name[i-1], sep="")
  html <- paste(html, java4, xaxis, sep="")
  html <- paste(html, java5, sep="")
  
  #Column to pivot on (warnings, column 26)
  dif <- data.frame(tix_df$town, tix_df[,26])
  dif <- cbind(dif, tix_df[,i])
  
  df_length <- 1:nrow(dif)
  data_array <- ""
  for (i in df_length) {
    name <- dif[i,1]
    data1 <- dif[i,2]
    data2 <- dif[i,3]
    data_array <- paste(data_array, "{\n\"name\" : \"", name, "\",\n\"data\" : [[", data1, ",", data2, "]]\n},", sep="")            
  }
  
  data_array <- gsub('.{1}$', '', data_array)
  
  data <- paste("var data = [ \n", data_array, "  ];
</script>
</body>
</html>")
  array_name_file <- gsub(" ", "", array_name)
  html <- paste(html, data, sep="")
  
  filename <- paste("/Users/andrewtran/Documents/Github/ct-tickets/scatter_charts/", xaxis, array_name_file, "_chart.html", sep="")
  write(html, filename)
  
}

# STOPS

xaxis <- "Traffic stops"

for (i in tix_list) {
  
  title <- paste(xaxis, " and ", array$row_name[i-1], ": ", round(array$corre[i-1], digits=2), sep="")
  array_name <- array$row_name[i-1]
  explainer <- ""
  sourceline <- ""
  byline <- ""
  
  #html <- paste(html_start, title, html2, title, "</div>\n<div class=\"explainer\">", sep="")
  #html <- paste(html, explainer, "</div>\n<div id=\"container\"></div>\n<div class=\"sourceline\">", sep="")
  #html <- paste(html, sourceline, "</div>\n<div class=\"smallByline\">", byline, "</div>\n", sep="")
  
  html_for_quiz <- "<!DOCTYPE HTML>
  <html>
  <head>
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
  <title>For quiz</title>
  <link href=\'http://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,700\' rel=\'stylesheet\' type=\'text/css\'>
  <script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js\"></script>
  <script type=\"text/javascript\" src=\"http://code.highcharts.com/highcharts.js\"> </script>  
  <link rel=StyleSheet href=\"http://projects.ctmirror.org/libs/universal.css\" type=\"text/css\">
  <link rel=StyleSheet href=\"libs/style.css\" type=\"text/css\">
  </head>
  <body>
  
  <div class=\"interactiveContainer\">
  <div id=\"container\"></div>
  <div class=\"sourceline\"></div>
  <div class=\"smallByline\"></div>"
  
  #html <- paste(html, java1, sep="")
  
  html <- paste(html_for_quiz, java1, sep="")
  html <- paste(html, xaxis, "\', ")
  html <- paste(html, java2, array$row_name[i-1], sep="")
  html <- paste(html, java3, array$row_name[i-1], sep="")
  html <- paste(html, java4, xaxis, sep="")
  html <- paste(html, java5, sep="")
  
  #Column to pivot on (warnings, column 26)
  dif <- data.frame(tix_df$town, tix_df[,24])
  dif <- cbind(dif, tix_df[,i])
  
  df_length <- 1:nrow(dif)
  data_array <- ""
  for (i in df_length) {
    name <- dif[i,1]
    data1 <- dif[i,2]
    data2 <- dif[i,3]
    data_array <- paste(data_array, "{\n\"name\" : \"", name, "\",\n\"data\" : [[", data1, ",", data2, "]]\n},", sep="")            
  }
  
  data_array <- gsub('.{1}$', '', data_array)
  
  data <- paste("var data = [ \n", data_array, "  ];
</script>
</body>
</html>")
  array_name_file <- gsub(" ", "", array_name)
  html <- paste(html, data, sep="")
  xaxis <- gsub(" ", "", xaxis)
  filename <- paste("/Users/andrewtran/Documents/Github/ct-tickets/scatter_charts/", xaxis, array_name_file, "_chart.html", sep="")
  write(html, filename)
  
}

# find the fake correlationi
# dunks <- read.csv("dunk.csv", stringsAsFactors=FALSE)

dunk_imp <- read.csv("dunkimport.csv", stringsAsFactors=FALSE)
dunk_imp$town <- str_to_upper(dunk_imp$town)

fake <- left_join(dunk_imp, tix)
fake <- na.omit(fake)

cor(fake$dunks, fake$tickets)

# Dunkin Donuts only

xaxis <- "Tickets"

for (i in tix_list) {
  
  title <- paste(xaxis, " and ", array$row_name[i-1], ": ", round(array$corre[i-1], digits=2), sep="")
  array_name <- "Number of Dunkin' Donuts"
  explainer <- ""
  sourceline <- ""
  byline <- ""
  
  #html <- paste(html_start, title, html2, title, "</div>\n<div class=\"explainer\">", sep="")
  #html <- paste(html, explainer, "</div>\n<div id=\"container\"></div>\n<div class=\"sourceline\">", sep="")
  #html <- paste(html, sourceline, "</div>\n<div class=\"smallByline\">", byline, "</div>\n", sep="")
  
  html_for_quiz <- "<!DOCTYPE HTML>
  <html>
  <head>
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
  <title>For quiz</title>
  <link href=\'http://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,700\' rel=\'stylesheet\' type=\'text/css\'>
  <script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js\"></script>
  <script type=\"text/javascript\" src=\"http://code.highcharts.com/highcharts.js\"> </script>  
  <link rel=StyleSheet href=\"http://projects.ctmirror.org/libs/universal.css\" type=\"text/css\">
  <link rel=StyleSheet href=\"libs/style.css\" type=\"text/css\">
  </head>
  <body>
  
  <div class=\"interactiveContainer\">
  <div id=\"container\"></div>
  <div class=\"sourceline\"></div>
  <div class=\"smallByline\"></div>"
  
  #html <- paste(html, java1, sep="")
  
  html <- paste(html_for_quiz, java1, sep="")
  html <- paste(html, xaxis, "\', ")
  html <- paste(html, java2, "Dunkin' Donuts stores", sep="")
  html <- paste(html, java3, "Dunkin' Donuts stores", sep="")
  html <- paste(html, java4, xaxis, sep="")
  html <- paste(html, java5, sep="")
  
  #Column to pivot on (warnings, column 26)
  dif <- data.frame(fake$town, fake$tickets)
  dif <- cbind(dif, fake$dunks)
  
  df_length <- 1:nrow(dif)
  data_array <- ""
  for (i in df_length) {
    name <- dif[i,1]
    data1 <- dif[i,2]
    data2 <- dif[i,3]
    data_array <- paste(data_array, "{\n\"name\" : \"", name, "\",\n\"data\" : [[", data1, ",", data2, "]]\n},", sep="")            
  }
  
  data_array <- gsub('.{1}$', '', data_array)
  
  data <- paste("var data = [ \n", data_array, "  ];
</script>
</body>
</html>")
  array_name_file <- gsub(" ", "", array_name)
  html <- paste(html, data, sep="")
  xaxis <- gsub(" ", "", xaxis)
  filename <- paste("/Users/andrewtran/Documents/Github/ct-tickets/scatter_charts/", xaxis, array_name_file, "_chart.html", sep="")
  write(html, filename)
  
}