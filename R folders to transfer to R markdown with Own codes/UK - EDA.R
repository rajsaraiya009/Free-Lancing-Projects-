mypack <- function(package){
  new.package <- package[!(package %in% installed.packages()[, "Package"])]
  if (length(new.package)) 
    install.packages(new.package, dependencies = TRUE)
  sapply(package, require, character.only = TRUE)}
packages <- c("dplyr", "caret", "tidyr", "tibble", "forcats", "knitr", "corrplot",
              "esquisse", "fastDummies", "plotly", "lubridate", "timeDate",
              "gridExtra", "visdat", "IRdisplay", "highcharter")
mypack(packages)

##Load cleaned file
accidents_df <- read.csv(file.choose(), header=TRUE)
accidents_df <- as_tibble(accidents_df)
accidents_numerical_df <- dummy_cols(accidents_df,
                                     select_columns = c('Region', 'Urban_or_Rural_Area', 'X1st_Road_Class', 'Road_Type',
                                                        'Road_Surface_Conditions', 'Weather', 'High_Wind', 'Lights',
                                                        'Junction_Detail', 'Junction_Location', 'X1st_Point_of_Impact',
                                                        'Driver_Journey_Purpose', 'Propulsion_Code', 'Vehicle_Make',
                                                        'Vehicle_Category', 'Vehicle_Manoeuvre'),
                                     remove_most_frequent_dummy=TRUE)

columns_to_drop <- c('Accident_Index', 'Latitude', 'Longitude', 'Datetime', 'Region', 'Urban_or_Rural_Area',
                     'X1st_Road_Class', 'Road_Type', 'Road_Surface_Conditions', 'Weather', 'High_Wind',
                     'Lights', 'Junction_Detail', 'Junction_Location', 'X1st_Point_of_Impact',
                     'Driver_Journey_Purpose', 'Propulsion_Code', 'Vehicle_Make', 'Vehicle_Category',
                     'Vehicle_Manoeuvre')

accidents_numerical_df <- accidents_numerical_df[ , !(names(accidents_numerical_df) %in% columns_to_drop)]
accidents_numerical_df_normalized <- predict(preProcess(accidents_numerical_df, method=c("center", "scale")), accidents_numerical_df)
accidents_numerical_df_normalized <- dummy_cols(accidents_numerical_df_normalized,
                                                select_columns = c('Accident_Severity'),
                                                remove_most_frequent_dummy=TRUE)
columns_to_drop <- c('Accident_Severity')
names(accidents_numerical_df_normalized)[names(accidents_numerical_df_normalized) == 'Accident_Severity_Fatal_Serious'] <- 'Fatal_or_Serious_Accident'
accidents_numerical_df_normalized <- accidents_numerical_df_normalized[ , !(names(accidents_numerical_df_normalized) %in% columns_to_drop)]
accidents_numerical_df <- dummy_cols(accidents_numerical_df,
                                     select_columns = c('Fatal_or_Serious_Accident'),
                                     remove_most_frequent_dummy=TRUE)

str(accidents_numerical_df)

columns_to_drop <- c('Accident_Severity')
names(accidents_numerical_df)[names(accidents_numerical_df) == 'Accident_Severity_Fatal_Serious'] <- 'Fatal_or_Serious_Accident'
accidents_numerical_df <- accidents_numerical_df[ , !(names(accidents_numerical_df) %in% columns_to_drop)]
rm(columns_to_drop)

#Correlation plots with the corrplot package
cols_to_filter <- c('Driver_IMD_Decile', 'Speed_limit', 'Year', 'Season', 'Month_of_Year',
                    'Day_of_Month', 'Day_of_Week', 'Hour_of_Day', 'Number_of_Vehicles',
                    'Age_of_Driver', 'Age_of_Vehicle', 'Engine_CC', 'Fatal_or_Serious_Accident')
corrplot(cor(accidents_numerical_df_normalized[, cols_to_filter]), type="upper", method="circle", tl.col="black", tl.cex=0.6, tl.offset=0.5)

cols_to_filter <- c('Urban_or_Rural_Area_Rural', 'X1st_Road_Class_B', 'X1st_Road_Class_Unclassified', 'X1st_Road_Class_C', 'X1st_Road_Class_Motorway', 
                    'X1st_Road_Class_A(M)', 'Road_Type_Roundabout', 'Road_Type_Dual carriageway', 'Road_Type_One way street', 'Road_Type_Slip road', 
                    'Road_Surface_Conditions_Wet or damp', 'Road_Surface_Conditions_Frost or ice', 'Road_Surface_Conditions_Snow', 
                    'Road_Surface_Conditions_Flood over 3cm. deep', 'High_Wind_Yes', 'Propulsion_Code_Heavy oil', 'Fatal_or_Serious_Accident')
corrplot(cor(accidents_numerical_df_normalized[, cols_to_filter]), type="upper", method="circle", tl.col="black", tl.cex=0.6, tl.offset=0.5)

#3
cols_to_filter <- c('Junction_Detail_Crossroads', 'Junction_Detail_T or staggered junction', 'Junction_Detail_More than 4 arms (not roundabout)',
                    'Junction_Detail_Roundabout', 'Junction_Detail_Private drive or entrance', 'Junction_Detail_Slip road',
                    'Junction_Detail_Other junction', 'Junction_Location_Mid Junction - on roundabout or on main road',
                    'Junction_Location_Approaching junction or waiting/parked at junction approach',
                    'Junction_Location_Cleared junction or waiting/parked at junction exit', 'Junction_Location_Leaving main road',
                    'Junction_Location_Entering main road', 'Junction_Location_Leaving roundabout', 'Junction_Location_Entering from slip road',
                    'Junction_Location_Entering roundabout', 'X1st_Point_of_Impact_Offside', 'X1st_Point_of_Impact_Nearside',
                    'X1st_Point_of_Impact_Back', 'X1st_Point_of_Impact_Did not impact', 'Fatal_or_Serious_Accident')
corrplot(cor(accidents_numerical_df_normalized[, cols_to_filter]), type="upper", method="circle", tl.col="black", tl.cex=0.6, tl.offset=0.5)

#Histogram Severity
p1 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Slight", ]$Season, histnorm="probability", name="Slight") %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Fatal_Serious", ]$Season, histnorm="probability", name="Fatal-Serious") %>%
  layout(barmode="overlay", title="Season comparison", xaxis=list(title="Season"))
print(p1)

#histogram age vs severity
p2 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Slight", ]$Age_of_Driver, histnorm="probability", name="Slight") %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Fatal_Serious", ]$Age_of_Driver, histnorm="probability", name="Fatal-Serious") %>%
  layout(barmode="overlay", title="Age of Driver comparison", xaxis=list(title="Age of Driver"))
print(p2)

#histogram age vehicle vs severity
p3 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Slight", ]$Age_of_Vehicle, histnorm="probability", name="Slight") %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Fatal_Serious", ]$Age_of_Vehicle, histnorm="probability", name="Fatal-Serious") %>%
  layout(barmode="overlay", title="Age of Vehicle comparison", xaxis=list(title="Age of Vehicle", range=c(0, 20)))
print(p3)

#histogram weather vs severity
p4 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Slight", ]$Weather, histnorm="probability", name="Slight") %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Fatal_Serious", ]$Weather, histnorm="probability", name="Fatal-Serious") %>%
  layout(barmode="overlay", title="Weather comparison", xaxis=list(title="Weather"))
print(p4)

#histogram region vs severity
p5 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Slight", ]$Region, histnorm="probability", name="Slight") %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Fatal_Serious", ]$Region, histnorm="probability", name="Fatal-Serious") %>%
  layout(barmode="overlay", title="Region comparison", xaxis=list(title="Region"))
print(p5)

#histogram ruralurban vs severity
p6 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Slight", ]$Urban_or_Rural_Area, histnorm="probability", name="Slight") %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Fatal_Serious", ]$Urban_or_Rural_Area, histnorm="probability", name="Fatal-Serious") %>%
  layout(barmode="overlay", title="Urban or Rural Area comparison", xaxis=list(title="Urban or Rural Area"))
print(p6)

#speedlimit vs severity
p7 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Slight", ]$Speed_limit, histnorm="probability", name="Slight") %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Fatal_Serious", ]$Speed_limit, histnorm="probability", name="Fatal-Serious") %>%
  layout(barmode="overlay", title="Speed Limit comparison", xaxis=list(title="Speed Limit"))
print(p7)

#road type
p8 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Slight", ]$Road_Type, histnorm="probability", name="Slight") %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Fatal_Serious", ]$Road_Type, histnorm="probability", name="Fatal-Serious") %>%
  layout(barmode="overlay", title="Road Type comparison", xaxis=list(title="Road Type"))
print(p8)

#road surface
p9 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Slight", ]$Road_Surface_Conditions, histnorm="probability", name="Slight") %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Fatal_Serious", ]$Road_Surface_Conditions, histnorm="probability", name="Fatal-Serious") %>%
  layout(barmode="overlay", title="Road Surface Conditions comparison", xaxis=list(title="Road Surface Condition"))
print(p9)

#lights comparison
p10 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Slight", ]$Lights, histnorm="probability", name="Slight") %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Fatal_Serious", ]$Lights, histnorm="probability", name="Fatal-Serious") %>%
  layout(barmode="overlay", title="Lights comparison", xaxis=list(title="Light"))
print(p10)

#junction comparison
p11 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Slight", ]$Junction_Detail, histnorm="probability", name="Slight") %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Fatal_Serious", ]$Junction_Detail, histnorm="probability", name="Fatal-Serious") %>%
  layout(barmode="overlay", title="Junction comparison", xaxis=list(title="Junction"))
print(p11)

#1st point Impact
p12 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Slight", ]$X1st_Point_of_Impact, histnorm="probability", name="Slight") %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Fatal_Serious", ]$X1st_Point_of_Impact, histnorm="probability", name="Fatal-Serious") %>%
  layout(barmode="overlay", title="1st Point of Impact comparison", xaxis=list(title="1st Point of Impact"))
print(p12)

#Vehicle Make
p13 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Slight", ]$Vehicle_Make, histnorm="probability", name="Slight") %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Fatal_Serious", ]$Vehicle_Make, histnorm="probability", name="Fatal-Serious") %>%
  layout(barmode="overlay", title="Vehicle Make comparison", xaxis=list(title="Vehicle Make"))
print(p13)

#vehicle category
p14 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Slight", ]$Vehicle_Category, histnorm="probability", name="Slight") %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Fatal_Serious", ]$Vehicle_Category, histnorm="probability", name="Fatal-Serious") %>%
  layout(barmode="overlay", title="Vehicle Category comparison", xaxis=list(title="Vehicle Category"))
print(p14)

#heatmap hour day week
# All Accidents
p1 <- select(accidents_df, one_of(c("Accident_Index", "Datetime", "Accident_Severity"))) %>%
  mutate(dhour = format(strptime(Datetime,"%Y-%m-%d %H:%M:%S"),'%H'),
         wday = wday(Datetime, label=TRUE)) %>%
  mutate(wday = fct_relevel(wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>%
  group_by(dhour, wday) %>%
  summarise(accidents_count = n_distinct(Accident_Index))  %>%
  ggplot(aes(dhour, wday, fill=accidents_count)) +
  geom_tile() +
  coord_fixed() +
  labs(x="Hour of Day", y="Day of Week") +
  scale_fill_distiller(palette="Spectral", name="Accidents Count") +
  ggtitle("All Accidents") +
  theme(plot.margin=grid::unit(c(2,2,2,2), "mm"))

# Fatal-Serious Accidents
p2 <- select(accidents_df, one_of(c("Accident_Index", "Datetime", "Accident_Severity"))) %>%
  filter(Accident_Severity=="Fatal_Serious") %>%
  mutate(dhour = format(strptime(Datetime,"%Y-%m-%d %H:%M:%S"),'%H'),
         wday = wday(Datetime, label=TRUE)) %>%
  mutate(wday = fct_relevel(wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>%
  group_by(dhour, wday) %>%
  summarise(accidents_count = n_distinct(Accident_Index))  %>%
  ggplot(aes(dhour, wday, fill=accidents_count)) +
  geom_tile() +
  coord_fixed() +
  labs(x="Hour of Day", y="Day of Week") +
  scale_fill_distiller(palette="Spectral", name="Accidents Count") +
  ggtitle("Fatal-Serious Accidents") +
  theme(plot.margin=grid::unit(c(2,2,2,2), "mm"))

# Slight Accidents
p3 <- select(accidents_df, one_of(c("Accident_Index", "Datetime", "Accident_Severity"))) %>%
  filter(Accident_Severity=="Slight") %>%
  mutate(dhour = format(strptime(Datetime,"%Y-%m-%d %H:%M:%S"),'%H'),
         wday = wday(Datetime, label=TRUE)) %>%
  mutate(wday = fct_relevel(wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>%
  group_by(dhour, wday) %>%
  summarise(accidents_count = n_distinct(Accident_Index))  %>%
  ggplot(aes(dhour, wday, fill=accidents_count)) +
  geom_tile() +
  coord_fixed() +
  labs(x="Hour of Day", y="Day of Week") +
  scale_fill_distiller(palette="Spectral", name="Accidents Count") +
  ggtitle("Slight Accidents") +
  theme(plot.margin=grid::unit(c(2,2,2,2), "mm"))

grid.arrange(p1, p2, p3, ncol=1)

#hours day vs severity
p16 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Slight", ]$Hour_of_Day, histnorm="probability", name="Slight", nbinsx=100) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Fatal_Serious", ]$Hour_of_Day, histnorm="probability", name="Fatal-Serious", nbinsx=100) %>%
  layout(barmode="overlay", title="Hour of Day comparison", xaxis=list(title="Hour of Day"))
print(p16)

#days vs severity
p17 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Slight", ]$Day_of_Week, histnorm="probability", name="Slight") %>%
  add_histogram(x=~accidents_df[accidents_df$Accident_Severity == "Fatal_Serious", ]$Day_of_Week, histnorm="probability", name="Fatal-Serious") %>%
  layout(barmode="overlay", title="Day of Week comparison", xaxis=list(title="Day of Week"))
print(p17)
