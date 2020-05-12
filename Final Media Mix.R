library(tidyverse)
library(readxl)
library(ggplot2)
library(ISLR)
library(modelr)
library(broom)
library(lubridate)
library(scales)
library(gridExtra)
library(janitor)
library(broom)
library(plot)
library(plotly)
library(corrplot)
library(ggcorrplot)
library(GGally)
library(corrr)
library(corrplot)
library(mvoutlier)
library(car)
library(lm.beta)
library(survival)
library(lattice)
library(Formula)
library(mctest)

media_data = read.csv("/Users/bhavinghoghari/Desktop/Pace University/Sem 2 other Tasks/Group m Competetion/Pace University Data/CSV Files/Case Competition Media and Sales Data V2.csv", header = T, sep = ",")
weather_data = read.csv("/Users/bhavinghoghari/Desktop/Pace University/Sem 2 other Tasks/Group m Competetion/Pace University Data/CSV Files/Weather Data.csv", header = T, sep = ",")
unemployment_data = read.csv("/Users/bhavinghoghari/Desktop/Pace University/Sem 2 other Tasks/Group m Competetion/Pace University Data/CSV Files/Unemployment Rate.csv", header = T, sep = ",")


# Change date format using library(lubridate)
media_data$Period = mdy(media_data$Period)
weather_data$weeks = mdy(weather_data$weeks)
unemployment_data$Period = mdy_hm(unemployment_data$Period)


# Create media data set - filter out by product
  #Alpha v5
alphaV5_media_data = media_data %>% 
  filter(Product == "Alpha v5")
  #Nomad
nomad_media_data = media_data %>% 
  filter(Product == "Nomad")
  #Soft Fusion
softFusion_media_data = media_data %>% 
  filter(Product == "Soft Fusion")


# Sales and Media Spend
  # Total Sales
total_sales_by_product = media_data %>% 
  filter(VariableName == "Sales") %>% 
  group_by(Product) %>% 
  summarise(total_sales = sum(VariableValue))

  # Total Media Spend
total_media_spend_by_product = media_data %>% 
  group_by(Product) %>% 
  filter(str_detect(VariableName, 'Spend')) %>% 
  summarise(total_media_spend = sum(VariableValue))

  # Ploting total sales and media spend
ggplot(total_sales_by_product, aes(x = Product, y = total_sales, fill = Product)) +
  geom_col(alpha = 0.6, width = 0.6) +
  geom_col(data = total_media_spend_by_product, # ploting media spend within the spend bar
           aes(x = Product, 
               y = total_media_spend), width = 0.4) + 
  geom_text(aes(label = dollar(total_sales)), vjust = -0.8) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Sales By Product", 
       subtitle = "And Media Spend", 
       x = "", y = "Sales") +
  theme_minimal()

  # Total sales over period by product
total_sales_by_product_over_period = media_data %>% 
  group_by(Period, Product) %>% 
  filter(VariableName == "Sales") %>% 
  summarise(total_sales = sum(VariableValue))

  # Total media spend over period by product
total_media_spend_by_product_over_period = media_data %>% 
  group_by(Period, Product) %>% 
  filter(str_detect(VariableName, 'Spend')) %>% 
  summarise(total_media_spend = sum(VariableValue))

  # Ploting sales and media spend over period. Color by Product
ggplot(total_sales_by_product_over_period,aes(x = Period, y = total_sales, fill = Product)) +
  geom_area(alpha = 0.6) +
  geom_area(data = total_media_spend_by_product_over_period, aes(x = Period, y = total_media_spend)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Sales Over Period", 
       subtitle = "And Media Spend", 
       x = "", y = "Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Ploting media spend over period. Color by product
ggplot(total_media_spend_by_product_over_period, aes(x = Period, y = total_media_spend, fill = Product)) +
  geom_area(alpha = 0.6) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Sales Over Period", 
       subtitle = "Alpha V5 | Nomad | Soft Fusion", 
       x = "", y = "Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create wide data set
media_data_spread = spread(media_data, VariableName, VariableValue)
glimpse(alphaV5_media_data_spread)

  # Cleaning column names using library(janitor)
media_data_spread = clean_names(media_data_spread) 


# Soft Fusion data set cleaning
  # Create wide data set
softFusion_media_data_spread = media_data_spread %>% 
  filter(product == "Soft Fusion")
glimpse(softFusion_media_data_spread)

  # Check for columns with all NA values
summary(is.na(softFusion_media_data_spread))

  # Removing variables with all NA values
softFusion_media_data_spread = select(softFusion_media_data_spread, "geography", "product", "period", 
                                   "digital_impressions", "digital_spend", "direct_tv_grp", "direct_tv_spend",
                                   "efficiency_tv_grp", "efficiency_tv_spend", "emails_delivered", 
                                   "facebook_impressions", "facebook_spend", "gmail_clicks", "gmail_spend", 
                                   "magazine_impressions", "magazine_spend", "newspaper_impressions", 
                                   "newspaper_spend", "sales", "search_clicks", "search_spend", "seasonality",
                                   "traditional_tv_grp", "traditional_tv_spend", "twitter_impressions", 
                                   "twitter_spend", "weekly_shipment", "you_tube_display_clicks", 
                                   "you_tube_display_spend")


# Alpha V5 data set cleaning
  # Create wide data set
alphaV5_media_data_spread = media_data_spread %>% 
  filter(product == "Alpha v5")
glimpse(alphaV5_media_data_spread)

  # Check for columns with all NA values
summary(is.na(alphaV5_media_data_spread))

  # Removing variables with all NA values
alphaV5_media_data_spread = select(alphaV5_media_data_spread, "geography", "product", "period", 
                                   "digital_impressions", "digital_spend", "emails_delivered", 
                                   "facebook_impressions", "facebook_spend", "out_of_home_impressions", 
                                   "out_of_home_spend", "sales", "search_clicks", "search_spend",
                                   "spot_tv_grp", "spot_tv_spend", "twitter_impressions", "twitter_spend",
                                   "weekly_shipment", "you_tube_display_clicks", "you_tube_display_spend",
                                   "youtube_stream_spend", "youtube_stream_views")


# Nomad data set cleaning
  # Create wide data set
nomad_media_data_spread = media_data_spread %>% 
  filter(product == "Nomad")
glimpse(nomad_media_data_spread)

  # Check for columns with all NA values
summary(is.na(nomad_media_data_spread))

  # Removing variables with all NA values
nomad_media_data_spread = select(nomad_media_data_spread, "geography", "product", "period", 
                                   "digital_impressions", "digital_spend", "emails_delivered", 
                                   "facebook_impressions", "facebook_spend", "gmail_clicks", "gmail_spend",
                                   "magazine_impressions", "magazine_spend", "online_launch", 
                                   "out_of_home_impressions", "out_of_home_spend", "pinterest_impressions", 
                                   "pinterest_spend", "sales", "search_clicks", "search_spend", "spot_tv_grp", 
                                   "spot_tv_spend", "weekly_shipment", "you_tube_display_clicks", 
                                   "you_tube_display_spend")


# Create data set grouped by period / weeks
  # Soft Fusion
softFusion_media_data_spread_group_by_period = softFusion_media_data_spread %>% 
  group_by(period) %>% 
  summarise_at(.vars = vars(digital_impressions:you_tube_display_spend),
               .funs = c("sum"))
dim(softFusion_media_data_spread_group_by_period)

  # Alpha V5
colnames(alphaV5_media_data_spread)
alphaV5_media_data_spread_group_by_period = alphaV5_media_data_spread %>% 
  group_by(period) %>% 
  summarise_at(.vars = vars(digital_impressions:youtube_stream_views),
               .funs = c("sum"))
glimpse(alphaV5_media_data_spread_group_by_period)

  # Nomad
colnames(nomad_media_data_spread)
nomad_media_data_spread_group_by_period = nomad_media_data_spread %>% 
  group_by(period) %>% 
  summarise_at(.vars = vars(digital_impressions:you_tube_display_spend),
               .funs = c("sum"))
glimpse(nomad_media_data_spread_group_by_period)

  # Weather Data
weather_data_group_by_weeks = weather_data %>% 
  group_by(weeks) %>% 
  summarise_at(.vars = vars(temprature_max:snow),
               .funs = c("mean"))
glimpse(weather_data_group_by_weeks)

  # Decreasing date by 1 day to match against the period in media data
weather_data_group_by_weeks$weeks = weather_data_group_by_weeks$weeks-1

  # Mutate temp min & temp max into avg temp column
weather_data_group_by_weeks = weather_data_group_by_weeks %>%  
  mutate(avg_temprature = (temprature_max + temprature_min) / 2)

  # Unemployment Data
unemployment_data_group_by_Period = unemployment_data %>% 
  group_by(Period) %>% 
  summarise(avg_unemployment_rate = mean(Unemployment.Rate))
  
  # Formating the date
  # extracting date from date and time using as.Date() function
unemployment_data_group_by_Period$Period = as.Date(unemployment_data_group_by_Period$Period) 
head(unemployment_data_group_by_Period)

  # Decreasing date by 1 day to mat
unemployment_data_group_by_Period$Period = unemployment_data_group_by_Period$Period-1
glimpse(unemployment_data_group_by_Period)

# Merge media data, weather data & unemployment data
  # Soft Fusion
softFusion_media_data_spread_group_by_period_merged_data = data.frame(softFusion_media_data_spread_group_by_period, 
                         weather_data_group_by_weeks, 
                         unemployment_data_group_by_Period)
glimpse(softFusion_media_data_spread_group_by_period_merged_data)

softFusion_media_data_spread_group_by_period_merged_data = 
  select(softFusion_media_data_spread_group_by_period_merged_data, -c(28, 34))

  # Alpha V5
alphaV5_media_data_spread_group_by_period_merged_data_test_1 =
  left_join(alphaV5_media_data_spread_group_by_period, 
            weather_data_group_by_weeks, # joining weather data 
            by = c("period" = "weeks"))

alphaV5_media_data_spread_group_by_period_merged_data = 
  left_join(alphaV5_media_data_spread_group_by_period_merged_data_test_1, 
            unemployment_data_group_by_Period, # joining unemployment data
            by = c("period" = "Period"))

  # Nomad
nomad_media_data_spread_group_by_period_merged_data_test_1 = 
  left_join(nomad_media_data_spread_group_by_period, 
          weather_data_group_by_weeks, # joining weather data
          by = c("period" = "weeks"))
View(nomad_media_data_spread_group_by_period_merged_data)

nomad_media_data_spread_group_by_period_merged_data = 
  left_join(nomad_media_data_spread_group_by_period_merged_data_test_1, 
            unemployment_data_group_by_Period, # joining weather data
            by = c("period" = "Period"))
glimpse(nomad_media_data_spread_group_by_period_merged_data)

# Coorelation Matrix
  # Soft Fusion
  # Removing not required variables
colnames(alphaV5_media_data_spread_group_by_period_merged_data)
softFusion_media_data_spread_group_by_period_merged_data_correlation = 
  select(softFusion_media_data_spread_group_by_period_merged_data, -c(1,2,4,5,6,8,9,11,13,14,15,16,18,21,23,
                                                                      25,26,28,29))
  # Ploting correlation
corrplot(cor(softFusion_media_data_spread_group_by_period_merged_data_correlation),
         method = 'square',
         type = 'lower',
         diag = F,
         tl.col = "black")

  # Alpha V5
  # Removing not required variables
alphaV5_media_data_spread_group_by_period_merged_data_correlation =
  select(alphaV5_media_data_spread_group_by_period_merged_data, -c(1,2,4,5,7,10,12,14,16,17,20,21,22))

  # Ploting correlation
corrplot(cor(alphaV5_media_data_spread_group_by_period_merged_data_correlation, use = "complete.obs"),
         method = 'square',
         type = 'lower',
         diag = F,
         tl.col = "black")

  # Nomad
  # Removing not required variables
colnames(nomad_media_data_spread_group_by_period_merged_data)
nomad_media_data_spread_group_by_period_merged_data_correlation = 
  select(nomad_media_data_spread_group_by_period_merged_data, -c(1,2,4,5,7,9,11,12,13,14,17,19,20,21,22,
                                                                 24,25))

  # Ploting correlation
corrplot(cor(nomad_media_data_spread_group_by_period_merged_data_correlation),
         method = 'square',
         type = 'lower',
         diag = F,
         tl.col = "black")

# Adding units sold column
  # Soft Fusion
softFusion_media_data_spread_group_by_period_merged_data =
  softFusion_media_data_spread_group_by_period_merged_data %>% 
  mutate(units_sold = sales / 150) # soft fusion price

  # Alpha V5
alphaV5_media_data_spread_group_by_period_merged_data = 
  alphaV5_media_data_spread_group_by_period_merged_data %>%
  mutate(units_sold = sales / 500) # alpha v5 price

  # Nomad
nomad_media_data_spread_group_by_period_merged_data = 
  nomad_media_data_spread_group_by_period_merged_data %>% 
  mutate(units_sold = sales / 250) # nomad price 

# Visualising units sold by product
  # Create sum of units sold
softFusion_units_sold = sum(softFusion_media_data_spread_group_by_period_merged_data$units_sold)
alphaV5_units_sold = sum(alphaV5_media_data_spread_group_by_period_merged_data$units_sold)
nomad_units_sold = sum(nomad_media_data_spread_group_by_period_merged_data$units_sold)
  
  # Create data frame of units sold 
units_sold_by_product = data.frame(products = c("Soft Fusion", "Alpha V5", "Nomad"),
                                   units_sold = c(11989366, 814890, 1158804))

  # Ploting units sold by products
ggplot(units_sold_by_product, aes(x = products, y = units_sold, fill = products)) +
  geom_col(alpha = 0.7, width = 0.7) +
  geom_text(aes(label = number(units_sold)), vjust = -0.8) +
  labs(title = "Total Units Sold By Product", x = "", y = "Units Sold") +
  theme_minimal()


# Soft Fusion Model
  # Model 1
softFusion_media_data_spread_group_by_period_merged_data_model_1 = 
  lm(units_sold ~ digital_spend + efficiency_tv_spend + facebook_spend + gmail_spend + search_spend + 
       twitter_spend + you_tube_display_spend + temprature_max + temprature_min + precipitation + 
       snow + avg_unemployment_rate, 
     data = softFusion_media_data_spread_group_by_period_merged_data,
     na.action = na.exclude)
summary(softFusion_media_data_spread_group_by_period_merged_data_model_1)
  # check for vif
vif(softFusion_media_data_spread_group_by_period_merged_data_model_1)


  # Model 2
  # after checking for vif, temp max and temp min variable had high coolinearity
softFusion_media_data_spread_group_by_period_merged_data_model_2 = 
  lm(units_sold ~ digital_spend + efficiency_tv_spend + facebook_spend + gmail_spend + search_spend + 
       twitter_spend + you_tube_display_spend + avg_temprature + # using avg_temprature variable instead of temp min & temp max
       precipitation + snow + avg_unemployment_rate, 
     data = softFusion_media_data_spread_group_by_period_merged_data,
     na.action = na.exclude)
summary(softFusion_media_data_spread_group_by_period_merged_data_model_2)
vif(softFusion_media_data_spread_group_by_period_merged_data_model_2)

  # Model 3 - Lagged effect
colnames(softFusion_media_data_spread_group_by_period_merged_data_lagged_variable)
  # creating lagged variable
softFusion_media_data_spread_group_by_period_merged_data_lagged_variable = 
  softFusion_media_data_spread_group_by_period_merged_data %>% 
  mutate(lag_digital_spend = lag(digital_spend), lag_efficiency_tv_spend = lag(efficiency_tv_spend),
         lag_you_tube_display_spend = lag(you_tube_display_spend))

  # Use lagged variable on model 3
softFusion_media_data_spread_group_by_period_merged_data_model_3 = 
  lm(units_sold ~ lag_digital_spend + lag_efficiency_tv_spend + facebook_spend + gmail_spend + search_spend + 
       twitter_spend + lag_you_tube_display_spend + avg_temprature +
       precipitation + snow + avg_unemployment_rate, 
     data = softFusion_media_data_spread_group_by_period_merged_data_lagged_variable,
     na.action = na.exclude)
  # Use model 3 in ppt  
summary(softFusion_media_data_spread_group_by_period_merged_data_model_3)
vif(softFusion_media_data_spread_group_by_period_merged_data_model_3)

par(mfrow =c(2,4))
plot(softFusion_media_data_spread_group_by_period_merged_data_model_2)
plot(softFusion_media_data_spread_group_by_period_merged_data_model_3)
  # 1- Residual vs Fitted: is a simple scatter plot between residuals and predicted values. It should look
    # more random or less random. Model 3 is more random than model 2
  # 2- Normal Q-Q: is a normal probability plot. It will give straight line if the errors are distributed 
    # normally. Point 46,51 & 103 are deviate from straight line in model 2 and point 48,51 & 103 
    # deviate from straight line in model 3 but less deviates less compared to model 2
  # 3- Scale Location: like residuals vs fitted, it should look random and have no particular pattern
    # model 3 is more random than model 2
  # 4- Residuals vs Leverage: also known as cooks distance plot tells us which points have the greates 
    # influence on the regression (leverage line). Point 

  # Model 4 - Quadratic model
colnames(softFusion_media_data_spread_group_by_period_merged_data_lagged_variable)
# creating lagged variable
softFusion_media_data_spread_group_by_period_merged_data_quadratic_variable = 
  softFusion_media_data_spread_group_by_period_merged_data %>% 
  mutate(quadratic_digital_spend = digital_spend^2, 
         quadratic_efficiency_tv_spend = efficiency_tv_spend^2,
         quadratic_you_tube_display_spend = you_tube_display_spend^2, 
         quadratic_facebook_spend = facebook_spend^2,
         quadratic_gmail_spend = gmail_spend^2, 
         quadratic_search_spend = search_spend^2, 
         quadratic_twitter_spend = twitter_spend^2)

# Use lagged variable on model 4
softFusion_media_data_spread_group_by_period_merged_data_model_4 = 
  lm(units_sold ~ quadratic_digital_spend + quadratic_efficiency_tv_spend + quadratic_facebook_spend + quadratic_gmail_spend + quadratic_search_spend + 
       quadratic_twitter_spend + quadratic_you_tube_display_spend + avg_temprature +
       precipitation + snow + avg_unemployment_rate, 
     data = softFusion_media_data_spread_group_by_period_merged_data_quadratic_variable,
     na.action = na.exclude)
summary(softFusion_media_data_spread_group_by_period_merged_data_model_4)
vif(softFusion_media_data_spread_group_by_period_merged_data_model_4)
# comparing plot of model 3 & 4 similarly as done for model 2 & 3, model 3 seems to fit better 

  # Saving predicted values using model 3
softFusion_media_data_spread_group_by_period_merged_data_lagged_variable$pred_units_sold =
  predict(softFusion_media_data_spread_group_by_period_merged_data_model_3)
sum(softFusion_media_data_spread_group_by_period_merged_data_lagged_variable$units_sold)
  # Saving residuals using model 3
softFusion_media_data_spread_group_by_period_merged_data_lagged_variable$residuals =
  residuals(softFusion_media_data_spread_group_by_period_merged_data_model_3)

  # Visualising predicted sales of model 3 againsts actual sales - Soft Fusion 
ggplot(softFusion_media_data_spread_group_by_period_merged_data_lagged_variable,
       aes(x = period, y = units_sold)) +
  geom_segment(aes(xend = period, yend = pred_units_sold), alpha = 0.2) +
  geom_point(aes(color = residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = F) +
  geom_point(aes(y = pred_units_sold), shape = 1) +
  scale_y_log10() +
  labs(title = "Predicted sales for Soft Fusion", x = "Period", y = "Sales") +
  theme_bw()

# Alpha V5 Model
  # Model 1
summary(is.na(alphaV5_media_data_spread_group_by_period_merged_data))
alphaV5_media_data_spread_group_by_period_merged_data_model_1 = 
  lm(units_sold ~ digital_spend + facebook_spend + out_of_home_spend + search_spend + spot_tv_spend +
       twitter_spend + you_tube_display_spend + youtube_stream_spend + avg_temprature +
       precipitation + snow + avg_unemployment_rate, 
     data = alphaV5_media_data_spread_group_by_period_merged_data,
     na.action = na.exclude)
summary(alphaV5_media_data_spread_group_by_period_merged_data_model_1)
vif(alphaV5_media_data_spread_group_by_period_merged_data_model_1)

  # Model 2 - Lagged effect
  # creating lagged variable
alphaV5_media_data_spread_group_by_period_merged_data_lagged_variable = 
  alphaV5_media_data_spread_group_by_period_merged_data %>% 
  mutate(lag_digital_spend = lag(digital_spend), lag_out_of_home_spend = lag(out_of_home_spend),
         lag_spot_tv_spend = lag(spot_tv_spend), lag_you_tube_display_spend = lag(you_tube_display_spend),
         lag_youtube_stream_spend = lag(youtube_stream_spend))

  # Using lagged variable in model 2
alphaV5_media_data_spread_group_by_period_merged_data_model_2 = 
  lm(units_sold ~ lag_digital_spend + facebook_spend + lag_out_of_home_spend + search_spend + 
       lag_spot_tv_spend + twitter_spend + lag_you_tube_display_spend + lag_youtube_stream_spend + 
       avg_temprature + precipitation + snow + avg_unemployment_rate, 
     data = alphaV5_media_data_spread_group_by_period_merged_data_lagged_variable,
     na.action = na.exclude)
  # Use model 2 in ppt
summary(alphaV5_media_data_spread_group_by_period_merged_data_model_2) # use this in ppt
vif(alphaV5_media_data_spread_group_by_period_merged_data_model_2)

  # Save predicted units sold vaue using model 2
alphaV5_media_data_spread_group_by_period_merged_data_lagged_variable$pred_units_sold = 
  predict(alphaV5_media_data_spread_group_by_period_merged_data_model_2)

  # Save residuals using model 2
alphaV5_media_data_spread_group_by_period_merged_data_lagged_variable$residuals = 
  residuals(alphaV5_media_data_spread_group_by_period_merged_data_model_2)

# Visualising predicted sales of model 3 againsts actual sales - Soft Fusion 
ggplot(alphaV5_media_data_spread_group_by_period_merged_data_lagged_variable,
       aes(x = period, y = units_sold)) +
  geom_segment(aes(xend = period, yend = pred_units_sold), alpha = 0.2) +
  geom_point(aes(color = residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = F) +
  geom_point(aes(y = pred_units_sold), shape = 1) +
  scale_y_log10() +
  labs(title = "Predicted sales for Alpha V5", x = "Period", y = "Sales") +
  theme_bw()
  

# Nomad Model
  # Model 1
summary(nomad_media_data_spread_group_by_period_merged_data)
nomad_media_data_spread_group_by_period_merged_data_model_1 = 
  lm(units_sold ~ digital_spend + facebook_spend + gmail_spend + magazine_spend + pinterest_spend + 
       search_spend + you_tube_display_spend + avg_temprature + precipitation + 
       snow + avg_unemployment_rate, 
     data = nomad_media_data_spread_group_by_period_merged_data,
     na.action = na.exclude)
summary(nomad_media_data_spread_group_by_period_merged_data_model_1)
vif(nomad_media_data_spread_group_by_period_merged_data_model_1)

par(mfrow =c(2,2))
plot(nomad_media_data_spread_group_by_period_merged_data_model_1)

plot(nomad_media_data_spread_group_by_period_merged_data_model_1)
  # 1- Residual vs Fitted: is a simple scatter plot between residuals and predicted values. It should look more or less random
  # 2- Normal Q-Q: is a normal probability plot. It will give straight line if the errors are distributed normally. But point 69,64 & 68 deviate from the straight line
  # 3- Scale - Location: like residuals vs fitted should look random. No patterns. Ours kind of have a upward slope pattern
  # 4- Residuals VS Leverage: also known as Cooks Distance plot tells us which points have the gretest influence on the regression (leverage point). Point 68,64,69 has the greatest influence on the regression line

  # using ggpairs() function fro library to see the correlation between all variables
ggpairs(nomad_media_data_spread_group_by_period_merged_data,
        columns = 27:30) # only usefule when seen by indicating columns

  # Model 2
colnames(nomad_media_data_spread_group_by_period_merged_data)
nomad_media_data_spread_group_by_period_merged_data_lagged_variable = 
  nomad_media_data_spread_group_by_period_merged_data %>% 
  mutate(lag_digital_spend = lag(digital_spend), lag_facebook_spend = lag(facebook_spend), 
         lag_magazine_spend = lag(magazine_spend), lag_you_tube_display_spend = lag(you_tube_display_spend))

  # Use lagged variable in model 2
nomad_media_data_spread_group_by_period_merged_data_model_2 = 
  lm(units_sold ~ lag_digital_spend + lag_facebook_spend + gmail_spend + lag_magazine_spend + 
       pinterest_spend + search_spend + lag_you_tube_display_spend + avg_temprature + precipitation + 
       snow + avg_unemployment_rate, 
     data = nomad_media_data_spread_group_by_period_merged_data_lagged_variable,
     na.action = na.exclude)
summary(nomad_media_data_spread_group_by_period_merged_data_model_2)
vif(nomad_media_data_spread_group_by_period_merged_data_model_2)
  
  # Saving predicted sales value
nomad_media_data_spread_group_by_period_merged_data_lagged_variable$pred_units_sold =
  predict(nomad_media_data_spread_group_by_period_merged_data_model_2)

  # Saving residuals
nomad_media_data_spread_group_by_period_merged_data_lagged_variable$residuals =
  residuals(nomad_media_data_spread_group_by_period_merged_data_model_2)

  # Visualising predictes units sales - Nomad
ggplot(nomad_media_data_spread_group_by_period_merged_data_lagged_variable,
       aes(x = period, y = units_sold)) +
  geom_segment(aes(xend = period, yend = pred_units_sold), alpha = 0.2) +
  geom_point(aes(color = residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = F) +
  geom_point(aes(y = pred_units_sold), shape = 1) +
  scale_y_log10() +
  labs(title = "Predicted Sales For Nomad", x = "Period", y = "Sales") +
  theme_bw()


  


