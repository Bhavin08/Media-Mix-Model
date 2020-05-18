# Media-Mix-Model

The aim of the project is to build a learn and visualize the relationship between media spend variables and other control factors such as weather data and unemployment data to Sales, and build media mix model.


I started out with loading the data set and changing the date variable format from factor to POSIXct and Date. Filter out data by product and visualized sales and media spend over period using ggplot() and cleaned the graph further using various functions such as alpha and width within geom_col(), geom_text() to label bars with total sales in dollar value, scale_y_continuous to expand y axis into values, labs() to title graph, x axis and y axis, and theme_minimal to give a clear and minimal background to graph.


Converted tall data set to wide data set for each product data set to be able to build model and get ride of variables with all of its values as NA and then grouped them by weeks.

Cleaned and the weaher and unemployment data and merged it to Media data set for each product
cleaning involved extracting date from weather data set and reducing it by one day to match it with the date in media data set.

For example if date in media data set was 2019/01/15, then in weather data set date was 2019/01/16, i.e; one day ahead hence decreasing it by one day to be able to merge it by date column using left_join().

And for unemployment data, the date was in format YYYY/mm/dd_HH:MM:SS hence, I extracted only the date (YYYY/mm/dd) and reduced it by one day to merge with media data set.

For weather data, it had two columns named temp_min & temp_max hence I added another column using mutate() calculating average of temp_min and temp_max and merged it to media data set to be able to factor it in model.


Built correlation matrix for each product data set using spend variables, weather, and unemployment rate. Later the variables for which the coorelation was very high were removed from regression model to avoid multi collinearity problem.


Added units sold column to each product media data set since using units_sold is better than using sales in $dollar as units sold is constant parameter as compared to revenue which changes in accordance with price.

Built model for each of the product, Model 1 is simply using the varibales directly from the medai data set and built another model with lagged variables to consider the lagged effect of the variables such as out of home spend, YouTube display spend, and TV spend since these variables does continue to give sales even after the weeks.
