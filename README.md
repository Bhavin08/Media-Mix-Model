# Media-Mix-Model

The aim of the project is to build a learn and visualize the relationship between medai spend variables and other control factors such as weather data and unemployment data to Sales, and build media mix model.


I started out with loading the data set and changing the date variable format from factor to POSIXct and Date. Filter out data by product and visualized sales and media spend over period using ggplot() and cleaned the graph further using various functions such as alpha and width within geom_col(), geom_text() to label bars with total sales in dollar value, scale_y_continuous to expand y axis into values, labs() to title graph, x axis and y axis, and theme_minimal to give a clear and minimal background to graph
