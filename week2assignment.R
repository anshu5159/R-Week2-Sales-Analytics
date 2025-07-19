#   --------------------|
#   ASSIGNMENT WEEK 2  -|
#   --------------------|
#   Topic : “Sales Analytics Using R”  -|
#   ------------------------------------|


#   Introduction  -|
#   ---------------|
#   Assingnment for the week 2 while being a part of the Data Science internship team at Zeno Talent under the guidance of Aishwarya. The assignment,
#   titled "Sales Analytics Using R" tests data manipulation, cleaning concepts of R programming while using the time series dataset. The assignment
#   includes data cleaning, data extraction, loops, aggregations, and visualizations using ggplot. The dataset used in the assignment is Sales Records
#   dataset downloaded from Kaggle platform.


library(dplyr)
#   dplyr package is used to run the data manipulation functions.
library(lubridate)
#   lubridate package is used to run the timeseries data handling functions which helps align the dates easily.
library(ggplot2)
#   ggplot package is used to visualize different plots.


#   Tasks  -|
#   --------|
#   Data Cleaning (NA HANDLING): Show & count all missing values using is.na() and sum(is.na())  -|
#   ----------------------------------------------------------------------------------------------|
 
sales_data <- read.csv("C:/Users/Asus/Desktop/zeno/weekly assignments/week2/Sales-Records/Sales Records.csv")
#   The code is used to read the csv file from the location provided within the quotes.

head(sales_data)
#   It helps view few data from the dataset from the very beginning.

str(sales_data)
#   This one gives the structure of the dataset which is the different column's data type.

summary(sales_data)
#   This one is used to give the summary of the dataset. The summary includes the mean, median, minimum value,
#   maximum value for every column that is numeric while the length for the character columns.

any(is.na(sales_data))
#   It checks if there is any NA i.e., missing values in the dataset. If present it prints TRUE else FALSE.

sum(is.na(sales_data))
#   This code helps calculate the total number of missing values in the dataset using the sum function.


print("Missing values per column:")
colSums(is.na(sales_data))
#   This one helps present the missing values in the dataset column-wise. The output is presented in the form of sum
#   of all the missing values in each column.


#   Show number of rows before cleaning  -|
#   --------------------------------------|

before_clean <- nrow(sales_data)
print(paste("Number of rows before the data is cleaned:", before_clean))
#   Here the nrow function is used to calculate the number of rows in the dataset which is then printed after storing
#   it in the variable before_clean. It here shows the number of rows before the data is cleaned.


#   Replace NA in Units_Sold and Unit_Price with column mean  -|
#   -----------------------------------------------------------|

sales_data$Units.Sold[is.na(sales_data$Units.Sold)] <- mean(sales_data$Units.Sold, na.rm = TRUE)
#   In this code first the is.na(sales_data$Units.Sold) is used to locate where the in the dataset the value is NA.
#   It then uses the mean of the same column excluding the missing values for which there is na.rm set TRUE to fill
#   the missing value with the mean.

sales_data$Unit.Price[is.na(sales_data$Unit.Price)] <- mean(sales_data$Unit.Price, na.rm = TRUE)
#   Similarly this code here also, first locates where the in the dataset the value is NA. It then uses the mean of
#   the same column excluding the missing values for which there is na.rm set TRUE to fill the missing value with the
#   mean of the column.


#   Drop rows where Revenue is NA  -|
#   --------------------------------|

sales_data <- sales_data[!is.na(sales_data$Total.Revenue), ]
head(sales_data)
#   This code here is used to remove the rows with NA in the Revenue column. The ! used here with is.na is used to
#   reverse the result of is.na. Hence it keeps the data where the Revenue is not NA(after reversing).


#   Show number of rows after cleaning  -|
#   -------------------------------------|

cleaned_data <- nrow(sales_data)
print(paste("Number of rows after the data is cleaned:", cleaned_data))
#   Here the nrow function is used to calculate the number of rows in the dataset which is then printed after storing
#   it in the variable cleaned_data. It here shows the number of rows after the data is cleaned.


#   Date Features with lubridate: Convert Date column to Date type  -|
#   -----------------------------------------------------------------|

sales_data$Order.Date <- mdy(sales_data$Order.Date)
head(sales_data$Order.Date)
#   In this case I have used the parsing technique to convert the Character into Date format.

sales_data$Ship.Date <- mdy(sales_data$Ship.Date)
head(sales_data$Ship.Date)
#   In this case I have used the parsing technique to convert the Character into Date format.

#   Add columns: Year, Month (label), Weekday (label)  -|
#   ----------------------------------------------------|

sales_data$Order_Year <- year(sales_data$Order.Date)
sales_data$Order_Month <- month(sales_data$Order.Date, label = TRUE)
sales_data$Order_Weekday <- wday(sales_data$Order.Date, label = TRUE)
head(sales_data)
#   The above lines shows extracting the year, month, weekday from the date format in the dataset. Also, the label 
#   TRUE is used to add the data in the newly added columns in the form of character as july for month instead of 7.
#   Similarly for the weekdays.


sales_data$Ship_Year <- year(sales_data$Ship.Date)
sales_data$Ship_Month <- month(sales_data$Ship.Date, label = TRUE)
sales_data$Ship_Weekday <- wday(sales_data$Ship.Date, label = TRUE)
head(sales_data)
#   Similarly for the shipping date extracting the year, month, weekday from the date format in the dataset. Again,
#   the label TRUE is used to add the data in the newly added columns in the form of character as july for month
#   instead of 7. Similarly for the weekdays.


#   Find  : Month with highest revenue, Day with lowest average units sold  -|
#   -------------------------------------------------------------------------|

monthly_revenue <- sales_data %>%
  group_by(Ship_Month) %>%
  summarise(TotalRevenue = sum(Total.Revenue)) %>%
  arrange(desc(TotalRevenue))
print("Month with the highest revenue is:")
print(month_revenue[1, ])
#   The code here uses the dplyr functions group_by, summarise, arrange. I have also used the pipe operator here.
#   Wherever the pipe operator is used it takes the output of the previous line of code and uses it as the input for
#   the next line of code. Here, the sales_data output is given to the group_by function to make a group based on
#   the month and the output is fed as input to the summarise function whose output is fed to arrange function where
#   the total revenue is arranged in the descending order. In the print[1,] is used to print the first row only
#   which represents the maximum revenue month.

avg_day_sales <- sales_data %>%
  group_by(Ship_Weekday) %>%
  summarise(Avg_sales = mean(Units.Sold)) %>%
  arrange(Avg_sales)
print("Day with the lowest avg units sold is:")
print(avg_day_sales[1, ])
#   Similarly using the pipe operator to calculate the average weekday sales. Using the arrange function to arrange
#   the weekday sales in the ascending order. In the end printing the weekday which has the lowest average weekday
#   sales.


#   Loops & Conditional Logic: Use a for loop to print monthly revenue totals  -|
#   ----------------------------------------------------------------------------|

months <- unique(sales_data$Ship_Month)
#   This code here is used to select the unique names of the months from the n times repeated names of month in the
#   Month column of the dataset.

for (val in months) {
  total_monthly <- sum(sales_data$Total.Revenue[sales_data$Ship_Month == val], na.rm = TRUE)
  print(paste("Total monthly revenue in", val, "is:", total_monthly))
}
#   Now the code uses the for loop to print the total monthly revenue using the sum function while using the
#   relational operators.


#   Create a Performance column: "High" if Revenue > avg revenue, "Low" otherwise  -|
#   --------------------------------------------------------------------------------|

avg_revenue <- mean(sales_data$Total.Revenue)
print(paste("The average revenue is:", avg_revenue))
sales_data$Performance <- ifelse(sales_data$Total.Revenue > avg_revenue, "High", "Low")
head(sales_data$Performance)
#   Using the mean to calculate the average revenue. Creating a new column named Performance to store the
#   performance of revenue based on the average value. I used the ifelse condition to certify the high and low
#   performance.


#   Grouping, Aggregation & Custom Function: Use aggregate() or dplyr to get (Total revenue by Region,Average  -|
#   units sold by Product, Region with highest average revenue)                                                -|
#   ------------------------------------------------------------------------------------------------------------|

region_revenue <- sales_data %>%
  group_by(Region) %>%
  summarise(total_revenue = sum(Total.Revenue))
print("The total revenue grouped by region is:")
print(region_revenue)
#   Using the dplyr function to group the revenue according to the regions and then calculating the sum.

products_avg <- aggregate(Units.Sold ~ Item.Type, data = sales_data, mean)
print("The Average units sold grouped by Product is:")
print(products_avg)
#   Now here the code uses the basic function like aggregate which uses Units.Sold ~ Item.Type to group the data of
#   units sold based on the type of item or product.

avg_region_revenue <- aggregate(Total.Revenue ~ Region, data = sales_data, mean)
region_high_avg <- avg_region_revenue[which.max(avg_region_revenue$Total.Revenue), ]
print("Region with highest average revenue:")
print(region_high_avg)
#   Earlier used the dplyr functions but here I have used the basic functions. Using aggregate to group revenue
#   based on the regions. Later using the which.max to get the maximum value.


#   Write a function region_report(region_name) – (  Return total revenue,  Top-selling product, Days  -|
#   where revenue > average)                                                                           -|
#   ----------------------------------------------------------------------------------------------------|

average_revenue <- mean(sales_data$Total.Revenue)
#   Calculating the mean or the average revenue.

region_report <- function(region_name) {
  region_data <- sales_data[sales_data$Region == region_name, ]
  total_revenue <- sum(region_data$Total.Revenue)
  product_sales <- aggregate(Units.Sold ~ Item.Type, data = region_data, sum)
  top_product <- product_sales[which.max(product_sales$Units.Sold), ]
  revenue_days_gt <- region_data$Ship.Date[region_data$Total.Revenue > average_revenue]
  
  print(paste("Report for:", region_name))
  print(paste("Total Revenue of",region_name,'is:', total_revenue))
  print("Top selling product is:")
  print(top_product)
  print("Dates where revenue > average are:")
  print(revenue_days_gt)
}
#   Creating a function which takes region name as the argument, uses it to create the region's sales data, total
#   revenue. Total product sales grouped by the type of the product in the region. Also, getting the days where the
#   revenue is greater then the average revenue in the region. In the end, printing all the outputs.

region_report("Asia")
#   Calling the function to generate a report for the region.


#   Visualizations with ggplot2:- Bar Chart: Region vs Revenue  -|
#   -------------------------------------------------------------|

region_revenue <- aggregate(Total.Revenue ~ Region, data = sales_data, sum)
#   Using grouping to remove the later confusion like which revenue to plot for which region.

ggplot(region_revenue, aes(x = Region, y = Total.Revenue)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "Total Revenue grouped by Region", x = "Regions", y = "Total Revenue")
#   Using ggplot package to plot the bar graph for Region vs Revenue.



#   Line Chart: Date vs Revenue  -|
#   ------------------------------|

ggplot(sales_data, aes(x = Ship.Date, y = Total.Revenue)) +
  geom_line(color = "brown") +
  labs("Revenue Trend Over Time", x = "Shipping Dates", y = "Total Revenue")
#   Using ggplot package to plot the line chart for Date vs Revenue.


#   Stacked Bar Chart: Product vs Units Sold (fill = Performance)  -|
#   ----------------------------------------------------------------|

product_perf <- aggregate(Units.Sold ~ Item.Type + Performance, data = sales_data, sum)
ggplot(product_perf, aes(x = Item.Type, y = Units.Sold, fill = Performance)) +
  geom_bar(stat = "identity", col = "red") +
  labs(title = "Units Sold by Product vs Performance", x = "Products", y = "Units Sold")
#   Using ggplot package to plot the stacked bar chart for Product vs Units Sold (fill = Performance).


#   Conclusion  -|
#   -------------|
#   In this Week 2 assignment, I worked on analyzing sales data using R. I have learnt cleaning messy(missing) 
#   data, handle missing values, work with dates, and use loops and conditions to make decisions in code. I also
#   created a custom function and plotted different types of charts for better understanding the data. Overall,
#   this task helped me see how data can be turned into useful insights in a simple and clear way.
