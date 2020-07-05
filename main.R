library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(e1071)
library(gbm)
library(data.table)
library(tictoc)

sales_data = fread("data/sales_train.csv")
item_data = fread("data/items.csv")
test_data = fread("data/test.csv")
toc()

glimpse(sales_data)
glimpse(item_data)
glimpse(test_data)

sales_data = merge(sales_data, item_data[,c("item_id", "item_category_id")], by = "item_id", all.x = T)
sales_data$date = as.Date(sales_data$date, "%d.%m.%Y")

sales_data$year = year(sales_data$date)
sales_data$year =  as.factor(sales_data$year)

sales_data$month = month(sales_data$date)
sales_data$month = as.factor(sales_data$month)

sales_data$day = day(sales_data$date)
sales_data$day = as.factor(sales_data$day)

sales_data$weekdays =  weekdays(sales_data$date)
sales_data$weekdays = as.factor(sales_data$weekdays)

# sales_data$shop_id = as.factor(sales_data$shop_id)
# sales_data$item_id =  as.factor(sales_data$item_id)
sales_data$item_category_id =  as.factor(sales_data$item_category_id)


### EDA ####
# sales shop wise
sales_shopwise = sales_data %>%
    select(shop_id, item_cnt_day) %>%
    group_by(shop_id) %>%
    summarise(item_cnt_day =  sum(item_cnt_day, na.rm = T))

ggplot(data =  sales_shopwise, 
       mapping = aes(x = reorder(shop_id, item_cnt_day), 
                     y = item_cnt_day, 
                     fill = factor(shop_id))) +
    geom_histogram(stat = "identity", color = "yellow") +
    # coord_flip() +
    xlab("Shop ID") + ylab("Sales Count")+
    # geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "center", aes(label = item_cnt_day)) +
    ggtitle(label = "Shop wise sales") +
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 



# sales item category wise
sales_categorywise = sales_data %>%
    select(item_category_id, item_cnt_day) %>%
    group_by(item_category_id) %>%
    summarise(item_cnt_day =  sum(item_cnt_day, na.rm = T))

ggplot(data =  sales_categorywise, 
       mapping = aes(x = reorder(item_category_id,item_cnt_day), 
                     y = item_cnt_day,
                     fill = factor(item_category_id))) +
    geom_histogram(stat = "identity", color = "yellow") +
    # coord_flip() +
    xlab("Item Category") + ylab("Sales Count") +
    ggtitle("Sale Item Category wise")+
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 


# most items in shop

items_in_shop = sales_data %>%
    select(shop_id, item_id) %>%
    group_by(shop_id) %>%
    summarise(item_id = n_distinct(item_id))

ggplot(data = items_in_shop,
       mapping = aes(x = reorder(shop_id,item_id),
                     y = item_id,
                     fill = factor(shop_id)))+
    geom_histogram(stat = "identity", color = "yellow") +
    xlab(" Shop ID")+ ylab(" Items in shop")+
    ggtitle("Most Items in Shops") +
    coord_flip()+
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 


# which catefgory of item is available most 
items_in_category = sales_data %>%
    select(item_category_id, item_id) %>%
    group_by(item_category_id) %>%
    summarise(item_id =  n_distinct(item_id))

ggplot(data = items_in_category,
       mapping = aes(x = reorder(item_category_id,item_id),
                     y = item_id,
                     fill = factor(item_category_id)))+
    geom_histogram(stat = "identity", color = "yellow") +
    xlab(" Category ID")+ ylab(" Items in Category")+
    ggtitle("Most Items per Category") +
    coord_flip()+
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 



# which item is most popular and most sold in the each shop 

popularity  =  sales_data %>%
    group_by(shop_id, item_id) %>%
    summarise(sold_item_count = sum(item_cnt_day)) %>%
    # filter(sold_item_count == max(sold_item_count)) %>%
    arrange(desc(sold_item_count))

popular_items_in_shop  =  sales_data %>%
    group_by(shop_id, item_id) %>%
    summarise(sold_item_count = sum(item_cnt_day)) %>%
    filter(sold_item_count == max(sold_item_count)) %>%
    arrange(desc(sold_item_count))

ggplot(data = popular_items_in_shop,
       mapping = aes(x = reorder(shop_id, sold_item_count),
                     y = sold_item_count,
                     fill = factor(item_id))) +
    geom_histogram(stat = "identity", color = "yellow") +
    xlab("") + ylab("Sales Count") +
    ggtitle("Most Popular Item per shop") +
    coord_flip()+
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 



# which shop has most category of items 
shop_with_most_category = sales_data %>%
    select(shop_id, item_category_id) %>%
    group_by(shop_id) %>%
    summarise(category_count =  n_distinct(item_category_id)) %>%
    arrange(desc(category_count))

ggplot(data = shop_with_most_category,
       mapping = aes(x = reorder(shop_id, category_count),
                     y = category_count,
                     fill = factor(shop_id))) +
    geom_histogram(stat = "identity", color = "yellow") +
    xlab("Shop ID") + ylab("Item Category Count") +
    ggtitle("Most Item category per shop") +
    coord_flip()+
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 


# which item category is most popular and most sold in each shop
popular_category =  sales_data %>%
    group_by(shop_id, item_category_id) %>%
    summarise(category_count = sum(item_cnt_day)) %>%
    filter(category_count == max(category_count)) %>%
    arrange(desc(category_count))

ggplot(data =  popular_category, 
       mapping = aes(x = reorder(shop_id, category_count),
                     y =  category_count,
                     fill = factor(item_category_id))) +
    geom_histogram(stat ="identity", color = "yellow") +
    xlab("Shop ID")+ ylab("Category per shop") + 
    ggtitle("Most popular item category per shop")+
    coord_flip()+
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 



# which item category is highest sales grossing in all shops
most_grossing_category = sales_data %>%
    group_by(item_category_id) %>%
    summarise(total_gross = sum(item_cnt_day * item_price)) %>%
    arrange(desc(total_gross))

ggplot(most_grossing_category, 
       aes(x = reorder(item_category_id, total_gross),
           y = total_gross,
           fill = factor(item_category_id))) +
    geom_histogram(stat = "identity", color = "yellow") +
    xlab("Category ID") + ylab("Total Gross")+
    ggtitle("Total Gross per Item category") +
    coord_flip()+
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 




# item categories available in each shop 
item_category_in_shops = sales_data %>%
    group_by(shop_id) %>%
    summarise(item_category =  paste(sort(unique(item_category_id)), collapse = ", ")) 

head(item_category_in_shops)

# which item gets sold  the most under which category 

most_sold_item_per_category = sales_data %>%
    group_by(item_category_id, item_id) %>%
    summarise(total_sales = sum(item_price * item_cnt_day)) %>%
    filter(total_sales == max(total_sales)) %>%
    arrange(desc(total_sales))


ggplot(most_sold_item_per_category,
       aes(x = reorder(item_category_id, total_sales), 
           y = total_sales,
           fill = factor(item_id))) +
    geom_histogram(stat = "identity", color = "yellow") +
    labs(title = "Items sold per category",x = "Category ID", y = "Sales", fill = "Item ID") +
    coord_flip()+ 
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    )


# day and month wise total sales 
month_daywise_total_sales =  sales_data %>%
    group_by(month, day) %>%
    summarise(total_sales =  sum(item_price * item_cnt_day))

ggplot(month_daywise_total_sales, 
       aes(x = day, 
           y = total_sales, 
           group =  month, 
           color =  factor(month))) +
    geom_line() + 
    geom_point() +
    labs(title = "Total Sales month-day wise", x = "Days", y = "Total sales", fill = "Months") +
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 



ggplot(month_daywise_total_sales, 
       aes(x = day, 
           y = total_sales, 
           fill =  factor(day))) +
    geom_histogram(stat = "identity", color = "yellow") +
    labs(title = "Total Sales month-day wise", x = "Days", y = "Total sales", fill = "Days") +
    facet_wrap(~month, ncol = 2)+
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 



# year wise total sales
yearly_sales = sales_data %>%
    group_by(year) %>%
    summarise(yearly_sale = sum(item_price * item_cnt_day))

ggplot(yearly_sales, aes(x =  year, y = yearly_sale, fill =  factor(year)))+
    geom_histogram(stat = "identity", color = "yellow")+
    labs(title = "Yearly Sales", x = "Year", y = "Total Sale", fill = "Year")+
    geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "center", aes(label = yearly_sale)) +
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 


# year and month wise total sales 
ym_sales = sales_data %>%
    group_by(year, month) %>%
    summarise(ym_sale = sum(item_price*item_cnt_day)) %>%
    arrange(year)

ym_sales$ym_sale = round(ym_sales$ym_sale, 2)
ggplot(ym_sales, aes(x =  month, y = ym_sale, fill =  factor(year)))+
    geom_histogram(stat = "identity", color = "yellow") +
    labs(title = "Yearly-Monthly Sales", x = "Months", y =  "Total sales", fill = "Year")+
    geom_label(stat = "identity",position = position_stack(),hjust = "center",vjust = 0.5, aes(label = ym_sale))+
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 


ggplot(ym_sales, aes(x =  month, y = ym_sale, fill =  factor(year)))+
    geom_histogram(stat = "identity", position = "dodge", color = "yellow") +
    labs(title = "Yearly-Monthly sales", x = "Months", y =  "Total sales", fill = "Year")+
    # geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "center", aes(label = ym_sale)) +
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 

# number of items sold each day 
daily_sale = sales_data %>%
    group_by(date) %>%
    summarise(items_sold =  sum(item_cnt_day))

ggplot(daily_sale, aes(x =  date, y = items_sold, color =  items_sold)) +
    geom_line() +
    geom_point()+
    labs(title = "Daily Item sold", x =  "Date", y = "Items sold")+
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 



# items sold on weekdays 
weekdays_item_sold = sales_data %>%
    group_by(weekdays) %>%
    summarise(item_sold = sum(item_cnt_day)) %>%
    arrange(desc(item_sold))

ggplot(weekdays_item_sold, aes(x =reorder(weekdays, item_sold), y =  item_sold, fill = factor(weekdays)))+
    geom_bar(stat = "identity", color = "yellow") +
    labs(title = "Items sold on weekdays", x = "Week Days", y =  "Items sold", fill = "Week Days") +
    geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "center", aes(label = item_sold)) +
    coord_flip()+
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 


# sales revenue on weekdays
weekdays_sales = sales_data %>%
    group_by(weekdays) %>%
    summarise(total_sale = sum(item_cnt_day * item_price)) %>%
    arrange(desc(total_sale))
weekdays_sales$total_sale = round(weekdays_sales$total_sale, 2)

ggplot(weekdays_sales, aes(x =reorder(weekdays, total_sale), y =  total_sale, fill = factor(weekdays)))+
    geom_bar(stat = "identity", color ="yellow") +
    labs(title = "Sales on weekdays", x = "Week Days", y =  "Items sold", fill = "Week Days") +
    coord_flip()+
    geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "center", aes(label = total_sale)) +
    theme(
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        # Change plot and panel background
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        # legend.position = c(0.6, 0.07),
        # legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        # align title to top center, top ledt is by default.
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        # axis ticks to bold black
        axis.text=element_text(colour = "yellow",face = "bold"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white")
    ) 


## Linear Regression Model ####
linear_model = lm(formula = item_cnt_day ~ shop_id + item_id,
                  data = sales_data) 

result = predict(linear_model, test_data[,c("shop_id","item_id")]) 

submission =  data.frame(ID = test_data$ID,
                         item_cnt_month = result)
head(submission)
#write.csv(submission, file = "sub1.csv", row.names = F)

# GBM Model
library(tictoc)
tic("Time Taken to Run GBM Model ")
gbm_model  =  gbm(item_cnt_day ~ shop_id + item_id,
                  data = sales_data,
                  shrinkage = 0.01,
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 5, 
                  bag.fraction = 0.5,
                  train.fraction = 0.8,
                  # cv.folds = 5,
                  n.cores = -1,
                  verbose = T)

toc()

result2 = predict(gbm_model,newdata = test_data[,c("shop_id","item_id")], n.trees = 1000)

sub2 = data.frame(ID = test_data$ID, 
                  item_cnt_month =  result2)

write.csv(sub2, "submission.csv", row.names = F)
