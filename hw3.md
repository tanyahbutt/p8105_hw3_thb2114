P8105 Homework 3 - Tanya Butt (thb2114)
================

## Problem 1

First, I will load the the Instacart data from the p8105.datasets
library and evaluate its contents.

``` r
data("instacart")
force(instacart)
instacart %>% 
  count(user_id)
```

The Instacart data is an online grocery shopping dataset from 2017. It
includes the following 15 variables: order\_id, product\_id,
add\_to\_cart\_order, reordered, user\_id, eval\_set, order\_number,
order\_dow, order\_hour\_of\_day, days\_since\_prior\_order,
product\_name, aisle\_id, department\_id, aisle, department.

The data has 1384617 observations. Each row is a product from a single
order from 131209 unique individuals.

I will now produce some graphs using the Instacart data.

``` r
instacart %>% 
  count(department, name = "department_id") %>% 
  ggplot(aes(x = department, y = department_id)) +
  geom_point() +
  labs(
    title = "Number of Products Ordered by Department ",
    x = "Department",
    y = "Number of Prodcuts Ordered",
    caption =  "Data from Instacart"
  ) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = scales::comma)
```

<img src="hw3_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

The departments most frequently ordered from in this dataset set are
“produce” and “dairy eggs”.

``` r
instacart %>% 
  group_by(department, product_name) %>% 
  summarize(n_obs = n()) %>% 
  filter(n_obs > 5000) %>% 
  ggplot(aes(x = product_name, y = n_obs)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Top 10 Products Ordered in the Instacart Dataset",
    x = "Product Name",
    y = "Number of Prodcuts Ordered",
    caption =  "Data from Instacart"
  ) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(
    breaks = c(0, 5000, 10000, 15000, 20000),
    labels = c("0", "5,000", "10,000", "15,000", "20,000")) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))
## `summarise()` has grouped output by 'department'. You can override using the `.groups` argument.
```

<img src="hw3_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

The top 10 most frequently ordered products in the Instacart dataset are
bags of organic bananas, bananas, large lemons, limes, organic avocados,
organic baby spinach, organic Has avocados, organic raspberries, organic
strawberries, and strawberries.

``` r
n_distinct(pull(instacart, aisle_id))
## [1] 134
instacart %>% 
  count(aisle_id, aisle, sort = TRUE)
## # A tibble: 134 × 3
##    aisle_id aisle                              n
##       <int> <chr>                          <int>
##  1       83 fresh vegetables              150609
##  2       24 fresh fruits                  150473
##  3      123 packaged vegetables fruits     78493
##  4      120 yogurt                         55240
##  5       21 packaged cheese                41699
##  6      115 water seltzer sparkling water  36617
##  7       84 milk                           32644
##  8      107 chips pretzels                 31269
##  9       91 soy lactosefree                26240
## 10      112 bread                          23635
## # … with 124 more rows
```

In the Instacart dataset, the number of aisles are 134 and the aisles
most ordered from are the ‘fresh vegetables’ and ‘fresh fruits’ aisle.

Below is a bar graph of the number of items ordered in aisles with &gt;
10,000 items ordered. ‘Fresh vegetables’ and ‘fresh fruits’ had the
highest number of orders in this dataset.

``` r
instacart %>% 
  count(aisle_id, aisle, sort = TRUE) %>% 
  filter(n > 10000) %>% 
  ggplot(aes(x = aisle, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Number of Products Ordered",
    x = "Aisle Name",
    y = "Number of Prodcuts Ordered",
    caption =  "Data from Instacart"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.05, size = 8)) +
  scale_y_continuous(
    breaks = c(0, 10000, 20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000),
    labels = c("0", "10,000", "20,000", "40,000", "60,000", "80,000", "100,000", "120,000", "140,000", "160,000")) 
```

<img src="hw3_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

I will now make a table showing the three most popular items in each of
the aisles, “baking ingredients”, “dog food care”, and “packaged
vegetable fruits.” I will include the number of times each item is
ordered in the table.

``` r
baking_df = filter(instacart, aisle == "baking ingredients") %>% 
  group_by(aisle_id, aisle, product_name) %>% 
  count(product_name, sort = TRUE) %>% 
  filter(product_name == "Light Brown Sugar" | product_name == "Pure Baking Soda" |
         product_name == "Cane Sugar") 
  

dogfood_df = filter(instacart, aisle == "dog food care") %>% 
  group_by(aisle_id, aisle, product_name) %>% 
  count(product_name, sort = TRUE) %>% 
  filter(product_name == "Snack Sticks Chicken & Rice Recipe Dog Treats" | product_name == "Organix Chicken & Brown Rice Recipe" | product_name == "Small Dog Biscuits")

package_vege_fruit_df = filter(instacart, aisle == "packaged vegetables fruits") %>% 
  group_by(aisle_id, aisle, product_name) %>% 
  count(product_name, sort = TRUE) %>% 
  filter(product_name == "Organic Baby Spinach" | product_name == "Organic Raspberries" |
           product_name == "Organic Blueberries")

package_baking_df = full_join(package_vege_fruit_df, baking_df, by = NULL) 
## Joining, by = c("aisle_id", "aisle", "product_name", "n")
baking_dogfood_package_df = full_join(package_baking_df, dogfood_df, by = NULL) %>% 
  rename("Aisle ID Number" = aisle_id) %>% 
  rename("Name of Product" = product_name) %>% 
  rename("Number of Times Ordered" = n) %>% 
  rename("Aisle Name" = aisle) %>% 
   knitr::kable()
## Joining, by = c("aisle_id", "aisle", "product_name", "n")
baking_dogfood_package_df
```

| Aisle ID Number | Aisle Name                 | Name of Product                               | Number of Times Ordered |
|----------------:|:---------------------------|:----------------------------------------------|------------------------:|
|             123 | packaged vegetables fruits | Organic Baby Spinach                          |                    9784 |
|             123 | packaged vegetables fruits | Organic Raspberries                           |                    5546 |
|             123 | packaged vegetables fruits | Organic Blueberries                           |                    4966 |
|              17 | baking ingredients         | Light Brown Sugar                             |                     499 |
|              17 | baking ingredients         | Pure Baking Soda                              |                     387 |
|              17 | baking ingredients         | Cane Sugar                                    |                     336 |
|              40 | dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |                      30 |
|              40 | dog food care              | Organix Chicken & Brown Rice Recipe           |                      28 |
|              40 | dog food care              | Small Dog Biscuits                            |                      26 |

I will now make a table showing the mean hour of the day at which Pink
Lady Apples and Coffee Ice Cream are ordered on each day of the week.

``` r
PLA_CIC_df = filter(instacart, product_name == "Pink Lady Apples" | product_name == "Coffee Ice Cream") %>% 
  group_by(product_name, order_dow) %>% 
  summarize(mean_order_hour = mean(order_hour_of_day)) %>% 
  rename("Name of Product" = product_name) %>% 
  rename("Mean Order Hour" = mean_order_hour) %>%
  mutate(order_dow = factor(c("0" = "Sunday", "1" = "Monday", "2" = "Tuesday",
                              "3" = "Wednesday", "4" = "Thursday", "5" = "Friday",
                              "6" = "Saturday"))) %>% 
  pivot_wider(
    names_from = "order_dow",
    values_from = "Mean Order Hour"
  ) %>% 
  knitr::kable()
## `summarise()` has grouped output by 'product_name'. You can override using the `.groups` argument.
PLA_CIC_df        
```

| Name of Product  |   Sunday |   Monday |  Tuesday | Wednesday | Thursday |   Friday | Saturday |
|:-----------------|---------:|---------:|---------:|----------:|---------:|---------:|---------:|
| Coffee Ice Cream | 13.77419 | 14.31579 | 15.38095 |  15.31818 | 15.21739 | 12.26316 | 13.83333 |
| Pink Lady Apples | 13.44118 | 11.36000 | 11.70213 |  14.25000 | 11.55172 | 12.78431 | 11.93750 |
