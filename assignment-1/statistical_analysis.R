# Suicide Rates Overview 1985 to 2016

## Library imports
install.packages("ggplot2", repos = "http://cran.rstudio.com/")
install.packages("dplyr", repos = "http://cran.rstudio.com/")
install.packages("RColorBrewer", repos = "http://cran.rstudio.com/")
install.packages("maps", repos = "http://cran.rstudio.com/")
install.packages("anchors", repos = "http://cran.rstudio.com/")
install.packages("mapproj", repos = "http://cran.rstudio.com/")

library(ggplot2)
library(dplyr)
library(RColorBrewer) # For better plots
library(maps)
library(mapproj)

## Dataset

### Read csv
dataset <-
  read.csv(
    "./data/master.csv",
    stringsAsFactors = T,
    fileEncoding = "UTF-8-BOM"
  )

### Show dataset table
dataset

### Show summary of the data

summary(dataset)

## Preparation of the data
### Order factor levels for age
dataset$age <-
  factor(
    dataset$age,
    levels = c(
      "5-14 years",
      "15-24 years",
      "25-34 years",
      "35-54 years",
      "55-74 years",
      "75+ years"
    )
  )
dataset$generation <-
  factor(
    dataset$generation,
    levels = c(
      "Generation Z",
      "Millenials",
      "Generation X",
      "Boomers",
      "Silent",
      "G.I. Generation"
    )
  )
dataset$gdp_for_year.... <-
  as.numeric(gsub(",", "", dataset$gdp_for_year....))

### Look for null values

sum(is.na(dataset))

### Remove column HDI.for.year and look for null values. [None]

sum(is.na(dataset %>% select(-starts_with("HDI"))))

### Prepare world map

world_map <- map_data("world")

### See if the countries in our dataset have the same name as the regions in the world map

unique(dataset$country)[!(unique(dataset$country) %in% unique(world_map$region))]

### Change some variables of world to adapt them to some regions of our dataset with different name

old_names = c(
  "USA",
  "UK",
  "North Korea",
  "South Korea",
  "Russia",
  "Cape Verde",
  "Antigua",
  "Barbuda",
  "Saint Kitts",
  "Nevis",
  "Saint Vincent",
  "Grenadines",
  "Trinidad",
  "Tobago"
)
new_names = c(
  "United States",
  "United Kingdom",
  "Republic of Korea",
  "Republic of Korea",
  "Russian Federation",
  "Cabo Verde",
  "Antigua and Barbuda",
  "Antigua and Barbuda",
  "Saint Kitts and Nevis",
  "Saint Kitts and Nevis",
  "Saint Vincent and Grenadines",
  "Saint Vincent and Grenadines",
  "Trinidad and Tobago",
  "Trinidad and Tobago"
)


world_map_adapted = anchors::replace.value(world_map,
                                           "region",
                                           from = old_names,
                                           to = new_names,
                                           verbose = FALSE)

### Map theme

get_map_theme = function() {
  theme(
    text = element_text(family = "Montserrat", color = "#FFFFFF")
    ,
    panel.background = element_rect(fill = "#444444")
    ,
    plot.background = element_rect(fill = "#444444")
    ,
    legend.background = element_rect(fill = "#444444")
    ,
    panel.grid = element_blank()
    ,
    plot.title = element_text(size = 16)
    ,
    axis.text = element_blank()
    ,
    axis.title = element_blank()
    ,
    axis.ticks = element_blank()
  )
}

### Check now how many wil not be shown in the world map [ Exceptions: Macau ]
unique(dataset$country)[!(unique(dataset$country) %in% unique(world_map_adapted$region))]

### European Data

european_countries = c(
  "Albania",
  "Andorra",
  "Austria",
  "Belarus",
  "Belgium",
  "Bosnia and Herzegovina",
  "Bulgaria",
  "Croatia",
  "Czech Republic",
  "Denmark",
  "Estonia",
  "Finland",
  "France",
  "Germany",
  "Greece",
  "Holy See",
  "Hungary",
  "Iceland",
  "Ireland",
  "Italy",
  "Latvia",
  "Liechtenstein",
  "Lithuania",
  "Luxembourg",
  "Malta",
  "Moldova",
  "Monaco",
  "Montenegro",
  "Netherlands",
  "North Macedonia",
  "Norway",
  "Poland",
  "Portugal",
  "Romania",
  "Russia",
  "San Marino",
  "Serbia",
  "Slovakia",
  "Slovenia",
  "Spain",
  "Sweden",
  "Switzerland",
  "Ukraine",
  "United Kingdom"
)

### Filter Dataset by European countries

european_dataset = dataset %>% filter(dataset$country %in% european_countries)

### Most affected countries dataset during crisis

most_affected_european_countries_crisis = c(
  "Greece",
  "Portugal",
  "Ireland",
  "Spain",
  "Italy",
  "France",
  "Netherlands",
  "United Kingdom",
  "Slovakia"
)

### Common data

summarize_function_year <- function(data) {
  data %>% summarize(
    suicides_no = sum(as.numeric(suicides_no)),
    population = sum(as.numeric(population)),
    suicide_rate = sum(suicides_no) / sum(population),
    suicide_rate_100k = round(suicide_rate * 100000, 3),
    gdp_for_year.... = mean(as.numeric(gdp_for_year....)),
    gdp_per_capita.... = mean(as.numeric(gdp_per_capita....))
  )
}

calculate_summary_by_group_year = function(data, ...) {
  data %>% group_by(...) %>% summarize_function
}

summarize_function <- function(data) {
  data %>% summarize(
    suicides_no = mean(as.numeric(suicides_no)),
    population = mean(as.numeric(population)),
    suicide_rate = suicides_no / population,
    suicide_rate_100k = round(suicide_rate * 100000, 3),
    gdp_for_year.... = mean(as.numeric(gdp_for_year....)),
    gdp_per_capita.... = mean(as.numeric(gdp_per_capita....))
  )
}

calculate_summary_by_group = function(data, ...) {
  data %>% group_by(...) %>% summarize_function
}

### Calculate the suicide rate per country and year

suicide_rate_per_country_year = dataset %>% calculate_summary_by_group_year(country, year)
suicide_rate_per_country_year

### Calculate the suicide rate per country, year and age

suicide_rate_per_country_year_age = dataset %>% calculate_summary_by_group_year(country, year, age)

### Calculate the suicide rate per country, year, age and gender

suicide_rate_per_country_year_age_sex <-
  dataset %>% calculate_summary_by_group_year(country, year, age, sex)
suicide_rate_per_country_year_age_sex

### Calculate the suicide rate per country over the years

suicide_mean_per_country = dataset %>% calculate_summary_by_group(country)

### Calculate the suicide rate per country, age and gender over the years

suicide_rate_per_country_age_sex = dataset %>% calculate_summary_by_group(country, age, sex)

### Calculate the suicide rate per country and sex over the years

suicide_rate_per_country_sex = dataset %>% calculate_summary_by_group(country, sex)

### Calculate the suicide rate per country and age over the years

suicide_rate_per_country_age = dataset %>% calculate_summary_by_group(country, age)

### Calculate the suicide rate per sex and age

suicide_rate_per_sex_age = dataset %>% calculate_summary_by_group(sex, age)

### Calculate the suicide rate per age

suicide_rate_per_age = dataset %>% calculate_summary_by_group(age)

### Calculate the suicide rate per sex

suicide_rate_per_sex = dataset %>% calculate_summary_by_group(sex)

### Calculate the suicide rate per country per generation per sex

suicide_rate_per_country_generation_sex = dataset %>% calculate_summary_by_group(country, generation, sex)

### Get top 10 countries with more suicides to raise plots later

suicide_mean_per_country_sorted = suicide_mean_per_country %>% arrange(-suicide_rate)
top_10_countries_more_suicides = suicide_mean_per_country_sorted %>% top_n(n = 10, suicide_rate)
suicide_rate_per_year_top_10 = suicide_rate_per_country_year %>% filter(country %in% top_10_countries_more_suicides$country) %>% arrange(-suicide_rate)
unique(suicide_rate_per_year_top_10$country)

## 0 Question: Study if the data is homogeneous
### If we count how many years of suicide information do the countries have, we can notice that countries such as Mongolia or Cabo Verde have only 1 year annotated, meaning that our data is not homogeneous and that every year the countries that have annotations for that year varies.
suicide_information_per_country <-
  dataset %>% group_by(country) %>% summarise(number_of_years = n_distinct(year))

### Number of annotations per year
suicide_information_per_country_map <-
  suicide_information_per_country %>% full_join(world_map_adapted, c("country" = "region"))

### World map indicating number of year annotated per country
ggplot(suicide_information_per_country_map,
       aes(long, lat, group = group)) +
  geom_polygon(aes(fill = number_of_years), color = "black") +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  labs(title = "Number of years with annotations per Country", fill = "Number of years") +
  get_map_theme()

## 1- Question
#random_years <- sample(min(dataset$year):max(dataset$year), 4, replace = F)
random_years <-
  sample(2000:2010, 4, replace = F) #some countries don't have the data of specific year
subdata <-
  dataset %>% calculate_summary_by_group(country, year) %>% subset(year %in% random_years) # subset(year<=2000 & year >=2003)
# use country as text input
subdata$country <- as.character(subdata$country)

###Suicide_rate related to gdp_per_year(Country Wealth)
#####For intuitive observation, gdp_for_year uses log function and suicide_rate presents in percentage
#####utilize fitting curve to describe the trend of two variables
#####adopt facet_wrap to valid the trend of several random years
ggplot(subdata, aes(x = log10(gdp_for_year), y = 100 * suicide_rate)) +
  labs(x = "gdp_for_year(log10)", y = "suicide_rate(%)", title = "Suicide rate related to Country Wealth") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = country), size = 2) +
  geom_point(aes(color = country, size = gdp_per_capita)) +
  geom_smooth() +
  facet_wrap( ~ year) +
  theme(legend.position = "none") # To plot it in google colab

ggplot(subdata, aes(x = gdp_per_capita / 1000, y = 100 * suicide_rate)) +
  labs(x = "gdp_per_capita(k)", y = "suicide_rate(%)", title = "Suicide rate related to Personal Wealth") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = country), size = 2) +
  geom_point(aes(color = country, size = log10(gdp_for_year))) +
  geom_smooth() +
  facet_wrap( ~ year) +
  theme(legend.position = "none") # To plot it in google colab

ggplot(suicide_mean_per_country, aes(x = log10(gdp), y = suicide_rate_100k)) +
  labs(x = "GDP per country (log10)", y = "Suicide Rate per 100k habitants", title = "Suicide rate related to Country Wealth") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = country), size = 2) +
  geom_point(aes(color = country, size = gdp_per_capita)) +
  geom_smooth() +
  theme(legend.position = "none") # To plot it in google colab

## 2- Question
WorldData <- map_data('world') %>%
  filter(region != "Antarctica") %>%
  fortify

WorldData = anchors::replace.value(WorldData,
                                   "region",
                                   from = old_names,
                                   to = new_names,
                                   verbose = FALSE)

Total <-
  dataset %>% group_by(country, year) %>% summarise(
    suicides_no = sum(suicides_no),
    population = sum(population),
    rates = suicides_no /
      population
  )
Countries <-
  summarise(
    group_by(Total, country),
    Mean_suicides = mean(rates),
    SD_suicides = sd(rates)
  )


df <- data.frame(
  region = Countries$country,
  value = 100 * (Countries$Mean_suicides),
  stringsAsFactors = FALSE
)

ggplot() +
  geom_map(
    data = WorldData,
    map = WorldData,
    aes(long, lat, group = group, map_id = region),
    fill = "white",
    colour = "#7f7f7f",
    size = 0.5
  ) +
  geom_map(
    data = df,
    map = WorldData,
    aes(fill = value, map_id = region),
    colour = "#7f7f7f",
    size = 0.5
  ) +
  coord_map(
    "rectangular",
    lat0 = 0,
    xlim = c(-180, 180),
    ylim = c(-60, 90)
  ) +
  scale_fill_continuous(low = "thistle2", high = "darkred", guide = "colorbar") +
  scale_y_continuous(breaks = c()) +
  scale_x_continuous(breaks = c()) +
  labs(fill = "Suicide rates (%)",
       title = "Suicide rate mean since 1987 ",
       x = "",
       y = "") +
  theme_bw()

SuicidesOverYears <-
  summarise(group_by(dataset, year),
            Suicides_rates = sum(suicides_no) / sum(population))
ggplot(data = SuicidesOverYears,
       aes(x = year, y = Suicides_rates)) + geom_point() +
  geom_smooth(se = FALSE,
              method = "gam",
              formula = y ~ s(log(x))) +
  labs(x = "Years", y = "Global Suicides Rate")

suicide_rate_country_year <- dataset %>%
  calculate_summary_by_group_year(country, year)

ggplot(suicide_rate_country_year, aes(x = country, y = suicide_rate)) +
  geom_point() +
  facet_wrap(~ country) +
  labs(x = "Countries", y = "Suicide Rate of each Year")

## 3- Question
suicide_rate_per_country_year_european = suicide_rate_per_country_year %>%
  filter(country %in% european_countries &
           year >= 2008 & year <= 2014)

ggplot(suicide_rate_per_country_year_european,
       aes(x = year, y = suicide_rate_100k)) +
  geom_boxplot(aes(group = year), outlier.size = -1) +
  geom_point() +
  labs(
    title = "Comparision in suicide rate during European Crisis",
    subtitle = "Focus in European countries between 2008 and 2014",
    x = "Years in European Crisis",
    y = "Suicide rate per 100k habitants"
  )

ggplot(suicide_rate_per_country_year_european,
       aes(x = year, y = suicide_rate)) +
  geom_line(aes(color = country)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Comparision in Suicide Rate during European Crisis [Between 2008 and 2014]",
    x = "Years in European Crisis",
    y = "Suicide rate per 100k habitants",
    color = "European Country"
  )

### Affected countries focus
most_affected_european_countries_crisis = c(
  "Greece",
  "Portugal",
  "Ireland",
  "Spain",
  "Italy",
  "France",
  "Netherlands",
  "United Kingdom",
  "Slovakia"
)

most_affected_countries_dataset = suicide_rate_per_country_year_european %>%
  filter(country %in% most_affected_european_countries_crisis)

ggplot(most_affected_countries_dataset,
       aes(x = year, y = suicide_rate_100k)) +
  geom_line(stat = "identity", aes(color = country)) +
  facet_wrap( ~ country, scales = "free_y") +
  labs(
    title = "Comparision in suicide rate during European debt crisis",
    subtitle = "Focus in most affected European countries between 2008 and 2014",
    color = "European Country",
    x = "Years in European Crisis",
    y = "Suicide rate per 100k habitants"
  )

ggplot(most_affected_countries_dataset,
       aes(x = year, y = suicide_rate_100k)) +
  geom_boxplot(aes(group = year)) +
  geom_point(aes(size = population, color = country)) +
  labs(
    title = "Comparision in Suicide Rate during European Crisis in most affected countries [Between 2008 and 2014]",
    color = "European Country",
    x = "Years in European Crisis",
    y = "Suicide rate per 100k habitants"
  )

### 4- Question

### Analyze which is the density of suicide rate for males and females.
ggplot(suicide_rate_per_country_sex,
       aes(x = suicide_rate_100k, fill = sex)) +
  geom_density(alpha = 0.4) +
  labs(title = "Distribution of Suicide Rate per Gender in history",
       x = "Suicide Rate per 100k habitants",
       y = "Density",
       fill = "Gender")

### Analyze the suicide_rate per gender in all the countries over the years

ggplot(suicide_rate_per_country_sex,
       aes(x = sex, y = suicide_rate_100k)) +
  geom_boxplot(aes(color = sex)) +
  labs(title = "Statistics of Suicide Rate per Gender in history",
       x = "Gender",
       y = "Suicide Rate per 100k habitants",
       fill = "Gender") +
  theme(legend.position = "none")

### Suicide Rate per Age Group and Gender in the History
ggplot(dataset, aes(x = age, y = suicides.100k.pop)) +
  geom_boxplot(aes(color = sex)) +
  ylim(0, 100) +
  labs(
    title = "Statistics of Suicide Rate per Age Group and Gender in the History",
    x = "Age Group",
    y = "Suicide Rate per 100k habitants",
    color = "Gender"
  )

### Suicide Rate per Generation Group and Gender in the History (same as age)
ggplot(suicide_rate_per_country_generation_sex,
       aes(x = generation, y = suicide_rate_100k)) +
  geom_boxplot(aes(color = sex)) +
  ylim(0, 100) +
  labs(title = "Statistics of Suicide Rate per Generation Group and Gender in the History",
       x = "Generation Group",
       y = "Suicide Rate per 100k habitants",
       fill = "Gender")

## 5- Question
ClustCountry <-
  dataset %>% group_by(age, country) %>% summarise(
    number_of_suicides = sum(suicides_no),
    population_number = sum(as.numeric(population)),
    rates = number_of_suicides / population_number
  )
ClustCountry <- arrange(ClustCountry, desc(population_number))
ClustSuicide <-
  filter(ClustCountry, number_of_suicides > 0) #We eliminate every 0 suicide elements
a <- ClustSuicide$population_number
y <- ClustSuicide$rates
x <- log(a)

ggplot(data = ClustSuicide,
       aes(
         x = log(population_number),
         y = rates,
         color = age
       )) + geom_point() +
  geom_smooth(se = FALSE,
              method = "gam",
              formula = y ~ s(log(x))) +
  labs(x = "Log transformation of the population", y = "Suicides Rate")
# We must omit the log in case we want to see what happens in low populated clusters