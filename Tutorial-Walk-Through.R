
# Load packages -----------------------------------------------------------

library(tidyverse)


# Deaths from Tigers ------------------------------------------------------

tiger_data <- read_csv("https://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02e2aDeathsFromTigers.csv")
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   person = col_double(),
#>   activity = col_character()
#> )

tiger_data                  # print the data in the console
#> # A tibble: 88 x 2
#>   person activity             
#>    <dbl> <chr>                
#> 1      1 Disturbing tiger kill
#> 2      2 Forest products      
#> 3      3 Grass/fodder         
#> 4      4 Fuelwood/timber      
#> 5      5 Grass/fodder         
#> 6      6 Forest products      
#> # … with 82 more rows

distinct(tiger_data, activity)
#> # A tibble: 9 x 1
#>   activity             
#>   <chr>                
#> 1 Disturbing tiger kill
#> 2 Forest products      
#> 3 Grass/fodder         
#> 4 Fuelwood/timber      
#> 5 Fishing              
#> 6 Herding              
#> 7 Sleeping in house    
#> 8 Walking              
#> 9 Toilet


# Tiger Deaths Contingency Table ------------------------------------------

count(tiger_data, activity)
#> # A tibble: 9 x 2
#>   activity                  n
#> * <chr>                 <int>
#> 1 Disturbing tiger kill     5
#> 2 Fishing                   8
#> 3 Forest products          11
#> 4 Fuelwood/timber           5
#> 5 Grass/fodder             44
#> 6 Herding                   7
#> # … with 3 more rows


# Tiger Deaths Bar Graph --------------------------------------------------

ggplot(data = tiger_data)

ggplot(data = tiger_data) +
  geom_bar(mapping = aes(x = activity))

ggplot(data = tiger_data) +
  geom_bar(mapping = aes(x = fct_infreq(activity)))

ggplot(data = tiger_data) +
  geom_bar(mapping = aes(x = fct_infreq(activity)), fill = "#C5351B")

ggplot(data = tiger_data) +
  geom_bar(mapping = aes(x = fct_infreq(activity)), fill = "#C5351B") +
  labs(x = "Activity", y = "Frequency (number of people)")

ggplot(data = tiger_data) +
  geom_bar(mapping = aes(x = fct_infreq(activity)), fill = "#C5351B", 
           width = .8) +
  labs(x = "Activity", y = "Frequency (number of people)") +
  scale_y_continuous(limits = c(0, 50), expand = expansion(mult = 0)) +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black", size = rel(1)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank()
  )


# Bird Abundances ---------------------------------------------------------

bird_data <- read_csv("https://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02e2bDesertBirdAbundance.csv")
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   species = col_character(),
#>   abundance = col_double()
#> )

bird_data                  # print the data in the console
#> # A tibble: 43 x 2
#>   species          abundance
#>   <chr>                <dbl>
#> 1 Black Vulture           64
#> 2 Turkey Vulture          23
#> 3 Harris's Hawk            3
#> 4 Red-tailed Hawk         16
#> 5 American Kestrel         7
#> 6 Gambel's Quail         148
#> # … with 37 more rows

# Bird Abundance Histogram ------------------------------------------------

ggplot(data = bird_data)

ggplot(data = bird_data) +
  geom_histogram(mapping = aes(x = abundance))

ggplot(data = bird_data) +
  geom_histogram(mapping = aes(x = abundance), binwidth = 50)

ggplot(data = bird_data) +
  geom_histogram(mapping = aes(x = abundance), binwidth = 50,
                 boundary = 0)

ggplot(data = bird_data) +
  geom_histogram(mapping = aes(x = abundance), binwidth = 50,
                 boundary = 0, closed = "left")

ggplot(data = bird_data) +
  geom_histogram(mapping = aes(x = abundance), binwidth = 50,
                 boundary = 0, closed = "left", 
                 fill = "#C5351B", color = "black") +
  labs(x = "Abundance", y = "Frequency (number of species)") +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30), 
                     expand = expansion(mult = 0)) +
  scale_x_continuous(breaks = seq(0, 600, 100)) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black", size = rel(1))
  )
