
# Load Packages -----------------------------------------------------------

library(tidyverse)

# Read Firefly Data -------------------------------------------------------

firefly_data <- read_csv("https://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02q19FireflySpermatophoreMass.csv")
firefly_data

# Mass measurments of each firefly spermataphore --------------------------

distinct(firefly_data, spermatophoreMass)


# Contingency Table of firefly spermataphore mass measurments ---------------------------

count(firefly_data, spermatophoreMass)


# Bar Graph of firefly spermatophore mass measurements --------------------

ggplot(data = firefly_data) +
  geom_bar(mapping = aes(x = spermatophoreMass), fill = "#92C7C7") +
  labs(x = "Mass of Spermatophore (mg)", y = "Frequency (number of individual fireflies)") +
  scale_x_continuous(breaks = seq(0, 0.175,0.01), limits = c(0, 0.176)) +
  scale_y_continuous(limits = c(0, 5), expand = expansion(mult = 0)) +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black", size = rel(1)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank())

# Histogram of firefly spermatophore mass measurements ----------------------------------

ggplot(data = firefly_data) +
  geom_histogram(mapping = aes(x = spermatophoreMass), binwidth = 0.01, 
                 boundary = 0, closed = "left", fill = "#92C7C7", 
                 color = "black") +
  labs(x = "Mass of Spermatophore (mg)", y = "Frequency (number of individual fireflies)", 
       title = "Frequency distribution of spermatophore mass from a sample of 35 male fireflies") +
  scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10), 
                     expand = expansion(mult = 0)) +
  scale_x_continuous(breaks = seq(0.03, 0.18, 0.01), limits = c(0.03, 0.18)) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black", size = rel(1)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Load packages and Bird Data


library(auk)                          # load the auk package
birds <- ebird_taxonomy %>%           # start with the ebird_taxonomy data
  as_tibble() %>%                     # tibbles print better in the console
  filter(category == "species")       # remove non-species taxa


# Bird Order Bar Graph

ggplot(data = birds) +
  geom_bar(mapping = aes(x = fct_infreq(order)), fill = "#E2A76F", 
           width = .8) +
  labs(x = "Order", y = "Frequency (log number of bird species)", 
       title = "Number of bird species in each order", 
       subtitle = "*Note that numbers are in log scale to better show all data.") +
  scale_y_log10() +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black", size = rel(.8)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank()
  )

