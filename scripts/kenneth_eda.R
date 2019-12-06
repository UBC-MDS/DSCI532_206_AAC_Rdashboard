library(tidyverse)
library(ggplotly)

df <- as_tibble(read_csv("../data/aac_data_cleaned.csv"))
df %>%
  ggplot(aes(x = factor(animal_type), y = total_time_in_shelter_days)) +
  
  geom_jitter( alpha = 0.1) +
  geom_boxplot(outlier.shape = NA) +
  coord_trans(y = "log10")

