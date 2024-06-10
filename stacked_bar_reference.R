

## stacked bar, bps x age class

## stacked bar, bps x class label (early, mid, late)

# Load packages
library(tidyverse)
library(scales)

# Options

options(scipen = 9999999999)

words_to_remove <- c("Laurentian-Acadian ",
                  "Boreal ",
                  "Laurentian ",
                  "Great Lakes ",
                  "Central ")

final_df_full <- read.csv("data/final_df_full.csv")
View(final_df_full)

for_chart <- final_df_full %>%
  filter(!is.na(age_category)) %>%
  filter(bps_acres > 10000) %>%
  select(c("age_category",
           "ref_scls_acres", 
           "bps_name", 
           "bps_acres"))%>%
  group_by(bps_name, age_category) %>%
  summarise(sum_scls_acres = sum(ref_scls_acres)) %>%
  mutate(bps_name = str_replace_all(bps_name, paste(words_to_remove, collapse = "|"), "")) %>%
  mutate(age_category = fct_collapse(age_category, Other = c("Early1",
                                                         "Early2",
                                                         "Mid1",
                                                         "Mid2"))) %>%
  mutate(age_category = fct_collapse(age_category, Late = c("Late1",
                                                             "Late2",
                                                             "Late3"))) %>%
  group_by(age_category, bps_name) %>%
  summarise(sum_scls_acres = sum(sum_scls_acres))

geographies <- c(
  "Boreal ",
  "Central Interior and Appalachian ",
  "Great Lakes ",
  "Laurentian ",
  "Laurentian-Acadian ",
  "North-Central Interior ")


  
plot_acres <-
  ggplot(for_chart, aes(fill = age_category, y = sum_scls_acres, x = reorder(bps_name, -sum_scls_acres))) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  labs(
    title = "Biophysical Settings of the area, with estimated reference acres of \n'late' and 'other succession classes",
    subtitle = "",
    caption = "Data from landfire.gov. BpSs with small footprint removed for clarity (> 10k acres)",
    x = "",
    y = "Acres",
    fill = "Succession Class") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(label = comma, n.breaks = 4) + 
  theme_bw(base_size = 22) + 
  scale_fill_manual(values = c("#f5922f", # orange
                               "#532a66" # purple
 )) +
  theme(plot.caption = element_text(hjust = 0, face = "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") +
  theme(legend.position = c(0.8, 0.3)) + 
  theme(plot.margin = unit(c(0.2, 0.75, 0.2, 0.2),
                           "inches"))

plot_acres





