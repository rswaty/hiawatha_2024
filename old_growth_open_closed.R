

## old growth change split out by open/closed


library(tidyverse)

## read in data

raw_data <- read.csv("outputs/final_bps_scls.csv")


## filter, group and add helper columns


old_classes <- c("Late1", "Late2")

old_growth_chng_canopy$bps_name <- gsub(paste(remove, collapse = "|"), "", old_growth_chng_canopy$bps_name)

old_growth_chng_canopy <- raw_data %>%
  filter(age_category %in% old_classes) %>%
  filter(canopy_category != 'ALL') %>%
  slice_max(order_by = bps_acres, n = 16) %>%
  group_by(bps_name, canopy_category) %>%
  summarize(ref_percent = sum(ref_percent, na.rm = TRUE),
            cur_percent = sum(cur_percent, na.rm = TRUE),
            bps_acres = max(bps_acres)) %>%
  mutate(change = cur_percent - ref_percent,
         sign_change = (change > 0))  

old_growth_chng_canopy <- old_growth_chng_canopy %>%
  mutate(bps_name = str_replace_all(bps_name, paste(remove, collapse = "|"), ""))



## try chart with facets

facet_names <- c(
  "CLS" = "Closed Canopy",
  "OPN" = "Open Canopy"
)

canopy_arrow_plot <- old_growth_chng_canopy %>%
  ggplot(aes(
    x = ref_percent, xend = cur_percent, 
    y = reorder(bps_name, bps_acres), yend = bps_name,
    color = sign_change)) +
  geom_segment(
    arrow = arrow(angle = 30, length = unit(0.5, 'cm')),
    size = 2) +
  labs(
    x = 'Percent Change', 
    y = element_blank(),
    title = 'Changes in Late Succession Classes Historical to ~2022',
    subtitle = 'Arrows in descending order by total extent of ecosystem'
  ) +
  scale_color_manual(
    values = c("#fcba03", "#10692c")) +
  theme_bw(base_size = 14) + 
  theme(legend.position = "none") +
  facet_wrap(~ canopy_category, 
             ncol = 2,
             labeller = as_labeller(facet_names))

canopy_arrow_plot

# if the plot looks good, save it
ggsave("outputs/open_closed_late.png", width = 12, height = 8)

