library(dplyr)
library(ggplot2)
library(ggalluvial)
library(readr)
library(tidyr)

## read KM-GBF dataset 
# this dataset contains selected columns of the Airtable dataset

km_gbf <- read_csv("data/KMGBF_indicators.csv")

## rename columns 
names(km_gbf)[4] <- "ActionTrack" 
  
## replace NAs with specified value
km_gbf$ActionTrack <- replace_na(km_gbf$ActionTrack, "none")

## change variable type
km_gbf$IndicatorCategory <- factor(km_gbf$IndicatorCategory, levels = c("headline", "binary", "component", "complementary"))

km_gbf$ActionTrack <- factor(km_gbf$ActionTrack, levels = c("Action track 1", 
                                "Action track 2", "Action track 3", "Action track 4",
                                "Action track 5", "Action track 6", "none"))


## make plot with all indicator categories

# One Health colors
colors <- c("#F7921C", "#EC008B", "#90268E", "#1976BC", "#D6DF22", "grey")

ggplot(data = km_gbf,
       aes(axis1 = IndicatorCategory, axis2 = ActionTrack)) +
  geom_alluvium(aes(fill = ActionTrack),
                curve_type = "cubic") +
  geom_stratum(aes(fill = ActionTrack)) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("IndicatoryCategory", "ActionTrack"),
                   expand = c(0.15, 0.05)) +
  scale_fill_manual(values = colors) +
  theme_void() + 
  theme(legend.position = "none")

ggsave("figures/sankey_all.png")





# make plot with all headline and binary indicators only

# filter dataset
km_gbf_small <- filter(km_gbf, IndicatorCategory %in% c("headline", "binary"))

# One Health colors
colors <- c("#F7921C", "#EC008B", "#90268E", "#D6DF22", "grey")

ggplot(data = km_gbf_small,
       aes(axis1 = IndicatorCategory, axis2 = ActionTrack)) +
  geom_alluvium(aes(fill = ActionTrack),
                curve_type = "cubic") +
  geom_stratum(aes(fill = ActionTrack)) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("IndicatoryCategory", "ActionTrack"),
                   expand = c(0.15, 0.05)) +
  scale_fill_manual(values = colors) +
  theme_void() + 
  theme(legend.position = "none")

ggsave("figures/sankey_headline_binary.png")

