library(dplyr)
library(forcats)
library(ggalluvial)
library(ggplot2)
library(ggsankey)
library(readr)
library(tidyr)

#### read data ####

# dataset of biodiversity indicators evaluated by all collaborators
df_raw <- read_csv("data/one_health_indicators.csv")


#### clean data ####

# function to recode link categories 
# for now, we want only two link categories (for simplicity reasons)
recode_links = function(var) {
  fct_recode(var,
           "not connected" = "not connected",
           "not connected" = "potentially connected",
           "connected" = "indirectly connected",
           "connected" = "directly connected")
}

# function to recode usability categories
# for now, we want only two usability categories (for simplicity reasons)
recode_uses = function(var) {
  fct_recode(var, 
             "not usable" = "not usable",
             "usable" = "usable after adaptation",
             "usable" = "directly usable")
}


df_clean <- df_raw %>%
  
  # rename variables
  rename(label = indicator_label,
         name = indicator_name,
         category = indicator_category,
         target = indicator_target,
         GAP = indicator_GAP_category,
         evaluator = evaluator_name,
         flag = evaluator_flag,
         comments = evaluator_comments,
         link_humans = connection_human_health,
         link_animals = connection_animal_health,
         link_plants = connection_plant_health,
         link_environment = connection_environmnental_health,
         use_AT1 = AT1_health_systems_usability,
         use_AT2 = AT2_pandemic_zoonoses_usability,
         use_AT3 = AT3_endemic_zoonoses_usability,
         use_AT4 = AT4_food_safety_usability,
         use_AT5 = AT5_antimicrobial_resistance_usability,
         use_AT6 = AT6_environment_usability,
         action_AT1 = AT1_health_systems_action,
         action_AT2 = AT2_pandemic_zoonoses_action,
         action_AT3 = AT3_endemic_zoonoses_action,
         action_AT4 = AT4_food_safety_action,
         action_AT5 = AT5_antimicrobial_resistance_action,
         action_AT6 = AT6_environment_action) %>% 

  # select variables
  select(category, 
         link_humans,
         link_animals,
         link_plants,
         link_environment,
         use_AT1,
         use_AT2,
         use_AT3,
         use_AT4,
         use_AT5,
         use_AT6) %>% 
  
  # change variable type
  mutate(category = factor(category, levels = c("headline", "binary", "component", "complementary")),
         link_humans = factor(link_humans),
         link_animals = factor(link_animals),
         link_plants = factor(link_plants),
         link_environment = factor(link_environment),
         use_AT1 = factor(use_AT1),
         use_AT2 = factor(use_AT2),
         use_AT3 = factor(use_AT3),
         use_AT4 = factor(use_AT4),
         use_AT5 = factor(use_AT5),
         use_AT6 = factor(use_AT6)) %>% 
  
  # recode connections
  mutate(link_humans = recode_links(link_humans),
         link_animals = recode_links(link_animals),
         link_plants = recode_links(link_plants),
         link_environment = recode_links(link_environment)) %>% 
  
  # recode usability 
  mutate(use_AT1 = recode_uses(use_AT1),
         use_AT2 = recode_uses(use_AT2),
         use_AT3 = recode_uses(use_AT3),
         use_AT4 = recode_uses(use_AT4),
         use_AT5 = recode_uses(use_AT5),
         use_AT6 = recode_uses(use_AT6))
  


#### calculate proportions #### 

# total number of indicators 
num_ind <- nrow(df_clean)

# proportion of indicators directly or indirectly linked to Health 
# (either human, animal, plant, or environmental health)
num_ind_health <- df_clean %>% 
  filter(link_humans == "connected" |
          link_animals == "connected" | 
          link_plants == "connected" |
          link_environment == "connected") %>%
  count()
  

prop_ind_health <- num_ind_health / num_ind
prop_ind_health


# proportion of indicators that can be used to monitor each action track (all indicators)
df_clean %>% 
  select(use_AT1, use_AT2, use_AT3, use_AT4, use_AT5, use_AT6) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  count() %>% 
  mutate(prop = n / num_ind * 100)
  

# proportion of indicators that can be used to monitor each action track (headline and binary indicators only)
num_headbin <- df_clean %>% 
  filter(category %in% c("headline", "binary")) %>% 
  count()

df_clean %>% 
  filter(category %in% c("headline", "binary")) %>% 
  select(use_AT1, use_AT2, use_AT3, use_AT4, use_AT5, use_AT6) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  count() %>% 
  mutate(prop = n / num_headbin * 100)

# proportion of indicators that can be used to monitor at least one AT, in each category
num_cat <- df_clean %>% 
  group_by(category) %>% 
  count() %>% 
  rename("total" = n)

df_clean %>% 
  filter(use_AT1 == "usable" | 
           use_AT2 == "usable" |
           use_AT3 == "usable" | 
           use_AT4 == "usable" |
           use_AT5 == "usable" |
           use_AT6 == "usable") %>% 
  group_by(category) %>% 
  count() %>% 
  full_join(num_cat) %>% 
  mutate(prop = n / total * 100)

# same thing, but without AT6
df_clean %>% 
  filter(use_AT1 == "usable" | 
           use_AT2 == "usable" |
           use_AT3 == "usable" | 
           use_AT4 == "usable" |
           use_AT5 == "usable") %>% 
  group_by(category) %>% 
  count() %>% 
  full_join(num_cat) %>% 
  mutate(prop = n / total * 100)



#### make figures ####

# One Health colors
colors <- c("#F7921C", "#EC008B", "#90268E", "#1976BC", "#36B449", "#D6DF22", "grey")


## analyse the usability of indicators for monitoring OH actions, grouped by indicator categories 

## prepare dataset 

df_long <- df_clean %>% 
  select(category,
         use_AT1,
         use_AT2,
         use_AT3,
         use_AT4,
         use_AT5,
         use_AT6) %>% 
  
  # long format needed since indicators are associated to multiple action tracks
  pivot_longer(cols = !category,
               names_to = "action_track",
               values_to = "usability") %>% 
  
  # rename action tracks
  mutate(action_track = fct_recode(action_track, 
                                   "action track 1" = "use_AT1",
                                   "action track 2" = "use_AT2",
                                   "action track 3" = "use_AT3",
                                   "action track 4" = "use_AT4",
                                   "action track 5" = "use_AT5",
                                   "action track 6" = "use_AT6")) 



#### Figure: Bar plots (proportion of usable indicators per AT and category)

# number of indicators in each category
df_total <- df_long %>%  
  group_by(category, action_track) %>% 
  count()

# number of usable indicators in each category
df_usable <- df_long %>%  
  filter(usability == "usable") %>% 
  group_by(category, action_track) %>% 
  count()


ggplot() + 
  geom_bar(data = df_total, aes(y=n, x=action_track), fill = "grey", alpha = 0.6,
           position="stack", stat="identity") +
  geom_bar(data = df_usable, aes(fill=action_track, y=n, x=action_track), 
           position="stack", stat="identity") +
  facet_wrap(~category) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_continuous(limits = c(0,125), expand = c(0, 0)) +
  scale_fill_manual(values = colors) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")


ggsave("figures/bars_all.png",
       width = 8, height = 6, dpi = 800, 
       units = "in", device='png')


#### Figure: Bar plot (proportion of usable indicators per AT only)

# total number of indicators
df_total_all <- df_long %>% 
  group_by(action_track) %>% 
  count()

# total number of usable indicators for each AT
df_total_usable <- df_long %>%  
  filter(usability == "usable") %>% 
  group_by(action_track) %>% 
  count()

# proportion of usable indicators for each AT
df_total_usable %>% 
  mutate(prop = n / num_ind * 100)

ggplot() + 
  geom_bar(data = df_total_all, aes(y=n, x=action_track), fill = "grey", alpha = 0.6,
           position="stack", stat="identity") +
  geom_bar(data = df_total_usable, aes(fill=action_track, y=n, x=action_track), 
           position="stack", stat="identity") +
  coord_flip() +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10),
                   limits = rev(levels(df_total_all$action_track))) +
  scale_y_continuous(limits = c(0,210), expand = c(0, 0)) +
  scale_fill_manual(values = colors) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

ggsave("figures/bars_total.png",
       width = 8, height = 6, dpi = 800, 
       units = "in", device='png')



#### Figure: Sankey diagram (all categories)

## note: this needs to be fixed
## since indicators can be used to monitor different action tracks, the proportions on the graph don't make sens (they don't add up)

# calculate number of non usable indicators per category
df_not_usable <- df_clean %>% 
  filter(use_AT1 == "not usable" &
           use_AT2 == "not usable" &
           use_AT3 == "not usable" &
           use_AT4 == "not usable" &
           use_AT5 == "not usable" &
           use_AT6 == "not usable") %>%
  group_by(category) %>% 
  count() %>% 
  mutate(action_track = "none",
         usability = "not usable")


df_long2 <- df_long %>% 
  
  # keep number of usable (potentially or directly usable) indicators only
  filter(usability == "usable") %>% 
  
  # count the number of usable indicators for each category and AT
  group_by(category, action_track) %>% 
  count() %>% 
  
  # add again the usability column
  mutate("usability" = "usable") %>% 
  
  # add number of non usable indicators
  bind_rows(df_not_usable)


ggplot(df_long2,
       aes(y = n, axis1 = category, axis2 = action_track)) +
  geom_alluvium(aes(fill = action_track)) +
  geom_stratum(aes(fill = action_track), color = "black") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Indicator category", "Action tracks"), expand = c(.1, .1)) +
  scale_fill_manual(values = colors) +
  theme_void() + 
  theme(legend.position = "none")

ggsave("figures/sankey_all.png")



#### Figure: Sankey diagram (headline and binary indicators only)

df_long_headbin <- df_long2 %>% 
  filter(category %in% c("headline", "binary"))

ggplot(df_long_headbin %>% filter(),
       aes(y = n, axis1 = category, axis2 = action_track)) +
  geom_alluvium(aes(fill = action_track)) +
  geom_stratum(aes(fill = action_track), color = "black") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Indicator category", "Action tracks"), expand = c(.1, .1)) +
  scale_fill_manual(values = colors) +
  theme_void() + 
  theme(legend.position = "none")


ggsave("figures/sankey_headline_binary.png")

