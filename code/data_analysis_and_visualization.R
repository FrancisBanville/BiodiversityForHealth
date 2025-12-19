library(dplyr)
library(forcats)
library(ggplot2)
library(readr)
library(rphylopic)
library(tidyr)


#### read data ####

# dataset of biodiversity indicators evaluated by all collaborators
df_raw <- read_csv("data/one_health_indicators.csv")


#### clean data ####

# function to recode link categories 
# used to generate a simpler dataset with only two link categories
recode_links = function(var) {
  fct_recode(var,
           "not connected" = "not connected",
           "not connected" = "potentially connected",
           "connected" = "indirectly connected",
           "connected" = "directly connected")
}

# function to recode usability categories
# used to generate a simpler dataset with only two usability categories
recode_uses = function(var) {
  fct_recode(var, 
             "not usable" = "not usable",
             "usable" = "usable after adaptation",
             "usable" = "directly usable")
}


# clean dataset
df_clean <- df_raw %>%
  
  # rename variables (shorter names)
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

  # remove unused variables
  select(-label,
         -name,
         -target,
         -evaluator,
         -flag,
         -comments) %>% 
  
  # change variable type
  mutate(category = factor(category, levels = c("headline", "binary", "component", "complementary")),
         GAP = factor(GAP),
         link_humans = factor(link_humans),
         link_animals = factor(link_animals),
         link_plants = factor(link_plants),
         link_environment = factor(link_environment),
         use_AT1 = factor(use_AT1),
         use_AT2 = factor(use_AT2),
         use_AT3 = factor(use_AT3),
         use_AT4 = factor(use_AT4),
         use_AT5 = factor(use_AT5),
         use_AT6 = factor(use_AT6),
         action_AT1 = factor(action_AT1),
         action_AT2 = factor(action_AT2),
         action_AT3 = factor(action_AT3),
         action_AT4 = factor(action_AT4),
         action_AT5 = factor(action_AT5),
         action_AT6 = factor(action_AT6))


# simplify dataset 
df_simple <- df_clean %>% 
  
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
  


#### calculate statistics #### 

# total number of indicators 
num_ind <- nrow(df_clean)

# proportion of indicators directly or indirectly linked to Health (simplified as connected)
# (either human, animal, plant, or environmental health)
num_ind_health <- df_simple %>% 
  filter(link_humans == "connected" |
          link_animals == "connected" | 
          link_plants == "connected" |
          link_environment == "connected") %>%
  count()
  

prop_ind_health <- num_ind_health / num_ind
prop_ind_health


# proportion of indicators that can be used to monitor each action track (all indicators)
df_simple %>% 
  select(use_AT1, use_AT2, use_AT3, use_AT4, use_AT5, use_AT6) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  count() %>% 
  mutate(prop = n / num_ind * 100)
  

# proportion of indicators that can be used to monitor each action track (headline and binary indicators only)
num_headbin <- df_simple %>% 
  filter(category %in% c("headline", "binary")) %>% 
  count()

df_simple %>% 
  filter(category %in% c("headline", "binary")) %>% 
  select(use_AT1, use_AT2, use_AT3, use_AT4, use_AT5, use_AT6) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  count() %>% 
  mutate(prop = n / num_headbin * 100)


# proportion of indicators that can be used to monitor at least one AT, in each category
num_cat <- df_simple %>% 
  group_by(category) %>% 
  count() %>% 
  rename("total" = n)

df_simple %>% 
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
df_simple %>% 
  filter(use_AT1 == "usable" | 
           use_AT2 == "usable" |
           use_AT3 == "usable" | 
           use_AT4 == "usable" |
           use_AT5 == "usable") %>% 
  group_by(category) %>% 
  count() %>% 
  full_join(num_cat) %>% 
  mutate(prop = n / total * 100)



#### prepare figures ####

# One Health colors
colors <- c("#F7921C", "#EC008B", "#90268E", "#1976BC", "#36B449", "#D6DF22", "grey")

# PhyloPics
# Homo sapiens (Action track 1)
AT1_uuid <- get_uuid(name = "Homo sapiens", n=42)
AT1_img <- get_phylopic(uuid = AT1_uuid[37])

# Rhinolophus hipposideros (Action track 2)
AT2_uuid <- get_uuid(name = "Rhinolophus hipposideros")
AT2_img <- get_phylopic(uuid = AT2_uuid)

# Aedes aegypti (Action track 3)
AT3_uuid <- get_uuid(name = "Aedes aegypti")
AT3_img <- get_phylopic(uuid = AT3_uuid)

# Gallus gallus domesticus (Action track 4)
AT4_uuid <- get_uuid(name = "Gallus gallus domesticus")
AT4_img <- get_phylopic(uuid = AT4_uuid)

# Escherichia coli (Action track 5)
AT5_uuid <- get_uuid(name = "Escherichia coli")
AT5_img <- get_phylopic(uuid = AT5_uuid)

# Carpobrotus edulis
AT6_uuid <- get_uuid(name = "Carpobrotus edulis")
AT6_img <- get_phylopic(uuid = AT6_uuid)


# prepare dataset to analyse the usability of indicators for monitoring OH actions, grouped by indicator categories 

df_long <- df_simple %>% 
  select(category,
         use_AT1,
         use_AT2,
         use_AT3,
         use_AT4,
         use_AT5,
         use_AT6) %>% 
  
  # long format needed since indicators are associated with multiple action tracks
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



#### make figures ####


#### Figure: bar plot (proportion of usable indicators per action track)

# total number of indicators
df_total_all <- df_long %>% 
  group_by(action_track) %>% 
  count()

# total number of usable indicators for each action track
df_total_usable <- df_long %>%  
  filter(usability == "usable") %>% 
  group_by(action_track) %>% 
  count()

# proportion of usable indicators for each action track
df_prop_usable <- df_total_usable %>% 
  mutate(prop = n / num_ind * 100)


ggplot() + 
  
  # add grey bars representing the total number of indicators
  geom_bar(data = df_total_all, aes(y=n, x=action_track), fill = "grey", alpha = 0.6,
           position="stack", stat="identity") +
  
  # add color bars representing the number of usable indicators for each action track
  geom_bar(data = df_total_usable, aes(fill=action_track, y=n, x=action_track), 
           position="stack", stat="identity") +
  
  # flip axes
  coord_flip() +
  
  # format x label 
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10),
                   limits = rev(levels(df_total_all$action_track))) +
  
  # format y label
  scale_y_continuous(limits = c(0,210), expand = c(0, 0)) +
  
  # use One Health colors
  scale_fill_manual(values = colors) +
  
  # change theme
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  
  # rename y axis
  ylab("Number of indicators") +
  
  # add icons
  add_phylopic(img = AT1_img, x = 6, y = 115, ysize = 0.7) +
  add_phylopic(img = AT2_img, x = 5, y = 110, ysize = 0.7) +
  add_phylopic(img = AT3_img, x = 4, y = 85, ysize = 0.4) +
  add_phylopic(img = AT4_img, x = 3, y = 123, ysize = 0.7) +
  add_phylopic(img = AT5_img, x = 2, y = 50, ysize = 0.6) +
  add_phylopic(img = AT6_img, x = 1, y = 173, ysize = 0.7) +
  
  # add proportions of usable indicators for each action track
  annotate("text", x = c(6, 5, 4, 3, 2, 1), 
           y = c(130, 125, 103, 138, 65, 188),
           label = paste0(round(df_prop_usable$prop, 0), "%"))

# save figure
ggsave("figures/bars_total.png",
       width = 8, height = 6, dpi = 800, 
       units = "in", device='png')




#### Figure: bar plot (proportion of usable indicators per action track and category)

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
  
  # add grey bars representing the total number of indicators
  geom_bar(data = df_total, aes(y=n, x=action_track), fill = "grey", alpha = 0.6,
           position="stack", stat="identity") +
  
  # add color bars representing the number of usable indicators for each action track
  geom_bar(data = df_usable, aes(fill=action_track, y=n, x=action_track), 
           position="stack", stat="identity") +
  
  # add icons
  add_phylopic(img = AT1_img, x = 1, y = 8, ysize = 12) +
  add_phylopic(img = AT2_img, x = 2, y = 8, ysize = 12) +
  add_phylopic(img = AT3_img, x = 3, y = 8, ysize = 7) +
  add_phylopic(img = AT4_img, x = 4, y = 8, ysize = 12) +
  add_phylopic(img = AT5_img, x = 5, y = 8, ysize = 10) +
  add_phylopic(img = AT6_img, x = 6, y = 8, ysize = 12) +
  
  # split by indicator category
  facet_wrap(~category) +
  
  # format x label
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  
  # format y label 
  scale_y_continuous(limits = c(0,125), expand = c(0, 0)) +
  
  # use One Health colors
  scale_fill_manual(values = colors) +
  
  # change theme
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  
  # rename y axis
  ylab("Number of indicators")


ggsave("figures/bars_all.png",
       width = 8, height = 6, dpi = 800, 
       units = "in", device='png')

