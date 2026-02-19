library(dplyr)
library(forcats)
library(grid)
library(ggplot2)
library(png)
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

# used to generate scores
recode_uses_scores = function(var) {
  as.numeric(as.character(fct_recode(var, 
             "0" = "not usable",
             "1" = "usable after adaptation",
             "2" = "directly usable")))
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
  select(-name,
         -target,
         -evaluator,
         -flag,
         -comments) %>% 

    # change variable type
  mutate(category = factor(category, levels = c("headline", "binary", "component", "complementary")),
         GAP = factor(GAP, levels = c("Access and benefit-sharing",
                                      "Agriculture, aquaculture, fisheries and forestry",
                                      "Biosafety and biotechnology",
                                      "Climate change",
                                      "Consumption",
                                      "Invasive alien species",
                                      "Knowledge and engagement of people",
                                      "Land and sea use",
                                      "Mainstreaming",
                                      "Means of implementation",
                                      "Nature’s contributions to people",
                                      "Pollution",
                                      "Species management",
                                      "Urban areas",
                                      "none assigned")),
         
         GAP = fct_recode(GAP, "No category" = "none assigned"),
  
         link_humans = factor(link_humans, levels = c("not connected",
                                                      "potentially connected",
                                                      "indirectly connected",
                                                      "directly connected")),
         link_animals = factor(link_animals, levels = c("not connected",
                                                        "potentially connected",
                                                        "indirectly connected",
                                                        "directly connected")),
         link_plants = factor(link_plants, levels = c("not connected",
                                                      "potentially connected",
                                                      "indirectly connected",
                                                      "directly connected")),
         link_environment = factor(link_environment, levels = c("not connected",
                                                                "potentially connected",
                                                                "indirectly connected",
                                                                "directly connected")),
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
  

# usability of indicators for any action track
use_AT_all <- df_clean %>% 
  select(use_AT1, use_AT2, use_AT3, use_AT4, use_AT5, use_AT6) %>% 
  
  # convert to scores
  mutate(use_AT1 = recode_uses_scores(use_AT1)) %>% 
  mutate(use_AT2 = recode_uses_scores(use_AT2)) %>% 
  mutate(use_AT3 = recode_uses_scores(use_AT3)) %>% 
  mutate(use_AT4 = recode_uses_scores(use_AT4)) %>% 
  mutate(use_AT5 = recode_uses_scores(use_AT5)) %>% 
  mutate(use_AT6 = recode_uses_scores(use_AT6)) %>%
  
  # calculate maximum score
  rowwise() %>% 
  mutate(use_ATs = max(use_AT1, use_AT2, use_AT3, use_AT4, use_AT5, use_AT6)) %>% 
  
  # convert to factor
  mutate(use_ATs = as.factor(as.character(use_ATs))) %>% 
  
  # rename factor
  mutate(use_ATs = fct_recode(use_ATs, 
                                  "not usable" = "0",
                                  "usable after adaptation" = "1",
                                  "directly usable" = "2"))
  
# merge with clean dataset
df_clean$use_ATs = use_AT_all$use_ATs

  
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
         use_AT6 = recode_uses(use_AT6),
         use_ATs = recode_uses(use_ATs)) 
  

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


# proportion of indicators that can be used (directly or after adaptation) to monitor each action track (all indicators)
df_simple %>% 
  select(use_AT1, use_AT2, use_AT3, use_AT4, use_AT5, use_AT6) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  count() %>% 
  mutate(prop = n / num_ind * 100)
  

# proportion of indicators that can be used (directly or after adaptation) to monitor each action track (headline and binary indicators only)
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


# proportion of indicators that can be used (directly or after adaptation) to monitor at least one AT, in each category
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



# number of indicators that can be used either directly or after adaptation for each action in the OH JPA

# actions in the first action track
table(df_clean$action_AT1, df_clean$use_AT1)

# actions in the second action track
table(df_clean$action_AT2, df_clean$use_AT2)

# actions in the third action track
table(df_clean$action_AT3, df_clean$use_AT3)

# actions in the fourth action track
table(df_clean$action_AT4, df_clean$use_AT4)

# actions in the fifth action track
table(df_clean$action_AT5, df_clean$use_AT5)

# actions in the sixth action track
table(df_clean$action_AT6, df_clean$use_AT6)



#### prepare figures ####

# One Health colors
colors <- c("#F7921C", "#EC008B", "#90268E", "#1976BC", "#36B449", "#D6DF22", "darkgrey")

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

# OHJPA logo
img_OHJPA <- readPNG("images/OHJPA.png")
img_OHJPA <- rasterGrob(img_OHJPA, interpolate=TRUE)

# prepare dataset to analyse the links between indicators and human, animal, plant, and environmental health

df_long_link <- df_clean %>% 
  select(link_humans,
         link_animals,
         link_plants,
         link_environment) %>% 
  
  # long format needed since indicators are associated with multiple groups
  pivot_longer(cols = everything(),
               names_to = "group",
               values_to = "link") %>% 
  
  # rename groups
  mutate(group = fct_recode(group, 
                                   "Human health" = "link_humans",
                                   "Animal health" = "link_animals",
                                   "Plant health" = "link_plants",
                                   "Environmental health" = "link_environment")) %>% 
  
  mutate(link = fct_recode(link, 
                                "Not connected" = "not connected",
                                "Potentially connected" = "potentially connected",
                                "Indirectly connected" = "indirectly connected",
                                "Directly connected" = "directly connected")) %>% 

  # reorder groups
  mutate(group = factor(group, levels = c("Human health",
                                        "Animal health", 
                                        "Plant health",
                                        "Environmental health")))


# prepare dataset to analyse the usability of indicators for monitoring OH actions, grouped by indicator categories 

df_long_cat <- df_clean %>% 
  select(category,
         use_AT1,
         use_AT2,
         use_AT3,
         use_AT4,
         use_AT5,
         use_AT6,
         use_ATs) %>% 
  
  # long format needed since indicators are associated with multiple action tracks
  pivot_longer(cols = !category,
               names_to = "action_track",
               values_to = "usability") %>% 
  
  # rename action tracks
  mutate(action_track = fct_recode(action_track, 
                                   "Action track 1" = "use_AT1",
                                   "Action track 2" = "use_AT2",
                                   "Action track 3" = "use_AT3",
                                   "Action track 4" = "use_AT4",
                                   "Action track 5" = "use_AT5",
                                   "Action track 6" = "use_AT6",
                                   "All action tracks" = "use_ATs")) %>% 
  
  # rename categories
  mutate(category = fct_recode(category, 
                                   "Headline indicators" = "headline",
                                   "Binary indicators" = "binary",
                                   "Component indicators" = "component",
                                   "Complementary indicators" = "complementary"))


# prepare dataset to analyse the usability of indicators for monitoring OH actions, grouped by GAP categories 

df_long_GAP <- df_clean %>% 
  select(GAP,
         use_AT1,
         use_AT2,
         use_AT3,
         use_AT4,
         use_AT5,
         use_AT6,
         use_ATs) %>% 
  
  # long format needed since indicators are associated with multiple action tracks
  pivot_longer(cols = !GAP,
               names_to = "action_track",
               values_to = "usability") %>% 
  
  # rename action tracks
  mutate(action_track = fct_recode(action_track, 
                                   "Action track 1" = "use_AT1",
                                   "Action track 2" = "use_AT2",
                                   "Action track 3" = "use_AT3",
                                   "Action track 4" = "use_AT4",
                                   "Action track 5" = "use_AT5",
                                   "Action track 6" = "use_AT6", 
                                   "All action tracks" = "use_ATs")) 



#### make figures ####

#### Figure: bar plot (number of indicators linked with human, animal, plant, and environmental health)

df_total_link <- df_long_link %>% 
  group_by(group, link) %>% 
  count()


ggplot() + 
  
  # add number of indicators connected to human, animal, plant and environmental health
  geom_bar(data = df_total_link, aes(y=n, x=group, fill=link), alpha = 0.8,
           position="stack", stat="identity") +

  
  # format y label
  scale_y_continuous(limits = c(0,210), expand = c(0, 0)) +
  
  # format x label 
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10),
                   limits = rev(levels(df_total_link$group))) +
  
  # change color
  scale_fill_brewer(palette = "Spectral") +
  
  # change theme
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank()) +
  
  # rename y axis
  ylab("Number of indicators")
 

# save figure
ggsave("figures/link_health.png",
       width = 8, height = 6, dpi = 800, 
       units = "in", device='png')



#### Figure: bar plot (proportion of usable indicators per action track)

# total number of indicators
n_all <- df_long_cat %>% 
  group_by(action_track) %>% 
  count() %>% 
  rename(tot = n)

# total number of directly usable indicators for each action track
n_direct <- df_long_cat %>%  
  filter(usability == "directly usable") %>% 
  group_by(action_track) %>% 
  count() %>% 
  rename(n_direct = n)

# total number of indicators usable after adaptation for each action track
n_adapt <- df_long_cat %>%  
  filter(usability == "usable after adaptation") %>% 
  group_by(action_track) %>% 
  count() %>% 
  rename(n_adapt = n)


# merge data frames
df_usability <- n_all %>%
  left_join(n_direct, by = "action_track") %>% 
  left_join(n_adapt, by = "action_track") 

# total number of usable indicators
df_usability <- df_usability %>% 
  mutate(n_usable = n_direct + n_adapt)

# proportion of usable indicators for each action track
df_usability <- df_usability %>% 
  mutate(prop_direct = n_direct / tot * 100) %>% # directly usable
  mutate(prop_adapt = n_adapt / tot * 100) %>%  # usable after adaptation
  mutate(prop_usable = n_usable / tot * 100) # both
  

ggplot() + 
  
  # add grey bars representing the total number of indicators
  geom_bar(data = df_usability, aes(y=tot, x=action_track), fill = "grey", alpha = 0.2,
           position="stack", stat="identity") +
  
  # add color bars representing the number of usable indicators for each action track
  geom_bar(data = df_usability, aes(fill=action_track, y=n_usable, x=action_track), 
           alpha=0.4, position="stack", stat="identity") +
  
  # add color bars representing the number of directly usable indicators for each action track
  geom_bar(data = df_usability, aes(fill=action_track, y=n_direct, x=action_track), 
           position="stack", stat="identity") +
  
  # flip axes
  coord_flip() +
  
  # format x label 
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10),
                   limits = rev(levels(df_long_cat$action_track))) +
  
  # format y label
  scale_y_continuous(limits = c(0,230), expand = c(0, 0)) +
  
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
  add_phylopic(img = AT1_img, x = 7, y = 115, ysize = 0.7) +
  add_phylopic(img = AT2_img, x = 6, y = 110, ysize = 0.7) +
  add_phylopic(img = AT3_img, x = 5, y = 85, ysize = 0.4) +
  add_phylopic(img = AT4_img, x = 4, y = 123, ysize = 0.7) +
  add_phylopic(img = AT5_img, x = 3, y = 50, ysize = 0.6) +
  add_phylopic(img = AT6_img, x = 2, y = 173, ysize = 0.7) +
  
  # add OHJPA logo
  annotation_custom(img_OHJPA, xmin=0, xmax=2, ymin=188, ymax=202) +

  # add proportions of usable indicators for each action track
  annotate("text", x = c(7, 6, 5, 4, 3, 2, 1), 
           y = c(130, 125, 103, 138, 65, 188, 210),
           label = paste0(round(df_usability$prop_usable, 0), "%"))

# save figure
ggsave("figures/usability_all.png",
       width = 8, height = 6, dpi = 800, 
       units = "in", device='png')




#### Figure: bar plot (proportion of usable indicators per action track and category)

# total number of indicators in each category and action track
n_all_cat <- df_long_cat %>%  
  group_by(category, action_track) %>% 
  count() %>% 
  rename(tot = n)

# number of directly usable indicators in each category and action track
n_direct_cat <- df_long_cat %>%  
  filter(usability == "directly usable") %>% 
  group_by(category, action_track) %>% 
  count() %>% 
  rename(n_direct = n)

# number of indicators usable after adaptation in each category and action track
n_adapt_cat <- df_long_cat %>%  
  filter(usability == "usable after adaptation") %>% 
  group_by(category, action_track) %>% 
  count() %>% 
  rename(n_adapt = n)

# merge data frames
df_usability_cat <- n_all_cat %>% 
  left_join(n_direct_cat, by = c("category", "action_track")) %>% 
  left_join(n_adapt_cat, by = c("category", "action_track"))

# total number of usable indicators in each category and action track
df_usability_cat <- df_usability_cat %>% 
  mutate(n_usable = n_direct + n_adapt)

# proportion of usable indicators for each category and action track
df_usability_cat <- df_usability_cat %>% 
  mutate(prop_direct = n_direct / tot) %>% # directly usable
  mutate(prop_adapt = n_adapt / tot) %>%  # usable after adaptation
  mutate(prop_usable = n_usable / tot) %>% # both 
  mutate(prop_tot = 1)

# number of indicators for each category
n_cat <- df_clean %>%  
  group_by(category) %>% 
  count() 


ggplot() + 
  
  # add grey bars representing the maximum proportion (1)
  geom_bar(data = df_usability_cat, aes(y=prop_tot, x=action_track), fill = "grey", alpha = 0.2,
           position="stack", stat="identity") +
  
  # add color bars representing the proportion of usable indicators for each action track
  geom_bar(data = df_usability_cat, aes(fill=action_track, y=prop_usable, x=action_track), 
           alpha = 0.4, position="stack", stat="identity") +
  
  # add color bars representing the proportion of directlt usable indicators for each action track
  geom_bar(data = df_usability_cat, aes(fill=action_track, y=prop_direct, x=action_track), 
           position="stack", stat="identity") +
  
  # add icons
  add_phylopic(img = AT1_img, x = 1, y = 0.1, ysize = 0.15) +
  add_phylopic(img = AT2_img, x = 2, y = 0.1, ysize = 0.15) +
  add_phylopic(img = AT3_img, x = 3, y = 0.07, ysize = 0.07) +
  add_phylopic(img = AT4_img, x = 4, y = 0.1, ysize = 0.15) +
  add_phylopic(img = AT5_img, x = 5, y = 0.1, ysize = 0.1) +
  add_phylopic(img = AT6_img, x = 6, y = 0.1, ysize = 0.15) +
  
  # add OHJPA logo
  annotation_custom(img_OHJPA, xmin=6.2, xmax=7.8, ymin=0.03, ymax=0.16) +
  
  # split by indicator category
  facet_wrap(~category, 
             labeller = labeller(category =
                                   c("Headline indicators" = paste0("Headline indicators (n = ", n_cat[1, 2] ,")"),
                                     "Binary indicators" = paste0("Binary indicators (n = ", n_cat[2, 2] ,")"),
                                     "Component indicators" = paste0("Component indicators (n = ", n_cat[3, 2] ,")"),
                                     "Complementary indicators" = paste0("Complementary indicators (n = ", n_cat[4, 2] ,")")))) +
  
  # format x label
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  
  # format y label 
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  
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
  ylab("Proportion of indicators")


ggsave("figures/usability_categories.png",
       width = 8, height = 6, dpi = 800, 
       units = "in", device='png')




#### Figure: bar plot (proportion of usable indicators per action track and GAP category)

# total number of indicators in each GAP category and action track
n_all_GAP <- df_long_GAP %>%  
  group_by(GAP, action_track) %>% 
  count() %>% 
  rename(tot = n)

# number of directly usable indicators in each GAP category and action track
n_direct_GAP <- df_long_GAP %>% 
  group_by(GAP, action_track) %>% 
  filter(usability == "directly usable") %>% 
  count() %>% 
  rename(n_direct = n)

# number of indicators usable after adaptation in each GAP category and action track
n_adapt_GAP <- df_long_GAP %>%  
  group_by(GAP, action_track) %>% 
  filter(usability == "usable after adaptation") %>% 
  count() %>% 
  rename(n_adapt = n)

# merge data frames
df_usability_GAP <- n_all_GAP %>% 
  left_join(n_direct_GAP, by = c("GAP", "action_track")) %>% 
  left_join(n_adapt_GAP, by = c("GAP", "action_track"))

# change NAs to 0s
df_usability_GAP <- df_usability_GAP %>% 
  replace_na(list(n_direct = 0, n_adapt = 0))
  
# total number of usable indicators in each category and action track
df_usability_GAP <- df_usability_GAP %>% 
  mutate(n_usable = n_direct + n_adapt)

# proportion of usable indicators for each category and action track
df_usability_GAP <- df_usability_GAP %>% 
  mutate(prop_direct = n_direct / tot) %>% # directly usable
  mutate(prop_adapt = n_adapt / tot) %>%  # usable after adaptation
  mutate(prop_usable = n_usable / tot) %>% # both 
  mutate(prop_tot = 1)

# number of indicators for each category
n_GAP <- df_clean %>%  
  group_by(GAP) %>% 
  count() 


ggplot() + 
  
  # add grey bars representing the total number of indicators
  geom_bar(data = df_usability_GAP, aes(y=prop_tot, x=action_track), fill = "grey", alpha = 0.2,
           position="stack", stat="identity") +
  
  # add color bars representing the number of usable indicators for each action track
  geom_bar(data = df_usability_GAP, aes(fill=action_track, y=prop_usable, x=action_track), 
           alpha = 0.4, position="stack", stat="identity") +
  
  # add color bars representing the number of directly usable indicators for each action track
  geom_bar(data = df_usability_GAP, aes(fill=action_track, y=prop_direct, x=action_track), 
           position="stack", stat="identity") +

  # add icons
  add_phylopic(img = AT1_img, x = 1, y = 0.1, ysize = 0.2) +
  add_phylopic(img = AT2_img, x = 2, y = 0.1, ysize = 0.2) +
  add_phylopic(img = AT3_img, x = 3, y = 0.1, ysize = 0.1) +
  add_phylopic(img = AT4_img, x = 4, y = 0.1, ysize = 0.2) +
  add_phylopic(img = AT5_img, x = 5, y = 0.1, ysize = 0.15) +
  add_phylopic(img = AT6_img, x = 6, y = 0.1, ysize = 0.2) +
  
  # add OHJPA logo
  annotation_custom(img_OHJPA, xmin=6, xmax=8, ymin=0.03, ymax=0.2) +
  
  # split by GAP category
  facet_wrap(~GAP, nrow = 5, ncol = 3,
             labeller = labeller(GAP =
                                   c("Access and benefit-sharing" = paste0("Access and benefit-sharing (n = ", n_GAP[1, 2] ,")"),
                                     "Agriculture, aquaculture, fisheries and forestry" = paste0("Agriculture, aquaculture, fisheries and forestry (n = ", n_GAP[2, 2] ,")"),
                                     "Biosafety and biotechnology" = paste0("Biosafety and biotechnology (n = ", n_GAP[3, 2] ,")"),
                                     "Climate change" = paste0("Climate change (n = ", n_GAP[4, 2] ,")"),
                                     "Consumption" = paste0("Consumption (n = ", n_GAP[5, 2] ,")"),
                                     "Invasive alien species" = paste0("Invasive alien species (n = ", n_GAP[6, 2] ,")"),
                                     "Knowledge and engagement of people" = paste0("Knowledge and engagement of people (n = ", n_GAP[7, 2] ,")"),
                                     "Land and sea use" = paste0("Land and sea use (n = ", n_GAP[8, 2] ,")"),
                                     "Mainstreaming" = paste0("Mainstreaming (n = ", n_GAP[9, 2] ,")"),
                                     "Means of implementation" = paste0("Means of implementation (n = ", n_GAP[10, 2] ,")"),
                                     "Nature’s contributions to people" = paste0("Nature’s contributions to people (n = ", n_GAP[11, 2] ,")"),
                                     "Pollution" = paste0("Pollution (n = ", n_GAP[12, 2] ,")"),
                                     "Species management" = paste0("Species management (n = ", n_GAP[13, 2] ,")"),
                                     "Urban areas" = paste0("Urban areas (n = ", n_GAP[14, 2] ,")"),
                                     "No category" = paste0("No category (n = ", n_GAP[15, 2] ,")")))) +
  
  # format x label
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  
  # format y label 
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  
  # use One Health colors
  scale_fill_manual(values = colors) +
  
  # change theme
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  
  # rename y axis
  ylab("Proportion of indicators")


ggsave("figures/usability_GAP.png",
       width = 10, height = 8, dpi = 800, 
       units = "in", device='png')
