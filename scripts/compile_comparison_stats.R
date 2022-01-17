## Compare a drone map(s) with a ground-truth map

library(tidyverse)
library(sf)
library(here)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#source(here("scripts/convenience_functions.R"))
source(here("scripts/compare_stem_map_functions.R"))


stats_files = list.files(datadir("/drone-map-evals/individual/"), full.names = TRUE)

d = read_csv(stats_files)

d = d %>%
  group_by(height_cat,tree_position) %>%
  summarize(across(starts_with("n_"),~sum(.x,na.rm=FALSE))) %>%
  mutate(sensitivity = n_ground_matched_drone/n_ground,
         precision = n_drone_matched_ground/n_drone) %>%
  mutate(f_score = 2*sensitivity*precision/(sensitivity+precision)) %>%
  mutate_if(is.numeric,round,digits=3) %>%
  arrange(desc(tree_position),height_cat) %>%
  select(tree_position,height_cat,f_score,sensitivity,precision)

write_csv(d,datadir("tables/tree_det_accuracy.csv"))

### Tree height scatterplot

height_files = list.files(datadir("drone-map-evals/matched-tree-lists/"),pattern="all", full.names=TRUE)

d = read_csv(height_files) %>%
  filter(!is.na(drone_tree_id) & !is.na(ground_tree_id)) %>%
  mutate(ground_tree_health = ifelse(is.na(ground_tree_health), "none",ground_tree_health))

p = ggplot(d,aes(x=ground_tree_height, y = drone_tree_height, color=ground_tree_height_acc)) +
  geom_abline(intercept=0,slope=1,color="red") +
  geom_point() +
  scale_color_manual(values=c("black","grey70"), name = "Ground meas.") +
  theme_bw() +
  theme(legend.position = c(.8,.2),legend.background = element_blank(),legend.key = element_rect(fill = "transparent"), legend.box.background = element_blank()) +
  lims(x = c(5,60),y=c(5,60)) +
  labs(x = "Ground-meausred height (m)",
       y = "Drone-measured height (m)") +
  annotate(
    geom = "text", x = 6, y = 58, 
    label = expression(R^2:~0.97), size = 3.5, hjust=0) + 
  annotate(
    geom = "text", x = 6, y = 53, 
    label = expression(Bias:~"-0.35 m"), size = 3.5, hjust=0) + 
  annotate(
    geom = "text", x = 6, y = 48, 
    label = expression(MAE:~"1.64 m"), size = 3.5, hjust=0)

png(datadir("figures/trees_height_match.png"), res=180,width=510*1.2,height=500*1.2)
p
dev.off()


d_accurate = d %>%
  filter(ground_tree_height_acc == "accurate")

cor(d_accurate$drone_tree_height, d_accurate$ground_tree_height)^2
mean(d_accurate$drone_tree_height - d_accurate$ground_tree_height)
mean(abs(d_accurate$drone_tree_height - d_accurate$ground_tree_height))
#correlation of 0.97            