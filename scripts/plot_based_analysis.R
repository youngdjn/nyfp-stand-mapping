## Compare a drone map(s) with a ground-truth map

library(tidyverse)
library(sf)
library(here)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#source(here("scripts/convenience_functions.R"))
source(here("scripts/compare_stem_map_functions.R"))

d = st_read(datadir("ground-mapping-data/tree-processed/tree-locs-rectified-macroplot.gpkg"))
st_geometry(d) = NULL

# treat them as 8-m plots
# get total number of trees per macroplot in differen size classes
d_summ = d %>%
  filter(dist <= 8) %>%
  mutate(dbh = dbh / 2.54) %>% # cm to in
  group_by(macroplot_id, species) %>%
  summarize(dbh10 = sum(between(dbh,10,20)),
            dbh20 = sum(between(dbh,20.01,30)),
            dbh30 = sum(between(dbh,30.01,40)),
            dbh40 = sum(dbh >= 40.01))

# pull in number of plots
numplots = d %>%
  group_by(macroplot_id) %>%
  summarize(nplots = length(unique(plot_id)))

d_summ = d_summ %>%
  left_join(numplots) %>%
  mutate(across(starts_with("dbh"), ~ . / nplots / 0.049658)) %>% # convert to mean number of trees per hectare
  select(-nplots)
  
d_long = d_summ %>%
  pivot_longer(cols=starts_with("dbh"), names_to="size",values_to ="tpa") %>%
  mutate(size = recode(size,dbh10 = "10-20",dbh20 = "20-30", dbh30 = "30-40", dbh40 = "> 40"))

p = ggplot(d_long, aes(x = size, y = tpa, fill=species)) +
  geom_bar(stat="identity") +
  facet_wrap(~macroplot_id) +
  theme_bw() +
  labs(x = "DBH class (inches)", y = "Trees per acre") +
  scale_fill_viridis_d()

png(datadir("figures/size_distrib.png"), res=180,width=1000,height=800)
p
dev.off()

#write_csv(d_summ,datadir("tables/plot_dens_summ.csv"))
            



### height-dbh fit
# use all trees > 25 cm dbh and > 20 m height w accurate height measurements

d_allo = d %>%
  filter(dbh > 25,
         height > 20,
         height_acc == "accurate")

p = ggplot(d_allo,aes(x=dbh,y=height,color=health,shape=status)) +
  geom_point() +
  scale_shape_manual(values=c(17,16)) +
  labs(x = "DBH (cm)",
       y = "Height (m)") +
  theme_bw()
  
png(datadir("figures/dbh-height-allo.png"), res=180,width=1000,height=500*1.2)
p
dev.off()

