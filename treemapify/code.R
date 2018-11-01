library(ggplot2)
library(treemapify)
G20 <- G20


# Area of the map is the countriers GDP
ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label = country)) + geom_treemap() +
  geom_treemap_text(fontface = 'italic', color = 'white', place = 'center', grow = T, min.size = 1)

## Adding subgroups

ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label = country, subgroup = region)) + geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = 'center', grow = T, alpha = 0.5, color = 'red',
                             fontface = 'italic', min.size = 1) +
  geom_treemap_text(color = 'white', place = 'topleft', reflow = T)


# we can add more than one subgroup

ggplot(G20, aes(area = 1,label = country, subgroup = hemisphere, 
                subgroup2 = region, subgroup3 = econ_classification)) +
  geom_treemap() +
  geom_treemap_subgroup3_border(color = 'blue', size = 1) +
  geom_treemap_subgroup2_border(color = 'white', size = 3) +
  geom_treemap_subgroup_border(color = 'red', size = 5) +
  geom_treemap_subgroup_text(place = 'middle', color = 'red', alpha = 0.5,  grow = T) +
  geom_treemap_subgroup2_text(color =  'white', alpha = 0.5, fontface = 'italic') +
  geom_treemap_subgroup3_text(place = 'top', color = 'blue', alpha = 0.5) +
  geom_treemap_text(color = 'white', place = 'middle', reflow = T)
  