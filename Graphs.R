
#########################
### Creating Sample data
#########################
library(ggplot2)
library(magrittr)
library(tidyverse)

# master data
clif_stren <- data.frame(
             clif_id = c(1:34),
             clif = c('Analytical','Context','Futuristic','Ideation','Input','Intellection','Learner','Strategic',
                      'Adaptability','Connectedness','Developer','Empathy','Harmony','Includer','Individualization','Positivity','Relator',
                      'Activator','Command','Communication','Competition','Maximizer','Self-Assurance','Significance','Woo',
                      'Achiever','Arranger','Belief','Consistency','Deliberative','Discipline','Focus','Responsibility','Restorative'
             ),
             theme = c(rep('STRATEGIC THINKING', times=8), rep('RELATIONSHIP BUILDING', times=9), rep('INFLUENCING', times=8), rep('EXECUTING', times=9)),
             color = c(rep('green',times=8), rep('blue', times=9), rep('orange', times=8),rep('magenta', times=9))
)

# mocked transactional data
clif_data <- data.frame(
      per = rep(1:5, each=34),
      order = c(rep(1:34, each=1), rep(1:34, each=1), rep(1:34, each=1), rep(1:34, each=1), rep(1:34, each=1)),
      clif_id = c(sample(34, replace=FALSE),sample(34, replace=FALSE),sample(34, replace=FALSE),sample(34, replace=FALSE),sample(34, replace=FALSE))
      
)

#join data
clif_data %<>% inner_join(clif_stren, by = "clif_id")



##############
### heatmap
##############

ggplot(clif_data) +
  aes(x = order, y = per, fill = color) +
  geom_tile(size = 1.5) +
  scale_fill_manual(
    values = c(blue = "#0070cd",
               green = "#00945e",
               orange = "#e97200",
               magenta = "#7b2581")
  ) +
  theme_minimal() +
  theme(legend.position = "none")





#########
# Points
##########

#order by people
ggplot(clif_data) +
  aes(x = per, y = order, size = clif) +
  geom_point(shape = "circle", colour = "#112446") +
  theme_minimal()



##############
### Radar
##############

# devtools::install_github("ricardo-bion/ggradar")
library(ggradar)


radar <- clif_data %>%
  group_by(color, per) %>%
  #filter(per == 1 | per == 2 | per == 3) %>%
  summarise(total_p = 1 - (sum(order)/200)) %>%
  pivot_wider(names_from = color, values_from = total_p)

radar$per <- as.character(radar$per)
radar[, 1] <- paste0("Person",1:5)  
colnames(radar) <- c("Group", "Blue", "Green", "Orange", "Magenta")

ggradar(radar)


