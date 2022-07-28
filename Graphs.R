
#########################
### Creating Sample data
#########################
library(ggplot2)

# Colors
cs_color <- data.frame(
    clif = c(1:35)
    ,col = rep(c("green","blue","Orange", "Purple", "Yellow"), times=7)
)


# sample dataset
cs <- data.frame(
     per= rep(1:5, each=35)
    ,order = c(rep(1:35, each=1), rep(1:35, each=1), rep(1:35, each=1), rep(1:35, each=1), rep(1:35, each=1))
    ,clif = c(sample(35, replace=FALSE),sample(35, replace=FALSE),sample(35, replace=FALSE),sample(35, replace=FALSE),sample(35, replace=FALSE))
    ,team =  c(rep("A", 35) , rep("B", 35), rep( "A", 35), rep("A", 35), rep("B", 35))
)


cs %<>% inner_join(cs_color, by = "clif")


#heatmap of the group
ggplot(cs) +
  aes(x = order, y = per, fill = col) +
  geom_tile(size = 1.5) +
  scale_fill_hue(direction = 1) +
  theme_minimal()


#order by people
ggplot(cs) +
  aes(x = per, y = order, size = clif) +
  geom_point(shape = "circle", colour = "#112446") +
  theme_minimal()



ggplot(cs) +
  aes(x = order, y = per, fill = col) +
  geom_tile(size = 1.5) +
  scale_fill_manual(
    values = c(blue = "#0D0887",
               green = "#06A51B",
               Orange = "#CD8F38",
               Purple = "#BD21CB",
               Yellow = "#F0F921")
  ) +
  theme_minimal() +
  theme(legend.position = "none")



# Radar


# devtools::install_github("ricardo-bion/ggradar")
library(ggradar)
library(tidyverse)


radar <- cs %>%
  group_by(col, per) %>%
  #filter(per == 1 | per == 2 | per == 3) %>%
  summarise(total_p = 1 - (sum(order)/200)) %>%
  pivot_wider(names_from = col, values_from = total_p)

radar$per <- as.character(radar$per)

#radar$blue <- as.numeric(radar$blue)
#radar$green <- as.numeric(radar$green)
#radar$Orange <- as.numeric(radar$Orange)
#radar$Purple <- as.numeric(radar$Purple)
#radar$Yellow <- as.numeric(radar$Yellow)


radar[, 1] <- paste0("Person",1:5) # paste0("Person", 1:3)
colnames(radar) <- c("Group", "Blue", "Green", "Orange", "Purple", "Yellow")
ggradar(radar)


