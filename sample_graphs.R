
# master data
cs <- data.frame(col = c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5), strID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))

# top 5 strenght by people
strng <- data.frame(per = c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3, 4,4,4,4,4, 5,5,5,5,5)
                    ,team = c("A","A","A","A","A","B","B","B","B","B","A","A","A","A","A","A","A","A","A","A","B","B","B","B","B")
                    ,order = c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
                    ,clif =c(9,10,1,4,5, 2,5,1,13,2, 6,1,2,15,2, 9,1,11,12,4, 1,15,4,2,3)
                    
)
#adding rowID
strng$rnid<-seq(nrow(strng))


stcl = data.frame (
  row.names = c("Person.1", "Person.2", "Person.3", "Person.4", "Person.5"),
  Achiever =    c(4, 12, 3, 15, 1),
  Analytical =  c(1, 5,  2, 11, 8),
  Woo =         c(3, 6, 12, 2, 4),
  Relator=      c(7, 2, 4 , 1, 2),
  Responsib =   c(9, 9, 1,  6, 12),
  Communicat =  c(6, 10, 6, 5, 3)
)


max_min <- data.frame(
  Achiever = c(15, 1), Analytical = c(15, 1), Woo = c(15, 1),
  Relator = c(15, 1), Responsib = c(15, 1), Communicat = c(15, 1)
)


rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min, stcl)
df


Person1_data <- df[c("Max", "Min", "Person.1"), ]
radarchart(Person1_data)


create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}


op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(Person1_data, caxislabels = c(0, 5, 10, 15, 20))
par(op)



op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df, caxislabels = c(0, 5, 10, 15, 20),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(df[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)




### Sample 2
cs<- data.frame (name  = c("tom", "tom", "tom", "tom", "tom", "anna","anna","anna","anna","anna"),
                 order_cs = c(1,2,3,4,5, 1,2,3,4,5),
                 team = c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"),
                  cs = c("Learner", "Intellection", "Significance", "Maximizer", "Empathy",
                         "Discipline","Futuristic","Communication","Achiever", "Learner"),
                 col_cs = c("Green", "Green", "Orange", "Magenta", "Blue",
                            "Magenta", "Green", "Orange", "Magenta", "Green")
)

cs

# Visualising
# install.packages("waffle")
library(waffle)
library(ggplot2)

parts <- c(1, 1, 1, 1, 1)
chart <- waffle(parts, rows=1)
chart


library(readr)
#  devtools::install_github("liamgilbey/ggwaffle")
# install.packages("waffle", "readr", "ggpubr")

library(ggwaffle)
library(ggpubr)
library(ggimage)
theme_set(theme_pubr())

link <- ("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/ggwaffledata_mf.csv")
bacon_waf <- read_csv(link)

# have a look at data
View(bacon_waf)

# basic plot with geom_waffle() from ggwaffle package
ggplot(bacon_waf, aes(x, y, fill = bacon)) + 
  geom_waffle()


install.packages("personograph")
library(personograph)
data <- list(first=0.06, second=0.94)
personograph(data,  colors=list(first="black", second="#efefef"),
             fig.title = "100 people who do not eat bacon",
             draw.legend = FALSE, dimensions=c(5,20))


### Testing visualization

# install.packages("GGally")

library(GGally)

# Data set is provided by R natively
data <- iris

# Plot
ggparcoord(data,
           columns = 1:4, groupColumn = 5
) 


# cs transpose

cs2 <- cs[,c(1,4)]
cs2_t <- cs2 %>% mutate(value = 1)  %>% spread(cs, value,  fill = 0 ) 

ggparcoord(cs2_t,
           columns = 2:10, groupColumn = 1
) 


cs3 <- cs[, c(1,2,4)]

cs3_t <- cs3 %>% mutate(value = 1)  %>% spread(cs, order_cs,  fill = 0) 
cs3_t <- cs3_t[, c(1,3:11)]

ggparcoord(cs3_t,
           columns = 2:10, groupColumn = 1) 
