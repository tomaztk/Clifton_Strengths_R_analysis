
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
