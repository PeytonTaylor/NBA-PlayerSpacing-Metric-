setwd("~/Desktop/Desktop/Development/athena_location_scrapes")
# library(anytime)
# library(sqldf)
# library(plotly)
# library(data.table)
# install.packages("gapminder")
# library(gapminder)
# library(measurements)
# library(dplyr)
# library(tidyr)
# library(graphics)
# library(sp)
# options(digits=15)

##### sets court dimensions ####### 
# (0,0) middle of the court

fullcourt_min_x = 0- conv_unit(25, "ft", "mm") # x == -7620 , 7620
fullcourt_max_x = conv_unit(25, "ft", "mm") 
fullcourt_min_y = 0-conv_unit(47, "ft", "mm") # y == -14325.6, 14325.6
fullcourt_max_y = conv_unit(47, "ft", "mm")

###################################


file<-read.csv("e487c9cc-99d8-42f8-a279-162d5904a64c.csv")

get_convex_hull(file)


#####
get_convex_hull<- function(file){
  
#order by timestamps/ select columns
file<-sqldf(" SELECT timestamp, clock_time, sequence, x, y, player_id, player_team_id, id, session_id
                  FROM data
                      ORDER BY clock_time")

data$seconds<-(-difftime(data$clock_time[1], data$clock_time)) #get seconds
data$seconds<- as.numeric(data$seconds)
data<-data %>% distinct() #delete duplicates

# binary all players on/off 
all.players=data
all.players$on.off<-ifelse(all.players$x > fullcourt_min_x & all.players$x< fullcourt_max_x & all.players$y > fullcourt_min_y & all.players$x < fullcourt_max_y, 1, 0)
all.players<-data.table::dcast(setDT(all.players), timestamp + id + session_id + clock_time + seconds ~ player_id , value.var = c("on.off"))


# filters timestamps w 10 players only
in.players<-data %>% filter(x > fullcourt_min_x & x< fullcourt_max_x, y > fullcourt_min_y & x < fullcourt_max_y) # all players within court boundries
in.players<-in.players %>% # seconds w 10
  group_by(seconds) %>% 
  filter(n() == 10)

# team id to team 'a' and 'b' for convex hull assignments
in.players$team<-ifelse(transform(in.players,id=as.numeric(factor(player_team_id)))["id"] == 1, 'a', "b")

# add player_number 1-5 per team
in.players <-in.players %>% 
  arrange(seconds,player_team_id)
in.players$player_num<-rep(seq(1,5), length.out= in.players) #sequence out
in.players<-unite(in.players, "player", c("team","player_num")) #create player column


# filters half court possessions w >= 3 players per team
half1<-in.players %>% 
  filter(x > fullcourt_min_x & x < fullcourt_max_x & y > 0 & y < fullcourt_max_y ) %>% 
  group_by(timestamp, seconds, player_team_id) %>% 
  tally() %>% 
  filter(n>=3)

half2<-in.players %>% 
  filter(x > fullcourt_min_x & x < fullcourt_max_x &  y > fullcourt_min_y & y < 0) %>% 
  group_by(timestamp, seconds, player_team_id) %>% 
  tally() %>% 
  filter(n>=3) %>%
  filter()

halfcourt_ts<- distinct(data.frame("timestamp"= rbind(half2, half1)$timestamp)) # all timestamps with >=3 players per team
halfcourt_poss<-subset(in.players, timestamp %in% halfcourt_ts$timestamp) #match in.players to halfcourt ts
  
#tranform to format for convex 
vex.data<-data.table::dcast(setDT(halfcourt_poss), timestamp + id + session_id + clock_time + seconds + sequence ~ player , value.var = c("player_team_id", "player_id","x","y")) #cast from long to wide format
vex.data$sequence<-NULL 
vex.data<-vex.data %>% group_by(seconds)  %>% summarise_all(~first(na.omit(.))) #combine player ids/team ids into one row w/o NAs

####### convex loop #######

area.data<-data.frame(id= vex.data$id, timestamp = vex.data$timestamp, a.area = NA, b.area = NA) #creates empty dataframe for results
i = 1 

for (i in 1:nrow(vex.data)){
  b1 <- vex.data[i,c("clock_time","x_b_1", "y_b_1")] #takes individual player1 to dataframe
  colnames(b1) = c("time","x", "y")
  a1 <- vex.data[i,c("clock_time","x_a_1", "y_a_1")]
  colnames(a1) = c("time","x", "y")
  
  b2<- vex.data[i,c("clock_time","x_b_2", "y_b_2")]
  colnames(b2) = c("time","x", "y")
  a2<- vex.data[i,c("clock_time","x_a_2", "y_a_2")]
  colnames(a2) = c("time","x", "y")
  
  b3 <-vex.data[i,c("clock_time","x_b_3", "y_b_3")]
  colnames(b3) = c("time","x", "y")
  a3 <- vex.data[i,c("clock_time","x_a_3", "y_a_3")]
  colnames(a3) = c("time","x", "y")
  
  b4 <- vex.data[i,c("clock_time","x_b_4", "y_b_4")]
  colnames(b4) = c("time", "x", "y")
  a4 <- vex.data[i,c("clock_time","x_a_4", "y_a_4")]
  colnames(a4) = c("time", "x", "y")
  
  b5 <- vex.data[i,c("clock_time","x_b_5", "y_b_5")]
  colnames(b5) = c("time","x", "y")
  a5 <- vex.data[i,c("clock_time","x_a_5", "y_a_5")]
  colnames(a5) = c("time","x", "y")
  
  b.frame<- rbind(b1, b2, b3, b4, b5) #combine subset of dataframes
  b.frame<- na.omit(b.frame) #remove NAs from each dataframe
  a.frame<- rbind(a1, a2, a3, a4, a5)
  a.frame<- na.omit(a.frame)
 
  b.ch2 <- chull(b.frame[,2:3])  #get points of convex hull
  b.ch3 <- c(b.ch2, b.ch2[1])
  
  a.ch2 <- chull(a.frame[,2:3])
  a.ch3 <- c(a.ch2, a.ch2[1])
  
  # get area of convex hull
  b.chull.coords <- b.frame[b.ch3, 2:3] #gets just x and y from chull w/o time column
  a.chull.coords <- a.frame[a.ch3, 2:3]
  
  b.chull.poly <- Polygon(b.chull.coords, hole=F)  #from the package sp
  b.chull.area <- b.chull.poly@area
  a.chull.poly <- Polygon(a.chull.coords, hole=F) 
  a.chull.area <- a.chull.poly@area
  
  # store results in empty dataframe                                                         
  area.data$b.area[i]<-conv_unit(b.chull.area, "mm2", "ft2") #convert to ft2
  area.data$a.area[i]<-conv_unit(a.chull.area, "mm2", "ft2") 
}

###### end of convex loop ########

names(area.data)[names(area.data) == "a.area"] <- paste0(as.data.frame(select(vex.data,contains("player_team_id_a"))[1])[1,1], ".area") #change names to team area
names(area.data)[names(area.data) == "b.area"] <- paste0(as.data.frame(select(vex.data,contains("player_team_id_b"))[1])[1,1], ".area") 

#merge of both players on/off and area data
 results<- merge(x = all.players, y = area.data, by = c("timestamp", "id"))
 
 return(as.data.frame(results))
 
 #write.csv(results, file = "results.csv")

 }
######End of function ##########

get_convex_hull(file)



## full game animation 
game <- in.players %>%
  plot_ly(
    x = ~x,
    y = ~y,

    color = ~player_team_id,
    frame = ~seconds,
    text = ~player_id,
    type = 'scatter',
    mode = 'markers' ,
    hoverinfo = ""
  ) %>% layout(
             shapes = list(type = "rect",
                           fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                           x0 = fullcourt_min_x, x1 = fullcourt_max_x, xref = "0",
                           y0 = fullcourt_min_y, y1 = fullcourt_max_y, yref = "0"))

plot(game)




