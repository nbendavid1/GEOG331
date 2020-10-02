# # # Davi Bendavid
# # # GEOG331
# # # Activity 4
# # # 10/2/2020

#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length (1) x width (2)
#2. iris  petal length (3) x width (4)
#3. iris sepal length (1) x petal length (2)


#pick out only versicolor species 
versi <- iris[iris$Species == "versicolor",]

#assign index values for for loop
y <- c(versi[1], versi[3], versi[1])
x <- c(versi[2], versi[2], versi[4])

rt <- list()

#create regressions
for (i in 1:3){
  rt[[i]] <- lm(y[[i]] ~ x[[i]])
  
  }



#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
					Height.cm = c(60,100,11.8))

#use left_join to add height column
newIris<-left_join(iris,height)


#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot

#first make blank plot
ggplot(iris, 
       aes(x=Sepal.Length, 
           y=Sepal.Width)) 

#3b. make a scatter plot with ggplot and get rid of  busy grid lines

#add data as scatter plot
ggplot(iris, 
       aes(x=Sepal.Length, 
           y=Sepal.Width)) +
  geom_point()

#remove gridlines
ggplot(iris, 
       aes(x=Sepal.Length, 
           y=Sepal.Width)) +
  geom_point()+
  theme_classic()


#3c.make a scatter plot with ggplot and get rid of grid lines,

#show species by color, and increase the point size
ggplot(iris, 
       aes(x = Sepal.Length, 
           y = Sepal.Width)) +
  geom_point(aes(x = Sepal.Length, 
                 y = Sepal.Width, 
                 color = Species, 
                 size = 2)) +
  theme_classic()


#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################

#   The arguments in plot are definitely much more simplistic than the arguments
#   in ggplot. This makes plot easier to understand but also significantly limits 
#   the amount of control the user has over the plot. The arguments in ggplot 
#   require more code and are a bit more complicated, but allow for a lot more control 
#   over every element of the plot. Ggplot also produces plots that are easier to read
#   and interpret and are much more aesthetically pleasing.


