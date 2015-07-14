library(Gmisc)
library(magrittr)
library(RColorBrewer)


#Make trans matrix

#mat.or.vec is a matrix (or vector) of all zeros
trans2<-mat.or.vec(5,5)
#This next line defines the non-zero elements. Here they are all in the second row.
trans2[2,]<-c(79, 1884+824+82, 699+313+201, 181+54, 0)
#Change to a table and label the rows and columns
trans2<-as.table(trans2)
row.names(trans2)<-c("Underweight","Healthy","Overweight","Obese","Severely Obese")
colnames(trans2)<-row.names(trans2)

#Below is a bit of Gmisc code to make a Gmisc class from the transition matrix trans2
#Here we label the two transitions by ages
transitions <- trans2 %>% getRefClass("Transition")$new(label=c("Age 17","Age 22"))

#Color changes
#Black text
transitions$txt_clr<-c("black")
#Choose a divergent palette from Brewer
#The funny indices are there so that the white ends up as the neutral healthy color
myPal<-c(brewer.pal(3,"RdYlGn")[3:2],brewer.pal(7,"RdYlGn")[3:1])
#The above goes from green for underweight to red for overweight.
#To go the other way use
#myPal<-c(brewer.pal(3,"RdYlGn")[1:2],brewer.pal(7,"RdYlGn")[5:7])
#To see other palettes use
#display.brewer.all(type="div")

transitions$fill_clr<-matrix(myPal,nrow = 5,ncol = 5)
transitions$render()


#Make second set of transitions
trans4<-mat.or.vec(5,5)
trans4[1,2]<-79
trans4[2,2:4]<-c(1881,824,82)
trans4[3,2:4]<-c(201,699,313)
trans4[4,3:4]<-c(54,181)
trans4[2,2:4]<-c(1884,824,82)

row.names(trans4)<-row.names(trans2)
colnames(trans4)<-row.names(trans2)

#This is the code that adds the second set of transitions
transitions$addTransitions(trans4,label="Age 27")

transitions$render()


