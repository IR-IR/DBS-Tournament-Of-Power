#This project is about Dragonball Super's Tournament of Power Eliminations

# This part is code for how many elimination each universe has
Tourney_Transcript <- read.csv("C:/Users/iftia/OneDrive/Documents/Iftiar's R Projects/DBS Tourney/DBS Tournament of Power.csv")
Tourney_Transcript 
#Since some competitors needed 2 or more people to eliminate them, some part of the file has NA
Elimination_Data_Frame <- data.frame(Universe = Tourney_Transcript$Universe, Eliminated_By_Universe = Tourney_Transcript$Eliminator_Universe)
Elimination_Data_Frame #shows which universe an eliminated competitor is from and which universe they were eliminated by
New_Elimination <- na.omit(Elimination_Data_Frame) #omits all the NAs
New_Elimination # clean data frame that shows who was out by who, universe wise
Eliminated_Table <- table(New_Elimination)
Eliminated_Table #table that shows how many from Universe X was eliminated by competitors from Universe Y
library(dplyr)
X <- dplyr::group_by(Tourney_Transcript, Universe) #transcript of competitor name, universe, eliminated by, and eliminator's universe
Y <- data.frame(X$Eliminator_Universe) #only shows the universe the eliminator is from
Y <- Y[!apply(is.na(Y) | Y == "", 1, all), ] #removes all empty values
Z <- table(Y)
Z #table that shows how many eliminations each universe has 
PreliminaryCount <- data.frame(Z)
PreliminaryCount
Final_Universe_Count <- PreliminaryCount[-c(1),] #to get rid of the "empty" row that has no universe
Final_Universe_Count 
library(ggplot2)
Elimination_By_Universe <- ggplot(data = Final_Universe_Count, aes(Final_Universe_Count$Y, Final_Universe_Count$Freq)) +
  xlab("Universe") + ylab("Total Eliminations") + geom_col(data = Final_Universe_Count, inherit.aes = TRUE, position = "stack",
                                                           col = "red", fill = "orange") + 
  ggtitle("Number of Fighters Each Universe Eliminated\n in DBS Tournament of Power") +
  scale_y_continuous(breaks = seq(0,50,10)) + geom_text(label = Final_Universe_Count$Freq, vjust = -.25) +
  theme(plot.title = element_text(size = 12))
Elimination_By_Universe
#Line 15 created a data frame to show which universe is the eliminator from
#Universe 3 and 10 did not have a single elimination, therefore won't be on X (the data frame) and will be missing in graph

#This part of the project is about collab vs non collab eliminations 
Collaboration <- dplyr::group_by(Tourney_Transcript, Collab)
Collaboration
Collab_DF <- data.frame(Collaboration$Collab)
Collab_DF <- Collab_DF[!apply(is.na(Collab_DF) | Collab_DF == "", 1, all), ]
Collab_DF
CleanedUp <- data.frame(Collab_DF)
CleanedUp
Table_Collab <- table(CleanedUp)
Table_Collab
Collaborated_DF <- data.frame(Table_Collab)
Collaborated_DF
Final_Collab_Count <- Collaborated_DF[-c(1), ]
Final_Collab_Count
Collab_NonCollab <- ggplot(data = Final_Collab_Count, aes(Final_Collab_Count$CleanedUp, Final_Collab_Count$Freq)) + 
  xlab("Was At Least Two People Needed?") + ylab ("Number of Occurances") + 
  ggtitle("Tournament of Power:\n Collaboarted vs \n Non-Collaborated \n Eliminations") +
  geom_col(data = Final_Collab_Count, inherit.aes = TRUE, col = "red", fill = "orange") + 
  theme(plot.title = element_text(size = 12)) + geom_text(label = Final_Collab_Count$Freq, vjust = 1)
Collab_NonCollab