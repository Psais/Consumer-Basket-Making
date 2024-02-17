library(dplyr)
library(haven)
library(ggplot2)
library(rstatix)
library(caret)
library(ineq)

split_row_half <- function(data, row_index) {
  original_row <- data[row_index, ]
  new_row1 <- c("0-4", original_row$Persons / 2)
  new_row2 <- c("4-9", original_row$Persons / 2)
  data <- rbind(new_row1, new_row2, data[-row_index, ])
  data$Persons <- as.numeric(data$Persons)
  return(data)
}

census_dat_init <- read.csv("C:/Users/Manas/Downloads/Census_Data_2001.csv")
census_dat <- census_dat_init[,1:9]
dist_dat <- split(census_dat, census_dat$Distt.)




dependency_ratio_table <- data.frame()


for (name in names(dist_dat)) {
  
  current_dat <- dist_dat[[name]]
  urban_data <- current_dat[current_dat$Popn_Type == "Urban", ][2:17,]
  rural_data <- current_dat[current_dat$Popn_Type == "Rural", ][2:17,]
  total_data <- current_dat[current_dat$Popn_Type == "Total", ][2:17,]
 
  coltotal <- c("Age.group", "Persons")
  
  work_urban <- urban_data[,coltotal]  
  work_rural <- rural_data[,coltotal]
  work_total <- total_data[,coltotal]
  
  work_rural1 <-  split_row_half(work_rural, 1)
  work_urban1 <-  split_row_half(work_urban, 1)
  work_total1 <-  split_row_half(work_total, 1)

  
  work_total1$Age.group <- factor(work_total1$Age.group, 
                                  levels = work_total1$Age.group, 
                                  ordered = TRUE)
  work_total1$Persons <- work_total1$Persons/sum(work_total1$Persons)
  
  work_urban1$Age.group <- factor(work_urban1$Age.group, 
                                  levels = work_urban1$Age.group, 
                                  ordered = TRUE)
  work_urban1$Persons <- work_urban1$Persons/sum(work_urban1$Persons)
  
  work_rural1$Age.group <- factor(work_rural1$Age.group, 
                                  levels = work_rural1$Age.group, 
                                  ordered = TRUE)
  work_rural1$Persons <- work_rural1$Persons/sum(work_rural1$Persons)
  
  width <- rep(0.05,17)
  main_rural <- paste("Rural-Age-Distribution:", total_data$Area.Name[1])
  main_tot <- paste("Total-Age-Distribution:", total_data$Area.Name[1])
  main_urban <- paste("Urban-Age-Distribution:", total_data$Area.Name[1])
  
  
  
  barplot(work_rural1$Persons, names.arg = work_rural1$Age.group, 
          col = "skyblue",
          xlab = "Class Intervals", 
          ylab = "Frequency", border = "black", 
          density = 20, width = width, main = main_rural)
  
  barplot(work_urban1$Persons, names.arg = work_urban1$Age.group, 
          col = "pink",
          xlab = "Class Intervals", 
          ylab = "Frequency", border = "black", 
          density = 20, width = width, main = main_urban)
  
  barplot(work_total1$Persons, names.arg = work_total1$Age.group, 
          col = "green",
          xlab = "Class Intervals", 
          ylab = "Frequency", border = "black", 
          density = 20, width = width, main = main_tot)
  
  non_dependents1 <- work_total[3:12,]
  total_dependency_ratio <- 
    (sum(work_total$Persons) - 
       sum(non_dependents1$Persons))/sum(non_dependents1$Persons)
  
  non_dependents2 <- work_urban[3:12,]
  urban_dependency_ratio <- 
    (sum(work_urban$Persons) - 
       sum(non_dependents2$Persons))/sum(non_dependents2$Persons)
  
  non_dependents3 <- work_rural[3:12,]
  rural_dependency_ratio <- 
    (sum(work_rural$Persons) -
       sum(non_dependents3$Persons))/sum(non_dependents3$Persons)
  
  new_entry <- data.frame(District = total_data$Area.Name[1], 
                          Urban_Dependency_Ratio = urban_dependency_ratio, 
                          Rural_Dependency_Ratio = rural_dependency_ratio, 
                          Total_Dependency_Ratio = total_dependency_ratio, 
                          Dependent_Popn = (sum(work_total$Persons) - 
                                              sum(non_dependents1$Persons)), 
                          Non_Dependents = sum(non_dependents1$Persons))
  
  dependency_ratio_table <- rbind.data.frame(dependency_ratio_table, new_entry)
  
}

Total_DR <- 
  sum(dependency_ratio_table$Dependent_Popn)/sum(dependency_ratio_table$Non_Dependents)

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", 
                             full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", 
                              full.names = TRUE)

file.copy(from=plots.png.paths, to="C:/Users/Manas/Downloads/Eco_Plots")

plots.png.detials <- file.info(plots.png.paths)
plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime),]
sorted.png.names <- gsub(plots.dir.path, "C:/Users/Manas/Downloads/Eco_Plots", 
                         row.names(plots.png.detials), fixed=TRUE)
numbered.png.names <- paste0("C:/Users/Manas/Downloads/Eco_Plots/", 
                             1:length(sorted.png.names), ".png")

# Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
file.rename(from=sorted.png.names, to=numbered.png.names)

write.csv(dependency_ratio_table, file = "C:/Users/Manas/Downloads/Eco_Plots/dependency.csv", row.names = FALSE)
