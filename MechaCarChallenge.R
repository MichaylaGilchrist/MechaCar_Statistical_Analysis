#Part 1: Linear Regression to Predict MPG
# Read csv
library(readr)
MechaCar_mpg <- read_csv("MechaCar_mpg.csv")

Model = lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = MechaCar_mpg)
summary(Model)

#Part 2: Visualizations for the Trip Analysis
#Read csv
library(readr)
Suspension_Coil <- read_csv("Suspension_Coil.csv")

#Create total summary table
total_summary = summarize(Suspension_Coil, Mean=mean(PSI), median(PSI), var(PSI), sd(PSI))
total_summary

#Create summary table by lot number
lot_summary <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
lot_summary

#Part 3. T-Tests on Suspension Coils overall and by Lot Number
#T-test for all PSI readings 
t.test(Suspension_Coil$PSI, mu=1500)

#T-test for PSI readings per lot
t.test(subset(Suspension_Coil, Manufacturing_Lot=="Lot1")$PSI, mu=1500)
t.test(subset(Suspension_Coil, Manufacturing_Lot=="Lot2")$PSI, mu=1500)
t.test(subset(Suspension_Coil, Manufacturing_Lot=="Lot3")$PSI, mu=1500)