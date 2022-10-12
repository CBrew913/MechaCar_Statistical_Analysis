library(dplyr) #load dplyr package

mc_mpg <- read.csv("MechaCar_mpg.csv") #read in the mpg dataset

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mc_mpg) #perform linear regression analysis
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mc_mpg)) # summarize linear regression

suscoil <- read.csv("Suspension_Coil.csv") #read in the suspension dataset

total_summary <- suscoil %>% #create PSI summary table
  summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = "keep")

lot_summary <- suscoil %>% #create PSI summary table grouped by lot
  group_by(Manufacturing_Lot) %>%
  summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = "keep")

t.test(log10(suscoil$PSI), mu=mean(log10(suscoil$PSI))) #compare sample versus population means

t.test(log10(suscoil$PSI), subset = Manufacturing_Lot=="Lot 1", mu=mean(log10(suscoil$PSI))) #compare sample versus population means for each lot
t.test(log10(suscoil$PSI), subset = Manufacturing_Lot=="Lot 2", mu=mean(log10(suscoil$PSI)))
t.test(log10(suscoil$PSI), subset = Manufacturing_Lot=="Lot 3", mu=mean(log10(suscoil$PSI)))

