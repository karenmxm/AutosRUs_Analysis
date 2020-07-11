# Read MechaCar_mpg dataset
MechaCar_mpg <- read.csv(file = 'MechaCar_mpg.csv',
                         check.names = F,
                         stringsAsFactors = F)
head(MechaCar_mpg)

# Change column names
colnames(MechaCar_mpg)[1] <- "vehicle_length"
colnames(MechaCar_mpg)[2] <- "vehicle_weight"
colnames(MechaCar_mpg)[3] <- "spoiler_angle"
colnames(MechaCar_mpg)[4] <- "ground_clearance"

#get correlation coefficient matrix
cor_matrix <- cor(MechaCar_mpg)

#generate multiple linear regression model
lm(mpg ~ vehicle_length + vehicle_weight + ground_clearance + AWD,
   data=MechaCar_mpg)

#generate summary statistics
summary(lm(mpg ~ vehicle_length + vehicle_weight + ground_clearance + AWD,
           data=MechaCar_mpg)) 


#Read Suspension_Coil dataset
Suspension_Coil <- read.csv(file = 'Suspension_Coil.csv',
                            check.names = F,
                            stringsAsFactors = F)

#summary statistics for Suspension_Coil PSI
summary(Suspension_Coil$PSI)
var(Suspension_Coil$PSI)
sd(Suspension_Coil$PSI)

#one sample variance test
VarTest(Suspension_Coil$PSI,
        alternative = c("less"),
        sigma.squared = 100)

#compare means of data and 1,500
t.test(log10(Suspension_Coil$PSI),mu = 1500) 





