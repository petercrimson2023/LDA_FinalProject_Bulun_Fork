
AdvEvent <- read.table("Data Tables/MTS1AdvEvent.txt", 
                       header = TRUE, sep = "|", fill = TRUE, stringsAsFactors = FALSE)

head(AdvEvent) 


VisitData <- read.table('Data Tables/MTS1ClinicTesting.txt', 
                        header = TRUE, sep = "|", fill = TRUE, stringsAsFactors = FALSE)

# View the first few rows
head(VisitData)

PtRoster <- read.table("Data Tables/MTS1PtRoster.txt", 
                       header = TRUE, sep = "|", fill = TRUE, stringsAsFactors = FALSE)

head(PtRoster) 


# Y = SER for patient i at time j -> calculated by SE = S + C/2 -> MTS1ClinicTesting.txt,
# S_OS = mean(AutoRef1SphOS, AutoRef2SphOS, AutoRef3SphOS)
# C_OS = mean(AutoRef1CylOS, AutoRef2CylOS, AutoRef3CylOS)
# 
# S_OD = mean(AutoRef1SphOD, AutoRef2SphOD, AutoRef3SphOD)
# C_OD = mean(AutoRef1CylOD, AutoRef2CylOD, AutoRef3CylOD)

# time = timei -> 'Visit' -> MTS1ClinicTesting.txt (long format)
# group = group/treatment assignment of time j -> MTS1PtRoster.txt 'TrtGroup'
# Investigating variable: Parent status of myopia (is_mother_has_myopia -> 'MTS1DemoMedOcuHx.txt'$MotherMyop, is_father_has_myopia -> 'MTS1DemoMedOcuHx.txt'$FatherMyop) 
# Adjusted variable: 
# baseline SER, age (-> MTS1PtRoster.txt 'AgeAsOfEnrollDt'), iris color (brown or non-brown) (->'MTS1DemoMedOcuHx.txt' $ 'EyeColor'),  East Asian vs. non-East Asian race -> (->'MTS1DemoMedOcuHx.txt' $ 'EyeColor'),


