data <- read.table('Data Tables/MTS1ClinicTesting.txt', 
                   header = TRUE, sep = "|", fill = TRUE, stringsAsFactors = FALSE)

# View the first few rows
head(data)

patient <- read.table("Data Tables/MTS1PtFinalStat.txt", 
                      header = TRUE, sep = "|", fill = TRUE, stringsAsFactors = FALSE)

head(patient) 
  
