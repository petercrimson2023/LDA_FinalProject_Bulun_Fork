# Load VisitData and PtRoster datasets
VisitData <- read.table("Data Tables/MTS1ClinicTesting.txt", 
                        header = TRUE, sep = "|", fill = TRUE, stringsAsFactors = FALSE)
head(VisitData)

PtRoster <- read.table("Data Tables/MTS1PtRoster.txt", 
                       header = TRUE, sep = "|", fill = TRUE, stringsAsFactors = FALSE)
head(PtRoster)

# Load the additional dataset for parental myopia and other demographic information
DemoMedOcuHx <- read.table("Data Tables/MTS1DemoMedOcuHx.txt", 
                           header = TRUE, sep = "|", fill = TRUE, stringsAsFactors = FALSE)
head(DemoMedOcuHx)

# Step 1: Calculate SER for each eye
# Left Eye (OS)
VisitData$S_OS <- rowMeans(VisitData[, c("AutoRef1SphOS", "AutoRef2SphOS", "AutoRef3SphOS")], na.rm = TRUE)
VisitData$C_OS <- rowMeans(VisitData[, c("AutoRef1CylOS", "AutoRef2CylOS", "AutoRef3CylOS")], na.rm = TRUE)
VisitData$SER_OS <- VisitData$S_OS + (VisitData$C_OS / 2)

# Right Eye (OD)
VisitData$S_OD <- rowMeans(VisitData[, c("AutoRef1SphOD", "AutoRef2SphOD", "AutoRef3SphOD")], na.rm = TRUE)
VisitData$C_OD <- rowMeans(VisitData[, c("AutoRef1CylOD", "AutoRef2CylOD", "AutoRef3CylOD")], na.rm = TRUE)
VisitData$SER_OD <- VisitData$S_OD + (VisitData$C_OD / 2)

# Step 2: Average SER across both eyes for each visit (i.e., time j)
VisitData$SER <- rowMeans(VisitData[, c("SER_OS", "SER_OD")], na.rm = TRUE)

# Step 3: Merge data with PtRoster and DemoMedOcuHx datasets
# Merging VisitData with PtRoster on Patient ID
MergedData <- merge(VisitData, PtRoster, by = "PtID")

# Merging the result with DemoMedOcuHx on Patient ID
MergedData <- merge(MergedData, DemoMedOcuHx, by = "PtID")

# Step 4: Create necessary variables
# Time (Visit) is already in VisitData as 'Visit'
# Group/Treatment assignment
MergedData$TreatmentGroup <- MergedData$TrtGroup

# Parental myopia status (investigating variables)
MergedData$is_mother_has_myopia <- MergedData$MotherMyop
MergedData$is_father_has_myopia <- MergedData$Father