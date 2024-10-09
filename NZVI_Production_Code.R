{r}
#Normalize Function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#For reading NOMIS data, Specify the rows to read
start_row <- 9
end_row <- 371

# Calculate the number of rows to skip before reading data
num_rows_to_skip <- start_row - 1  # minus 1 because skip is 1-based index

# Calculate the number of rows to read
num_rows_to_read <- end_row - start_row + 1
```

Libraries
```{r}
library(tidyverse)
library(Matrix)
library(sf)
```

Score Calculations
1 digit
Economic Complexity 
```{r Construct Location Quotient Matrix}
#Read BRES percentages Data, with special rules to deal with formatting of columns and renaming semicolons, adding X to beginning of industry column names to help reference later
LAD <-  read.csv("Data\\Business Register and Employment Survey\\2022 LAD 1 Digit Percentages.csv", skip = num_rows_to_skip, nrows = num_rows_to_read, blank.lines.skip = TRUE, fileEncoding = "Latin1", check.names = F) %>%
  select(where(~!all(is.na(.)))) %>%
  select(where(~!all(. == ""))) %>%
  rename(Area = 1, ladcode = 2)%>%
  rename_with(~ paste0("X", .), -(1:2)) %>%
  rename("XO : Public administration and defence and compulsory social security" = "XO : Public administration and defence; compulsory social security",
         "XG : Wholesale and retail trade and repair of motor vehicles and motorcycles" = "XG : Wholesale and retail trade; repair of motor vehicles and motorcycles")

# Convert the dataset to long format
LAD_long <- pivot_longer(LAD, cols = starts_with("X"), names_to = "Industry", values_to = "Employment")

# Calculate total employment for each local authority
total_emp_local <- LAD_long %>%
  group_by(Area) %>%
  summarize(TotalEmpLocal = sum(Employment))

# Calculate total employment for each industry at the national level
total_emp_industry <- LAD_long %>%
  group_by(Industry) %>%
  summarize(TotalEmpIndustry = sum(Employment))

# Calculate the total employment at the national level
total_emp_national <- sum(LAD_long$Employment)

# Merge total employment for local authorities
LAD_long <- LAD_long %>%
  left_join(total_emp_local, by = "Area")

# Merge total employment for industries
LAD_long <- LAD_long %>%
  left_join(total_emp_industry, by = "Industry")

# Add national total employment to the data frame
LAD_long <- LAD_long %>%
  mutate(TotalEmpNational = sum(Employment))

# Calculate location quotient
LAD_long <- LAD_long %>%
  mutate(LQ = (Employment / TotalEmpLocal) / (TotalEmpIndustry / TotalEmpNational))

# Pivot the data back to matrix with LQ values
LQ_matrix <- LAD_long %>%
  select(Area, Industry, LQ) %>%
  pivot_wider(names_from = Industry, values_from = LQ) %>%
  replace(is.na(.), 0)  # Replace NA values with 0
```

```{r Create Local Authority Similarity Matrix}
# Convert the LQ values to a binary M matrix
binary_M <- as.data.frame(LQ_matrix %>% select(-Area) > 1)
row.names(binary_M) <- LQ_matrix$Area

# Calculate local authority diversity vector D
D <- rowSums(binary_M)
D[D == 0] <- 1 # Handle zero diversity

# Calculate industry ubiquity vector U
U <- colSums(binary_M)
U[U == 0] <- 1 # Handle zero ubiquity

# Create diagonal matrices
D_diag <- diag(1 / D)
U_diag <- diag(1 / U)

# Compute the local authority similarity matrix M
M_matrix <- as.matrix(binary_M)
LAD_similarity_matrix <- D_diag %*% M_matrix %*% U_diag %*% t(M_matrix)
```

```{r Calculate Economic and Product Complexity Index}
# Calculate the eigenvalues and eigenvectors for the ECI
eci_eigen <- eigen(LAD_similarity_matrix)
eci_values <- eci_eigen$values
eci_vectors <- eci_eigen$vectors

# Find the second largest eigenvalue and its corresponding eigenvector
eci_second_eigenvector <- eci_vectors[, order(eci_values, decreasing = TRUE)[2]]

# Display the ECI (second largest eigenvector)
print("ECI (Second largest eigenvector):")
print(eci_second_eigenvector)

# Calculate the industry similarity matrix for PCI
industry_similarity_matrix <- U_diag %*% t(M_matrix) %*% D_diag %*% M_matrix

# Display the industry similarity matrix
print(industry_similarity_matrix)

# Calculate the eigenvalues and eigenvectors for the PCI
pci_eigen <- eigen(industry_similarity_matrix)
pci_values <- pci_eigen$values
pci_vectors <- pci_eigen$vectors

# Find the second largest eigenvalue and its corresponding eigenvector
pci_second_eigenvector <- pci_vectors[, order(pci_values, decreasing = TRUE)[2]]

# Create the ECI data frame
LAD_ECI <- data.frame(
  Area = row.names(binary_M),
  ECI = eci_second_eigenvector
)

# Create the PCI data frame
LAD_PCI <- data.frame(
  Industry = colnames(binary_M),
  PCI = pci_second_eigenvector
)

write.csv(LAD_ECI, "Data\\LAD_ECI_1_Digit.csv")
```

Relatedness
```{r Calculate proximity matrix}
# Function to calculate proximity matrix
calculate_proximity <- function(binary_M) {
  num_industries <- ncol(binary_M)
  proximity_matrix <- matrix(0, nrow = num_industries, ncol = num_industries)
  colnames(proximity_matrix) <- colnames(binary_M)
  rownames(proximity_matrix) <- colnames(binary_M)
  
  for (i in 1:num_industries) {
    for (j in 1:num_industries) {
      if (i != j) {
        m_i_j <- sum(binary_M[, i] * binary_M[, j])
        m_i <- sum(binary_M[, i])
        m_j <- sum(binary_M[, j])
        proximity_matrix[i, j] <- min(m_i_j / m_i, m_i_j / m_j)
      }
    }
  }
  return(proximity_matrix)
}

# Calculate the proximity matrix
proximity_matrix <- calculate_proximity(binary_M)
```

Reliance
```{r Over reliance on one employer}
#Read BRES percentages data
LAD <-  read.csv("Data\\Business Register and Employment Survey\\2022 LAD 1 Digit Percentages.csv", skip = num_rows_to_skip, nrows = num_rows_to_read, blank.lines.skip = TRUE, fileEncoding = "Latin1", check.names = F) %>%
  select(where(~!all(is.na(.)))) %>%
  select(where(~!all(. == ""))) %>%
  rename(Area = 1, ladcode = 2)%>%
  rename_with(~ paste0("X", .), -(1:2)) %>%
  rename("XO : Public administration and defence and compulsory social security" = "XO : Public administration and defence; compulsory social security",
         "XG : Wholesale and retail trade and repair of motor vehicles and motorcycles" = "XG : Wholesale and retail trade; repair of motor vehicles and motorcycles")

#Read BRES counts data
LADct <-  read.csv("Data\\Business Register and Employment Survey\\2022 LAD 1 Digit Counts.csv", skip = num_rows_to_skip, nrows = num_rows_to_read, blank.lines.skip = TRUE, fileEncoding = "Latin1", check.names = F) %>%
  select(where(~!all(is.na(.)))) %>%
  select(where(~!all(. == ""))) %>%
  rename(Area = 1, ladcode = 2) %>%
  rename_with(~ paste0("X", .), -(1:2)) %>%
  rename("XO : Public administration and defence and compulsory social security" = "XO : Public administration and defence; compulsory social security",
         "XG : Wholesale and retail trade and repair of motor vehicles and motorcycles" = "XG : Wholesale and retail trade; repair of motor vehicles and motorcycles")

# Convert the dataset to long format
LADct_long <- pivot_longer(LADct, cols = starts_with("X"), names_to = "Industry", values_to = "Employmentct")

#Check for threshold of reliance visually:
percentages <- LAD[, 3:ncol(LAD)]
percentages <- as.data.frame(sapply(percentages, function(x) as.numeric(as.character(x))))
percentages_vector <- unlist(percentages)
percentages_vector <- na.omit(percentages_vector)

hist(percentages_vector, breaks = 50, col = "skyblue", xlab = "Percentage of Employees", main = "Histogram of Percentages")

###SET THRESHOLD FOR OVERRELIANCE HERE###
# Identify cells with value 10, excluding the first two columns
indices <- which(LAD[, -c(1, 2)] >= 10, arr.ind = TRUE)

# Adjust column indices to match the original dataframe
indices[, 2] <- indices[, 2] + 2

# Create a list with the corresponding area details and industries
result <- data.frame(
  AreaName = LAD[indices[, 1], 1],
  AreaCode = LAD[indices[, 1], 2],
  Industry = colnames(LAD)[indices[, 2]],
  Value = LAD[indices]
)

# Add a row number within each group of AreaName and AreaCode, join to counts
result <- result %>%
  group_by(AreaName, AreaCode) %>%
  mutate(row_num = row_number()) %>%
  rename(ladcode = AreaCode) %>%
  left_join(LADct_long, by = c("ladcode", "Industry")) 

#Normalize and weight reliance by percentages and counts
result$Value = as.numeric(result$Value)
result$Value = normalize(result$Value)
result$Value = result$Value + 1
result$Employmentct = normalize(result$Employmentct)
result$Employmentct = result$Employmentct + 1
result$Value = result$Value*result$Employmentct

# Reshape the dataframe to wide format
result_wide <- result %>%
  pivot_wider(
    names_from = row_num,
    values_from = c(Industry, Value),
    names_sep = ""
  ) %>%
  ungroup()

#Add/remove "values" here as neccessary, depending on how many industries are above defined threshold
result_wide$Value1[is.na(result_wide$Value1)] <- 0
result_wide$Value2[is.na(result_wide$Value2)] <- 0
result_wide$Value3[is.na(result_wide$Value3)] <- 0
result_wide$Value4[is.na(result_wide$Value4)] <- 0
result_wide$Value5[is.na(result_wide$Value5)] <- 0

#Add/remove "values" here also
Reliance1 <- result_wide %>%
  mutate(across(c(Value1, Value2, Value3, Value4, Value5), ~ as.numeric(.))) %>%
  mutate(Total = Value1 + Value2 + Value3 + Value4 + Value5) %>%
  mutate(Industries = paste(Industry1, Industry2, Industry3, Industry4, Industry5, sep = "; ")) %>%
  mutate(Industries = gsub("; NA", "", Industries)) %>%
  select(ladcode, Total, Industries) %>%
  rename(Reliance = Total)

#Normalize etc
Reliance1$Reliance[is.na(Reliance1$Reliance)] <- 0

Reliance1$Reliance <- normalize(Reliance1$Reliance)

Reliance1$Reliance <- Reliance1$Reliance + 1

Reliance1$Industries <- as.character(Reliance1$Industries)

write.csv(Reliance1, "Data\\LAD_Reliance_1_Digit.csv")
```

```{r RR score adding in relatedness}
LAD_Reliance <- read.csv("Data\\LAD_Reliance_1_Digit.csv")

# Function to extract relatedness scores from proximity matrix
extract_relatedness_scores <- function(industries, proximity_matrix) {
  n <- length(industries)
  relatedness_scores <- matrix(NA, nrow = n, ncol = n, dimnames = list(industries, industries))
  for (i in 1:n) {
    for (j in 1:n) {
      relatedness_scores[i, j] <- proximity_matrix[industries[i], industries[j]]
    }
  }
  return(relatedness_scores)
}

# Split the IndsofInt column and extract unique industries
LAD_Reliance$Industries <- strsplit(LAD_Reliance$Industries, "; ")

# Extract unique industries from all rows
unique_industries <- unique(unlist(LAD_Reliance$Industries))

# Define a function to remove "NA" from a list
remove_na <- function(industry_list) {
  industry_list[industry_list != "NA"]
}

# Apply the function to each list in the Industries column
LAD_Reliance$Industries <- lapply(LAD_Reliance$Industries, remove_na)

# Extract relatedness scores for each row
relatedness_scores_list <- lapply(LAD_Reliance$Industries, function(industries) {
  extract_relatedness_scores(industries, proximity_matrix)
})

# Calculate mean and sum of relatedness scores for each row
LAD_Reliance$Mean_Relatedness <- sapply(relatedness_scores_list, function(scores) {
  mean(scores[!is.na(scores)])
})

LAD_Reliance$Mean_Relatedness <- normalize(LAD_Reliance$Mean_Relatedness)

LAD_Reliance$Mean_Relatedness <- LAD_Reliance$Mean_Relatedness + 1

#Calculate Reliance/Relatedness Scores using mean
LAD_Reliance$RR <- LAD_Reliance$Mean_Relatedness*LAD_Reliance$Reliance

LAD_Reliance$RR <- normalize(LAD_Reliance$RR)

LAD_Reliance$RR <- LAD_Reliance$RR + 1

LAD_Reliance <- LAD_Reliance %>% select(ladcode, RR, Reliance, Mean_Relatedness)

#Get the totals for each LAD
LAD_Reliance <- LAD_Reliance %>%
  group_by(ladcode) %>%
  summarize(RR = sum(RR, na.rm = TRUE), Reliance = sum(Reliance, na.rm = TRUE), Mean_Relatedness)

LAD_Reliance$RR <- normalize(LAD_Reliance$RR)
LAD_Reliance$Reliance <- normalize(LAD_Reliance$Reliance)

LAD_Reliance$RR <- LAD_Reliance$RR + 1 
LAD_Reliance$Reliance <- LAD_Reliance$Reliance + 1

write.csv(LAD_Reliance, "Data\\LAD_RR_1_Digit.csv")
```

```{r RRC score adding in complexity}
LAD_RRC<- LAD_ECI %>%
  mutate(ECI = Re(ECI))

LAD_RRC$ECI <- (LAD_RRC$ECI + 1)/2

LAD_RRC$ECI <- normalize(LAD_RRC$ECI)

#We don't add one here because we want more complexity to lower the NZI score as a measure of resilience of the area
#LAD_RRC$ECI <- LAD_RRC$ECI + 1 

LAD <- LAD %>%
  select(Area, ladcode)

LAD_RRC <- LAD_RRC %>%
  left_join(LAD, by = "Area") %>%
  left_join(LAD_Reliance, by = "ladcode")

LAD_RRC$RRC <- LAD_RRC$RR*LAD_RRC$ECI

LAD_RRC$RRC <- normalize(LAD_RRC$RRC)

LAD_RRC$RRC <- LAD_RRC$RRC + 1

write.csv(LAD_RRC, "Data\\LAD_RRC_1_Digit.csv")
```

5 Digit
Economic Complexity 
```{r Construct Location Quotient Matrix}
#Read BRES percentages data
LAD <-  read.csv("Data\\Business Register and Employment Survey\\2022 LAD 5 Digit Percentages.csv", skip = num_rows_to_skip, nrows = num_rows_to_read, blank.lines.skip = TRUE, fileEncoding = "Latin1", check.names = F) %>%
  select(where(~!all(is.na(.)))) %>%
  select(where(~!all(. == ""))) %>%
  rename(Area = 1, ladcode = 2)%>%
  rename_with(~ paste0("X", .), -(1:2))
original_colnames <- colnames(LAD)  # Store original column names

#Need to do some wrangling to only keep industries of interest

# Function to modify column names
modify_colname <- function(colname) {
  if (nchar(colname) > 5) {
    return(substr(colname, 2, 6))
  } else {
    return(substr(colname, 2, nchar(colname)))
  }
}

# Apply modification to column names, excluding the first two columns
modified_colnames <- c(
  original_colnames[1:2], 
  sapply(original_colnames[3:length(original_colnames)], modify_colname)
)

# Set the modified column names to the dataframe
colnames(LAD) <- modified_colnames

#Store modified column names for later use
stored_modified_colnames <- modified_colnames

IndsofInt <- read.csv("Industries_of_Interest.csv")

# Function to ensure SIC_Code is 5 digits
pad_sic_code <- function(code) {
  code <- as.character(code)
  if (nchar(code) == 4) {
    return(paste0(code, "0"))
  } else {
    return(code)
  }
}

# Apply the function to the SIC_Code column
IndsofInt$SIC_Code <- sapply(IndsofInt$SIC_Code, pad_sic_code)

# Convert back to a factor or integer if needed
IndsofInt$SIC_Code <- as.integer(IndsofInt$SIC_Code)

#Create Vector of Sic_Codes
sic_codes <- IndsofInt$SIC_Code

# Get the column names of the LAD dataframe
lad_columns <- names(LAD)

# Identify the columns that match sic_codes or are the first two columns
columns_to_keep <- c(lad_columns[1:2], lad_columns[lad_columns %in% sic_codes])

# Subset the dataframe to keep only those columns
LAD <- LAD[, columns_to_keep]

# Create a named vector for mapping SIC_Code to Description
sic_to_desc <- setNames(IndsofInt$Description, IndsofInt$SIC_Code)

# Rename columns of LAD dataframe using the mapping
colnames(LAD) <- sic_to_desc[colnames(LAD)]

LAD <- LAD %>% rename(Area = 1, ladcode = 2) %>%
  rename_with(~ paste0("X", .), -(1:2))
# Convert the dataset to long format
LAD_long <- pivot_longer(LAD, cols = starts_with("X"), names_to = "Industry", values_to = "Employment")

# Calculate total employment for each local authority
total_emp_local <- LAD_long %>%
  group_by(Area) %>%
  summarize(TotalEmpLocal = sum(Employment))

# Calculate total employment for each industry at the national level
total_emp_industry <- LAD_long %>%
  group_by(Industry) %>%
  summarize(TotalEmpIndustry = sum(Employment))

# Calculate the total employment at the national level
total_emp_national <- sum(LAD_long$Employment)

# Merge total employment for local authorities
LAD_long <- LAD_long %>%
  left_join(total_emp_local, by = "Area")

# Merge total employment for industries
LAD_long <- LAD_long %>%
  left_join(total_emp_industry, by = "Industry")

# Add national total employment to the data frame
LAD_long <- LAD_long %>%
  mutate(TotalEmpNational = sum(Employment))

# Calculate location quotient
LAD_long <- LAD_long %>%
  mutate(LQ = (Employment / TotalEmpLocal) / (TotalEmpIndustry / TotalEmpNational))

# Pivot the data back to matrix with LQ values
LQ_matrix <- LAD_long %>%
  select(Area, Industry, LQ) %>%
  pivot_wider(names_from = Industry, values_from = LQ) %>%
  replace(is.na(.), 0)  # Replace NA values with 0
```

```{r Create Local Authority Similarity Matrix}
# Convert the LQ values to a binary M matrix
binary_M <- as.data.frame(LQ_matrix %>% select(-Area) > 1)
row.names(binary_M) <- LQ_matrix$Area

# Calculate local authority diversity vector D
D <- rowSums(binary_M)
D[D == 0] <- 1 # Handle zero diversity

# Calculate industry ubiquity vector U
U <- colSums(binary_M)
U[U == 0] <- 1 # Handle zero ubiquity

# Create diagonal matrices
D_diag <- diag(1 / D)
U_diag <- diag(1 / U)

# Compute the local authority similarity matrix M
M_matrix <- as.matrix(binary_M)
LAD_similarity_matrix <- D_diag %*% M_matrix %*% U_diag %*% t(M_matrix)
```

```{r Calculate Economic and Product Complexity Index}
# Calculate the eigenvalues and eigenvectors for the ECI
eci_eigen <- eigen(LAD_similarity_matrix)
eci_values <- eci_eigen$values
eci_vectors <- eci_eigen$vectors

# Find the second largest eigenvalue and its corresponding eigenvector
eci_second_eigenvector <- eci_vectors[, order(eci_values, decreasing = TRUE)[2]]

# Display the ECI (second largest eigenvector)
print("ECI (Second largest eigenvector):")
print(eci_second_eigenvector)

# Calculate the industry similarity matrix for PCI
industry_similarity_matrix <- U_diag %*% t(M_matrix) %*% D_diag %*% M_matrix

# Display the industry similarity matrix
print(industry_similarity_matrix)

# Calculate the eigenvalues and eigenvectors for the PCI
pci_eigen <- eigen(industry_similarity_matrix)
pci_values <- pci_eigen$values
pci_vectors <- pci_eigen$vectors

# Find the second largest eigenvalue and its corresponding eigenvector
pci_second_eigenvector <- pci_vectors[, order(pci_values, decreasing = TRUE)[2]]

# Create the ECI data frame
LAD_ECI <- data.frame(
  Area = row.names(binary_M),
  ECI = eci_second_eigenvector
)

# Create the PCI data frame
LAD_PCI <- data.frame(
  Industry = colnames(binary_M),
  PCI = pci_second_eigenvector
)

write.csv(LAD_ECI, "Data\\LAD_ECI_5_Digit.csv")
```

Relatedness
```{r Calculate proximity matrix}
# Function to calculate proximity matrix
calculate_proximity <- function(binary_M) {
  num_industries <- ncol(binary_M)
  proximity_matrix <- matrix(0, nrow = num_industries, ncol = num_industries)
  colnames(proximity_matrix) <- colnames(binary_M)
  rownames(proximity_matrix) <- colnames(binary_M)
  
  for (i in 1:num_industries) {
    for (j in 1:num_industries) {
      if (i != j) {
        m_i_j <- sum(binary_M[, i] * binary_M[, j])
        m_i <- sum(binary_M[, i])
        m_j <- sum(binary_M[, j])
        proximity_matrix[i, j] <- min(m_i_j / m_i, m_i_j / m_j)
      }
    }
  }
  return(proximity_matrix)
}

# Calculate the proximity matrix
proximity_matrix <- calculate_proximity(binary_M)
```

Reliance
```{r Over reliance on one employer}
#Read BRES percentages data
LAD <-  read.csv("Data\\Business Register and Employment Survey\\2022 LAD 5 Digit Percentages.csv", skip = num_rows_to_skip, nrows = num_rows_to_read, blank.lines.skip = TRUE, fileEncoding = "Latin1", check.names = F) %>%
  select(where(~!all(is.na(.)))) %>%
  select(where(~!all(. == ""))) %>%
  rename(Area = 1, ladcode = 2)%>%
  rename_with(~ paste0("X", .), -(1:2))

#Need to do some wrangling to only keep 5 digit industries of interest

original_colnames <- colnames(LAD)  # Store original column names

# Function to modify column names
modify_colname <- function(colname) {
  if (nchar(colname) > 5) {
    return(substr(colname, 2, 6))
  } else {
    return(substr(colname, 2, nchar(colname)))
  }
}

# Apply modification to column names, excluding the first two columns
modified_colnames <- c(
  original_colnames[1:2], 
  sapply(original_colnames[3:length(original_colnames)], modify_colname)
)

# Set the modified column names to the dataframe
colnames(LAD) <- modified_colnames

#Store modified column names for later use
stored_modified_colnames <- modified_colnames

IndsofInt <- read.csv("Industries_of_Interest.csv")

# Function to ensure SIC_Code is 5 digits
pad_sic_code <- function(code) {
  code <- as.character(code)
  if (nchar(code) == 4) {
    return(paste0(code, "0"))
  } else {
    return(code)
  }
}

# Apply the function to the SIC_Code column
IndsofInt$SIC_Code <- sapply(IndsofInt$SIC_Code, pad_sic_code)

# Convert back to a factor or integer if needed
IndsofInt$SIC_Code <- as.integer(IndsofInt$SIC_Code)

#Create Vector of Sic_Codes
sic_codes <- IndsofInt$SIC_Code

# Get the column names of the LAD dataframe
lad_columns <- names(LAD)

# Identify the columns that match sic_codes or are the first two columns
columns_to_keep <- c(lad_columns[1:2], lad_columns[lad_columns %in% sic_codes])

# Subset the dataframe to keep only those columns
LAD <- LAD[, columns_to_keep]

# Create a named vector for mapping SIC_Code to Description
sic_to_desc <- setNames(IndsofInt$Description, IndsofInt$SIC_Code)

# Rename columns of LAD dataframe using the mapping
colnames(LAD) <- sic_to_desc[colnames(LAD)]

LAD <- LAD %>% rename(Area = 1, ladcode = 2) %>%
  rename_with(~ paste0("X", .), -(1:2))

#Read BRES counts data
LADct <-  read.csv("Data\\Business Register and Employment Survey\\2022 LAD 5 Digit Counts.csv", skip = num_rows_to_skip, nrows = num_rows_to_read, blank.lines.skip = TRUE, fileEncoding = "Latin1", check.names = F) %>%
  select(where(~!all(is.na(.)))) %>%
  select(where(~!all(. == ""))) %>%
  rename(Area = 1, ladcode = 2) %>%
  rename_with(~ paste0("X", .), -(1:2))

#As before, do some wrangling to keep industries of interest only
original_colnames <- colnames(LADct)  # Store original column names

# Function to modify column names
modify_colname <- function(colname) {
  if (nchar(colname) > 5) {
    return(substr(colname, 2, 6))
  } else {
    return(substr(colname, 2, nchar(colname)))
  }
}

# Apply modification to column names, excluding the first two columns
modified_colnames <- c(
  original_colnames[1:2], 
  sapply(original_colnames[3:length(original_colnames)], modify_colname)
)

# Set the modified column names to the dataframe
colnames(LADct) <- modified_colnames

#Store modified column names for later use
stored_modified_colnames <- modified_colnames

IndsofInt <- read.csv("Industries_of_Interest.csv")

# Function to ensure SIC_Code is 5 digits
pad_sic_code <- function(code) {
  code <- as.character(code)
  if (nchar(code) == 4) {
    return(paste0(code, "0"))
  } else {
    return(code)
  }
}

# Apply the function to the SIC_Code column
IndsofInt$SIC_Code <- sapply(IndsofInt$SIC_Code, pad_sic_code)

# Convert back to a factor or integer if needed
IndsofInt$SIC_Code <- as.integer(IndsofInt$SIC_Code)

#Create Vector of Sic_Codes
sic_codes <- IndsofInt$SIC_Code

# Get the column names of the LAD dataframe
lad_columns <- names(LADct)

# Identify the columns that match sic_codes or are the first two columns
columns_to_keep <- c(lad_columns[1:2], lad_columns[lad_columns %in% sic_codes])

# Subset the dataframe to keep only those columns
LADct <- LADct[, columns_to_keep]

# Create a named vector for mapping SIC_Code to Description
sic_to_desc <- setNames(IndsofInt$Description, IndsofInt$SIC_Code)

# Rename columns of LAD dataframe using the mapping
colnames(LADct) <- sic_to_desc[colnames(LADct)]

LADct <- LADct %>% rename(Area = 1, ladcode = 2) %>%
  rename_with(~ paste0("X", .), -(1:2))

# Convert the dataset to long format
LADct_long <- pivot_longer(LADct, cols = starts_with("X"), names_to = "Industry", values_to = "Employmentct")

#Check for threshold visually:
percentages <- LAD[, 3:ncol(LAD)]
percentages <- as.data.frame(sapply(percentages, function(x) as.numeric(as.character(x))))
percentages_vector <- unlist(percentages)
percentages_vector <- na.omit(percentages_vector)

hist(percentages_vector, breaks = 50, col = "skyblue", xlab = "Percentage of Employees", main = "Histogram of Percentages")

#SET THRESHOLD HERE
# Identify cells with value 1, excluding the first two columns
indices <- which(LAD[, -c(1, 2)] >= 1, arr.ind = TRUE)

# Adjust column indices to match the original dataframe
indices[, 2] <- indices[, 2] + 2

# Create a list with the corresponding area details and industries
result <- data.frame(
  AreaName = LAD[indices[, 1], 1],
  AreaCode = LAD[indices[, 1], 2],
  Industry = colnames(LAD)[indices[, 2]],
  Value = LAD[indices]
)

# Add a row number within each group of AreaName and AreaCode, join to counts
result <- result %>%
  group_by(AreaName, AreaCode) %>%
  mutate(row_num = row_number()) %>%
  rename(ladcode = AreaCode) %>%
  left_join(LADct_long, by = c("ladcode", "Industry")) 

#Normalize and weight employment percentages and counts
result$Value = as.numeric(result$Value)
result$Value = normalize(result$Value)
result$Value = result$Value + 1
result$Employmentct = normalize(result$Employmentct)
result$Employmentct = result$Employmentct + 1
result$Value = result$Value*result$Employmentct

# Reshape the dataframe to wide format
result_wide <- result %>%
  pivot_wider(
    names_from = row_num,
    values_from = c(Industry, Value),
    names_sep = ""
  ) %>%
  ungroup()

#Add/remove "values" here as neccessary, depending on how many industries are above defined threshold
result_wide$Value1[is.na(result_wide$Value1)] <- 0
result_wide$Value2[is.na(result_wide$Value2)] <- 0
result_wide$Value3[is.na(result_wide$Value3)] <- 0
result_wide$Value4[is.na(result_wide$Value4)] <- 0
result_wide$Value5[is.na(result_wide$Value5)] <- 0

#Add/remove "values" here also
Reliance1 <- result_wide %>%
  mutate(across(c(Value1, Value2, Value3, Value4, Value5), ~ as.numeric(.))) %>%
  mutate(Total = Value1 + Value2 + Value3 + Value4 + Value5) %>%
  mutate(Industries = paste(Industry1, Industry2, Industry3, Industry4, Industry5, sep = "; ")) %>%
  mutate(Industries = gsub("; NA", "", Industries)) %>%
  select(ladcode, Total, Industries) %>%
  rename(Reliance = Total)

#Normalize etc
Reliance1$Reliance[is.na(Reliance1$Reliance)] <- 0

Reliance1$Reliance <- normalize(Reliance1$Reliance)

Reliance1$Reliance <- Reliance1$Reliance + 1

Reliance1$Industries <- as.character(Reliance1$Industries)

write.csv(Reliance1, "Data\\LAD_Reliance_5_Digit.csv")
```

```{r RR score adding in relatedness}
LAD_Reliance <- read.csv("Data\\LAD_Reliance_5_Digit.csv")

# Function to extract relatedness scores from proximity matrix
extract_relatedness_scores <- function(industries, proximity_matrix) {
  n <- length(industries)
  relatedness_scores <- matrix(NA, nrow = n, ncol = n, dimnames = list(industries, industries))
  for (i in 1:n) {
    for (j in 1:n) {
      relatedness_scores[i, j] <- proximity_matrix[industries[i], industries[j]]
    }
  }
  return(relatedness_scores)
}

# Split the IndsofInt column and extract unique industries
LAD_Reliance$Industries <- strsplit(LAD_Reliance$Industries, "; ")

# Extract unique industries from all rows
unique_industries <- unique(unlist(LAD_Reliance$Industries))

# Define a function to remove "NA" from a list
remove_na <- function(industry_list) {
  industry_list[industry_list != "NA"]
}

# Apply the function to each list in the Industries column
LAD_Reliance$Industries <- lapply(LAD_Reliance$Industries, remove_na)

# Extract relatedness scores for each row
relatedness_scores_list <- lapply(LAD_Reliance$Industries, function(industries) {
  extract_relatedness_scores(industries, proximity_matrix)
})

# Calculate mean and sum of relatedness scores for each row
LAD_Reliance$Mean_Relatedness <- sapply(relatedness_scores_list, function(scores) {
  mean(scores[!is.na(scores)])
})

LAD_Reliance$Mean_Relatedness <- normalize(LAD_Reliance$Mean_Relatedness)

LAD_Reliance$Mean_Relatedness <- LAD_Reliance$Mean_Relatedness + 1

#Calculate Reliance/Relatedness Scores using mean
LAD_Reliance$RR <- LAD_Reliance$Mean_Relatedness*LAD_Reliance$Reliance

LAD_Reliance$RR <- normalize(LAD_Reliance$RR)

LAD_Reliance$RR <- LAD_Reliance$RR + 1

#Get the totals for each LAD
LAD_Reliance <- LAD_Reliance %>%
  group_by(ladcode) %>%
  summarize(RR = sum(RR, na.rm = TRUE), Reliance = sum(Reliance, na.rm = TRUE), Mean_Relatedness)

LAD_Reliance <- LAD_Reliance %>% select(ladcode, RR, Reliance, Mean_Relatedness)

LAD_Reliance$RR <- normalize(LAD_Reliance$RR)
LAD_Reliance$Reliance <- normalize(LAD_Reliance$Reliance)

LAD_Reliance$RR <- LAD_Reliance$RR + 1 
LAD_Reliance$Reliance <- LAD_Reliance$Reliance + 1

write.csv(LAD_Reliance, "Data\\LAD_RR_5_Digit.csv")
```

```{r RRC score adding in complexity}
LAD_RRC<- LAD_ECI %>%
  mutate(ECI = Re(ECI))

LAD_RRC$ECI <- (LAD_RRC$ECI + 1)/2

LAD_RRC$ECI <- normalize(LAD_RRC$ECI)

#We don't add one here because we want more complexity to lower the NZI score as a measure of resilience of the area
#LAD_RRC$ECI <- LAD_RRC$ECI + 1 

LAD <- LAD %>%
  select(Area, ladcode)

LAD_RRC <- LAD_RRC %>%
  left_join(LAD, by = "Area") %>%
  left_join(LAD_Reliance, by = "ladcode")

LAD_RRC$RR[is.na(LAD_RRC$RR)] <- 1
LAD_RRC$Reliance[is.na(LAD_RRC$Reliance)] <- 1

LAD_RRC$RRC <- LAD_RRC$RR*LAD_RRC$ECI

LAD_RRC$RRC <- normalize(LAD_RRC$RRC)

LAD_RRC$RRC <- LAD_RRC$RRC + 1

write.csv(LAD_RRC, "Data\\LAD_RRC_5_Digit.csv")
```

Dataset Construction
```{r Dataset Construction}
#Read RRC scores for each digit level
RRC1 <- read.csv("Data\\LAD_RRC_1_Digit.csv") %>%
  rename(ECI1 = ECI, RR1 = RR, Reliance1 = Reliance, RRC1 = RRC, MR1 = Mean_Relatedness, LAD22CD = ladcode, LAD22NM = Area)
RRC5 <- read.csv("Data\\LAD_RRC_5_Digit.csv") %>%
  rename(ECI5 = ECI, RR5 = RR, Reliance5 = Reliance, RRC5 = RRC, MR5 = Mean_Relatedness,
         LAD22CD = ladcode)
#Population counts for LADs
LADPOP <-  read.csv("Data\\2022 Census\\LAD_Population.csv") %>%
  rename(LAD22CD = 1, Population = 2) %>%
  mutate(LAD22CD = trimws(LAD22CD, which = "left"))

#Join the datasets and calculate final vulnerability score
NZI <- RRC5 %>%
  left_join(RRC1, by = "LAD22CD") %>%
  left_join(LADPOP, by = "LAD22CD") %>%
  mutate(NZI_score = RRC1*RRC5)

#Normalize
NZI$NZI_Score <- normalize(NZI$NZI_score)

#Create ranks, deciles, and final NZI score which considers population of area
VNZI <- NZI %>%
  mutate(RRC_decile = ntile(NZI_score, 10),
         RRC_rank = rank(NZI_score),
         Population_decile = ntile(Population, 10),
         VNZI_pop = RRC_decile*Population_decile,
         VNZI = sqrt(VNZI_pop),
         VNZI_rank = rank(VNZI),
         RRC_score = NZI_score) %>%
  select(LAD22CD, LAD22NM,VNZI, VNZI_rank, VNZI_pop, RRC_score, RRC_rank, RRC_decile, Population, Population_decile, RRC5, RR5, Reliance5, MR1, ECI5, RRC1, RR1, Reliance1, MR5, ECI1) %>%
  group_by(LAD22NM) %>%
  slice_max(order_by = MR1, with_ties = FALSE) %>%
  ungroup()

write.csv(VNZI, "VNZI_LAD.csv")
```
