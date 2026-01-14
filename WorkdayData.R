library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(forcats)

# Choose a file using a dialog box
file_path <- file.choose() ## P&D Fee History Detail

# Now you can use file_path with your data loading function
# For example, if it's a CSV file: 
if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
  feesCollected <- read.csv(file_path)
  print("CSV file loaded successfully!")
  head(feesCollected)
} else if (grepl("\\.txt$", file_path, ignore.case = TRUE)) {
  feesCollected <- read.table(file_path, header = TRUE) # Example for text file with header
  print("Text file loaded successfully!")
  head(feesCollected)
} else if (grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
  # You'll need the 'readxl' package for Excel files
  # install.packages("readxl") # Uncomment and run if you don't have it
  library(readxl)
  feesCollected <- read_excel(file_path)
  print("Excel file loaded successfully!")
  head(feesCollected)
} else {
  print("Unsupported file type or no file selected.")
}

### Add 2026 CSV file
file_path <- file.choose()
feesCollected1 <- read.csv(file_path)
intersect(names(feesCollected), names(feesCollected1))
feesCollected <- merge(feesCollected, feesCollected1, by = intersect(names(feesCollected), names(feesCollected1)), all.x = TRUE, all.y = TRUE)


########################################################

feesCollected$Primary.Cost.ObjectCategory <- sub(" > .*", "", feesCollected$Primary.Cost.Object)
summary(as.factor(feesCollected$Primary.Cost.ObjectCategory))

feesCollected$Line.MemoCategory <- sub("Transfer Revenue from Deposits Payable to Task for Services Provided.*", "", feesCollected$Line.Memo)
summary(as.factor(feesCollected$Line.MemoCategory))

feesCollected$Line.MemoCategory <- ifelse(
  grepl("ROW", feesCollected$Line.MemoCategory), 
  "ROW Fee", 
  feesCollected$Line.MemoCategory
)
temp <- as.data.frame(summary(as.factor(feesCollected$Line.MemoCategory)))

feesCollected$Line.MemoCategory <- ifelse(
  grepl("OD Transmittal STP/COVP", feesCollected$Line.MemoCategory), 
  "OD Transmittal STP/COVP", 
  feesCollected$Line.MemoCategory
)
temp <- as.data.frame(summary(as.factor(feesCollected$Line.MemoCategory)))

feesCollected$Line.MemoCategory <- ifelse(
  grepl("Transfer Revenue from Deposits Payable to Task Services Provided", feesCollected$Line.MemoCategory), 
  "OD Transmittal STP/COVP", 
  feesCollected$Line.MemoCategory
)
temp <- as.data.frame(summary(as.factor(feesCollected$Line.MemoCategory)))

feesCollected[[5]] <- as.factor(feesCollected[[5]])
feesCollected[[6]] <- as.factor(feesCollected[[6]])



summary(as.factor(feesCollected$Cost.Center.ID))
attributes(feesCollected)

feesCollected$ROW <- NA
feesCollected$ROW[grep("Customer Transaction: DCS RIGHT OF WAY", feesCollected$Operational.Transaction)] <- "Yes"
feesCollected <- feesCollected %>%
  mutate(
    Department = as.factor(Department),
    Division = as.factor(Division),
    Primary.Cost.Object.ID = as.factor(Primary.Cost.Object.ID),
    Primary.Cost.Object = as.factor(Primary.Cost.Object),
    Ledger.Account = as.factor(Ledger.Account),
    Accounting.Date1 = as.Date(feesCollected$Accounting.Date, , "%m/%d/%Y"),
    Amount = as.numeric(Amount),
    Header.Memo = as.character(Header.Memo),
    Line.Memo = as.character(Line.Memo),
    FY = ifelse(month(Accounting.Date1) >= 7,
                        paste0("FY ", year(Accounting.Date1) + 1),
                        paste0("FY ", year(Accounting.Date1)))
  )
temp <- as.data.frame(feesCollected$Accounting.Date1, feesCollected$FY)

temp <- feesCollected |>
  group_by(Line.Memo) |>
  summarise(n = n())

temp <- subset(feesCollected, grepl("Transfer unearned revenue to deposits payable from charges for services", feesCollected$Line.Memo))
temp <- subset(feesCollected, grepl("Transfer Revenue from Deposits", feesCollected$Line.Memo))

# Display the structure of the data frame and the type of each column
str(feesCollected)


for (col_name in names(feesCollected)) {
  
  # Get the specific column from the dataframe
  column_data <- feesCollected[[col_name]]
  
  # Count the number of unique levels in the column when treated as a factor
  # Using as.factor() ensures this works on character and numeric columns too
  num_levels <- nlevels(as.factor(column_data))
  
  # Check if the number of levels is between 1 and 9 (inclusive)
  if (num_levels > 0 && num_levels < 10) {
    
    # Print a header for the column
    cat("----------------------------------------\n")
    cat("Frequency Distribution for:", col_name, "\n")
    cat("----------------------------------------\n")
    
    # Print the frequency table for that column
    print(table(column_data, useNA = "ifany")) # useNA shows count of missing values if any
    
    # Add a blank line for better readability
    cat("\n")
  }
}

yearly_fees <- feesCollected %>%
  group_by(FY) %>%
  summarise(
    TotalAmount = sum(Amount, na.rm = TRUE)
  ) %>%
  ungroup()

yearly_project_summary <- feesCollected %>%
  group_by(FY, Primary.Cost.Object, ROW) %>%
  summarise(
    TotalAmount = sum(Amount, na.rm = TRUE)
  ) %>%
  ungroup()

yearly_project_summary <- subset(yearly_project_summary, yearly_project_summary$Primary.Cost.Object %in% c("905400 DCS Transportation Planning", "905405 DCS Transportation Development"))

feesCollected$Primary.Cost.Object1 <- "With project number"
feesCollected$Primary.Cost.Object1[feesCollected$Primary.Cost.Object %in% c("905400 DCS Transportation Planning", "905405 DCS Transportation Development")] <- "Without project number"
feesCollected$Primary.Cost.Object1 <- as.factor(feesCollected$Primary.Cost.Object1)

yearly_project_summary <- feesCollected %>%
  group_by(FY, Primary.Cost.Object1) %>%
  summarise(
    TotalAmount = sum(Amount, na.rm = TRUE)
  ) %>%
  ungroup()



ggplot(yearly_project_summary, aes(x = FY, y = -1*TotalAmount, fill = Primary.Cost.Object1)) +
  geom_col() +
  labs(
    title = "Total Amount by Fiscal Year and Cost Object",
    x = "Fiscal Year",
    y = "Total Amount ($)",
    fill = "Primary Cost Object"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

yearly_project_summary <- feesCollected %>%
  group_by(FY, Primary.Cost.ObjectCategory) %>%
  summarise(
    TotalAmount = sum(Amount, na.rm = TRUE)
  ) %>%
  ungroup()


ggplot(yearly_project_summary, aes(x = FY, y = -1*TotalAmount, fill = Primary.Cost.ObjectCategory)) +
  geom_col() +
  labs(
    title = "Total Amount by Fiscal Year and Cost Object",
    x = "Fiscal Year",
    y = "Total Amount ($)",
    fill = "Primary Cost Object"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(yearly_project_summary, aes(x = FY, y = -1*TotalAmount)) +
  geom_col() +
  labs(
    title = "Total Amount by Fiscal Year and Cost Object",
    x = "Fiscal Year",
    y = "Total Amount ($)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### Ledger.Account
#feesCollected$Ledger.Account[grepl("PROJECT_PLAN_TASK-3-167406", feesCollected$Primary.Cost.Object.ID)] <- "PWB"

yearly_Ledger.Account <- feesCollected %>%
  group_by(FY, Ledger.Account) %>%
  summarise(
    TotalAmount = sum(Amount, na.rm = TRUE)
  ) %>%
  ungroup()



yearly_Ledger.Account$TotalAmount[grep("FY 2026", yearly_Ledger.Account$FY, value = FALSE)] <- 
  yearly_Ledger.Account$TotalAmount[grep("FY 2026", yearly_Ledger.Account$FY, value = FALSE)]*4
yearly_Ledger.Account$FY[grep("FY 2026", yearly_Ledger.Account$FY, value = FALSE)] <- "FY 2026\n(estimate)"


ggplot(yearly_Ledger.Account, aes(x = FY, y = -1*TotalAmount, fill = Ledger.Account)) +
  geom_col() +
  labs(
    title = "Total Amount by Fiscal Year and Ledger Account",
    x = "Fiscal Year",
    y = "Total Amount ($)",
    fill = "Ledger Account"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



temp <- feesCollected |> 
  group_by(Primary.Cost.Object.ID) |> 
  filter(Primary.Cost.Object.ID == "PROJECT_PLAN_TASK-3-167406")
  #summarise(TotalAmount = sum(Amount, na.rm = TRUE))
########## Final Graph ############################


ggplot(yearly_Ledger.Account, aes(x = FY, y = -1*TotalAmount, fill = Ledger.Account)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = dollar(-1*TotalAmount)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Transportation Permit Fees Collection by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount ($)",
    fill = "Ledger Account"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_discrete(labels = c("Flat-fee permits", "Full cost recovery\ncharges for services", "Charges for services - PWB"))


########################################################################################################
feesCollected$Ledger.Account1 <- as.character(feesCollected$Ledger.Account)
feesCollected$Ledger.Account1[grepl("PROJECT_PLAN_TASK-3-167406", feesCollected$Primary.Cost.Object.ID)] <- "PWB"
summary(as.factor(feesCollected$Ledger.Account1))

yearly_Ledger.Account1 <- feesCollected %>%
  group_by(FY, Ledger.Account, Ledger.Account1) %>%
  summarise(
    TotalAmount = sum(Amount, na.rm = TRUE)
  ) %>%
  ungroup()



yearly_Ledger.Account1$TotalAmount[grep("FY 2026", yearly_Ledger.Account1$FY, value = FALSE)] <- 
  yearly_Ledger.Account1$TotalAmount[grep("FY 2026", yearly_Ledger.Account1$FY, value = FALSE)]*4
yearly_Ledger.Account1$FY[grep("FY 2026", yearly_Ledger.Account1$FY, value = FALSE)] <- "FY 2026\n(estimate)"


ggplot(yearly_Ledger.Account, aes(x = FY, y = -1*TotalAmount, fill = Ledger.Account)) +
  geom_col() +
  labs(
    title = "Total Amount by Fiscal Year and Ledger Account",
    x = "Fiscal Year",
    y = "Total Amount ($)",
    fill = "Ledger Account"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



temp <- feesCollected |> 
  group_by(Primary.Cost.Object.ID) |> 
  filter(Primary.Cost.Object.ID == "PROJECT_PLAN_TASK-3-167406")
#summarise(TotalAmount = sum(Amount, na.rm = TRUE))

########## Final Graph ############################

ggplot(yearly_Ledger.Account1, aes(x = FY, y = -1*TotalAmount, fill = Ledger.Account1)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = dollar(-1*TotalAmount)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Transportation Permit Fees Collection by Fiscal Year",
    subtitle = "(after excluding fees collected from Portland Water Bureau project)",
    x = "Fiscal Year",
    y = "Total Amount ($)",
    fill = "Ledger Account"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_discrete(labels = c("Flat-fee permits", "Full cost recovery\ncharges for services", "Charges for services - PWB"))


############### Test #####################
yearly_Ledger.Account1$Ledger.Account1[yearly_Ledger.Account1$Ledger.Account1 == "PWB"] <- "50231: PWB"
cat1 <- "50230:Permits"
cat3 <- "50235:Charges for Services"
cat2 <- "50231: PWB"

ggplot(yearly_Ledger.Account1, aes(x = FY, y = -1 * TotalAmount, fill = Ledger.Account1)) +
  # Layer 1: The "Standalone" bar (Dodge position)
  geom_col(
    data = subset(yearly_Ledger.Account1, Ledger.Account1 == cat1),
    position = position_nudge(x = -0.225), # Shift left
    width = 0.45
  ) +
  # Layer 2: The "Stacked" bars (Stack position)
  geom_col(
    data = subset(yearly_Ledger.Account1, Ledger.Account1 %in% c(cat2, cat3)),
    position = position_stack(vjust = 1),
    mapping = aes(x = as.numeric(as.factor(FY)) + 0.225), # Shift right
    width = 0.45
  ) +
  labs(
    title = "Transportation Permit Fees Collection by Fiscal Year",
    subtitle = "(Revenue from the Portland Water Bureau project shown in blue)",
    x = "Fiscal Year",
    y = "Total Amount",
    fill = "Ledger Account"
  ) +
  scale_y_continuous(labels = label_dollar()) +
  #scale_fill_discrete(labels = c("Flat Fee Revenue", "Full Cost Recovery Fee\nRevenue (non-PWB)", "Full Cost Recovery Fee\nRevenue from PWB")) +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "50230:Permits" = "#1b9e77",        # Dark Green
      "50235:Charges for Services" = "#d95f02", # Orange
      "50231: PWB" = "skyblue"                   # Purple
    ),
    labels = c(
      "50230:Permits" = "Flat Fee Revenue", 
      "50231: PWB" = "Full Cost Recovery Fee\nRevenue from PWB",
      "50235:Charges for Services" = "Full Cost Recovery Fee\nRevenue (non-PWB)"
    )
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#### Journal.Source

yearly_journal.source <- feesCollected %>%
  group_by(FY, Journal.Source) %>%
  summarise(
    TotalAmount = sum(Amount, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(yearly_journal.source, aes(x = FY, y = -1*TotalAmount, fill = Journal.Source)) +
  geom_col() +
  labs(
    title = "Total Amount by Fiscal Year and Journal Source",
    x = "Fiscal Year",
    y = "Total Amount ($)",
    fill = "Journal Source"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Cost.Center #### Not relevant because the name of the cost center was changed from Transportation Planning to Transportation Development

yearly_Cost.Center <- feesCollected %>%
  group_by(FY, Cost.Center) %>%
  summarise(
    TotalAmount = sum(Amount, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(yearly_Cost.Center, aes(x = FY, y = -1*TotalAmount, fill = Cost.Center)) +
  geom_col() +
  labs(
    title = "Total Amount by Fiscal Year and Cost Center",
    x = "Fiscal Year",
    y = "Total Amount ($)",
    fill = "Cost Center"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Project

yearly_Project <- feesCollected %>%
  group_by(FY, Project) %>%
  summarise(
    TotalAmount = sum(Amount, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(yearly_Project, aes(x = FY, y = -1*TotalAmount, fill = Project)) +
  geom_col() +
  labs(
    title = "Total Amount by Fiscal Year and Project",
    x = "Fiscal Year",
    y = "Total Amount ($)",
    fill = "Project"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### Project.Plan.Phase

yearly_Project.Plan.Phase <- feesCollected %>%
  group_by(FY, Project.Plan.Phase) %>%
  summarise(
    TotalAmount = sum(Amount, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(yearly_Project.Plan.Phase, aes(x = FY, y = -1*TotalAmount, fill = Project.Plan.Phase)) +
  geom_col() +
  labs(
    title = "Total Amount by Fiscal Year and Project.Plan.Phase",
    x = "Fiscal Year",
    y = "Total Amount ($)",
    fill = "Project.Plan.Phase"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#########################################################################################################
####################### EXAMINE FY 2025 FEES ############################################################
#########################################################################################################

fy25 <- subset(feesCollected, feesCollected$FY == "FY 2025")


#####################################################################################################
#######################   TYLER DATA   ############################################################## 
#####################################################################################################

file_path <- file.choose()
tyler <- read.csv(file_path)

# Check the fields that are categorical variables.
for (col_name in names(tyler)) {
  
  # Get the specific column from the dataframe
  column_data <- tyler[[col_name]]
  
  # Count the number of unique levels in the column when treated as a factor
  # Using as.factor() ensures this works on character and numeric columns too
  num_levels <- nlevels(as.factor(column_data))
  
  # Check if the number of levels is between 1 and 9 (inclusive)
  if (num_levels > 0 && num_levels < 10) {
    
    # Print a header for the column
    cat("----------------------------------------\n")
    cat("Frequency Distribution for:", col_name, "\n")
    cat("----------------------------------------\n")
    
    # Print the frequency table for that column
    print(table(column_data, useNA = "ifany")) # useNA shows count of missing values if any
    
    # Add a blank line for better readability
    cat("\n")
  }
}


# Add fiscal year field
tyler$Payment.Date1 <- as.Date(tyler$Payment.Date, format = "%m/%d/%Y")
tyler$Fiscal.Year <- ifelse(
  as.numeric(format(tyler$Payment.Date1, "%m")) >= 7,
  as.numeric(format(tyler$Payment.Date1, "%Y")) + 1,
  as.numeric(format(tyler$Payment.Date1, "%Y"))
)

# See Payment.Method by FY.
payment_counts <- table(tyler$Fiscal.Year, tyler$Payment.Method)
print(payment_counts)

temp <- tyler[grepl("ROW", tyler$Fee.Description), ]
temp1 <- temp |> 
  group_by(Fee.Name, Fiscal.Year) |> 
  summarise(Total = n())


##################################### TYLER  PERMIT DATA ##############################

file_path <- file.choose()
tylerpermit <- read.csv(file_path)

tylerpermit$Permit.Application.Date1 <- as.Date(tylerpermit$Permit.Application.Date, format = "%m/%d/%Y")
tylerpermit$Fiscal.Year <- ifelse(
  as.numeric(format(tylerpermit$Permit.Application.Date1, "%m")) >= 7,
  as.numeric(format(tylerpermit$Permit.Application.Date1, "%Y")) + 1,
  as.numeric(format(tylerpermit$Permit.Application.Date1, "%Y"))
)

temp1 <- tylerpermit |> 
  group_by(Fiscal.Year) |> 
  summarise(Total = n())

################################### TYLER FEE DATA ##################################

file_path <- file.choose()
tylerFee <- read.csv(file_path)
summary(as.factor(tylerFee$Fee.Name))
tylerFee$Fee.Name <- trimws(tylerFee$Fee.Name)
tylerFee$Fee.Name[tylerFee$Fee.Name == "Lane Closure Fee"] <- "Bridge-§ 27.052 (A)(3)(l) Temporary Closure Street"
#tylerFee <- subset(tylerFee, tylerFee$Fee.Name != "§ 27.052 (A)(3)(a)  Right of Way - Driveway")

# Do I change Fee.Name "Lane Closure Fee" ($500 or $300?) to "Bridge-§ 27.052 (A)(3)(l) Temporary Closure Street" ($550 or $300?)

tylerFee$Amount.Paid1 <- as.numeric(gsub("[\\$,]", "", tylerFee$Amount.Paid))
tylerFee$Payment.Amount1 <- as.numeric(gsub("[\\$,]", "", tylerFee$Payment.Amount))

tylerFee$Payment.Date1 <- as.Date(tylerFee$Payment.Date, format = "%m/%d/%Y")
tylerFee$Fiscal.Year <- ifelse(
  as.numeric(format(tylerFee$Payment.Date1, "%m")) >= 7,
  as.numeric(format(tylerFee$Payment.Date1, "%Y")) + 1,
  as.numeric(format(tylerFee$Payment.Date1, "%Y"))
)
tylerFee$Fee.Name <- gsub("MCRR 26.150   TRANSPORTATION: Stormwater Review", "MCRR 26.150 TRANSPORTATION: Stormwater Review", tylerFee$Fee.Name)
tylerFee$Fee.Name <- gsub("§ 27.052 (A)(3)(a)  Right of Way - Driveway", "§ 27.052 (A)(3)(a) Right of Way - Driveway", tylerFee$Fee.Name)
grep("§ 27.052 (A)(3)(a) Right of Way - Driveway", tylerFee$Fee.Name)


transportationPermits <- grep("§", levels(as.factor(tylerFee$Fee.Name)), value = TRUE)
transportationPermits <- c(transportationPermits, 
                           grep("Transportation", setdiff(levels(as.factor(tylerFee$Fee.Name)), transportationPermits), value = TRUE, ignore.case = TRUE))
grep("Sewer", levels(as.factor(tylerFee$Fee.Name)), value = TRUE)
setdiff(levels(as.factor(tylerFee$Fee.Name)), transportationPermits)
transportationPermits <- c(transportationPermits, 
                           "New Driveway Deposit",
                           grep("MCRR", setdiff(levels(as.factor(tylerFee$Fee.Name)), transportationPermits), value = TRUE, ignore.case = TRUE))

setdiff(levels(as.factor(tylerFee$Fee.Name)), transportationPermits)
transportationPermits <- setdiff(transportationPermits, grep("MCC", tylerFee$Fee.Name, value = TRUE))
transportationPermits
grep("  ", transportationPermits, value = TRUE)
tylerFee$Fee.Name <- gsub("  ", " ", tylerFee$Fee.Name)
transportationPermits <- gsub("  ", " ", transportationPermits)

tylerFee$Fee.Name[tylerFee$Fee.Name == "UT § 27.052 (A)(3)(l) Temporary Closure Street"] <- "Temporary Closure Street for Utilities"
tylerFee$Fee.Name[tylerFee$Fee.Name == "§ 27.052 (A)(3)(l) Temporary Closure Street"] <- "Temporary Closure Street (Roads)"
tylerFee$Fee.Name[tylerFee$Fee.Name == "§ 27.052 (C) Right of Way Permit Fee"] <- "Right of Way Permit Fee (Roads)"
tylerFee$Fee.Name[tylerFee$Fee.Name == "§ 27.055 Street and Road Widening Permit"] <- "Street and Road Widening Permit"
tylerFee$Fee.Name[tylerFee$Fee.Name == "§ 27.056 Deposit for County Design, PR, Ins."] <- "Deposit for County Design, PR, Ins."
tylerFee$Fee.Name[tylerFee$Fee.Name == "§ 27.056 Miscellaneous Public Works Fees"] <- "Miscellaneous Public Works Fees"
tylerFee$Fee.Name[tylerFee$Fee.Name == "§ 29.506 Transportation Compatibility Assessment"] <- "Transportation Compatibility Assessment"
tylerFee$Fee.Name[tylerFee$Fee.Name == "§ 29.506 Transportation Planning Review"] <- "Transportation Planning Review"
tylerFee$Fee.Name[tylerFee$Fee.Name == "Bridge-§ 27.052 (A)(3)(l) Temporary Closure Street"] <- "Temporary Closure Street (Bridges)"
tylerFee$Fee.Name[tylerFee$Fee.Name == "Bridge § 27.052 (C) Right of Way Permit Fee"] <- "Right of Way Permit Fee (Bridges)"
tylerFee$Fee.Name[tylerFee$Fee.Name == "MCRR 26.150 TRANSPORTATION: Stormwater Review"] <- "Stormwater Review"
tylerFee$Fee.Name[tylerFee$Fee.Name == "MCRR 16.000: Road Rules Variance"] <- "Road Rules Variance"
tylerFee$Fee.Name[tylerFee$Fee.Name == "MCRR 18.135 Permit extension"] <- "Permit extension"
tylerFee$Fee.Name[tylerFee$Fee.Name == "MCRR 7.000: Transportation Impact Study Review"] <- "Transportation Impact Study Review"
tylerFee$Fee.Name[tylerFee$Fee.Name == "27.054 Road Vacation Feasibility"] <- "Road Vacation Feasibility"
tylerFee$Fee.Name[tylerFee$Fee.Name == "Appeal of Administrative Decision- Transportation"] <- "Appeal of Administrative Decision"

pattern_clean<- "^.*§.*?\\)\\s*|^(§.*?)\\s+"
tylerFee$Fee.Name <- sub(pattern_clean, "", tylerFee$Fee.Name)

transportationPermits[transportationPermits == "UT § 27.052 (A)(3)(l) Temporary Closure Street"] <- "Temporary Closure Street for Utilities"
transportationPermits[transportationPermits == "§ 27.052 (A)(3)(l) Temporary Closure Street"] <- "Temporary Closure Street (Roads)"
transportationPermits[transportationPermits == "§ 27.052 (C) Right of Way Permit Fee"] <- "Right of Way Permit Fee (Roads)"
transportationPermits[transportationPermits == "§ 27.055 Street and Road Widening Permit"] <- "Street and Road Widening Permit"
transportationPermits[transportationPermits == "§ 27.056 Deposit for County Design, PR, Ins."] <- "Deposit for County Design, PR, Ins."
transportationPermits[transportationPermits == "§ 27.056 Miscellaneous Public Works Fees"] <- "Miscellaneous Public Works Fees"
transportationPermits[transportationPermits == "§ 29.506 Transportation Compatibility Assessment"] <- "Transportation Compatibility Assessment"
transportationPermits[transportationPermits == "§ 29.506 Transportation Planning Review"] <- "Transportation Planning Review"
transportationPermits[transportationPermits == "Bridge-§ 27.052 (A)(3)(l) Temporary Closure Street"] <- "Temporary Closure Street (Bridges)"
transportationPermits[transportationPermits == "Bridge § 27.052 (C) Right of Way Permit Fee"] <- "Right of Way Permit Fee (Bridges)"
transportationPermits[transportationPermits == "MCRR 26.150 TRANSPORTATION: Stormwater Review"] <- "Stormwater Review"
transportationPermits[transportationPermits == "MCRR 16.000: Road Rules Variance"] <- "Road Rules Variance"
transportationPermits[transportationPermits == "MCRR 18.135 Permit extension"] <- "Permit extension"
transportationPermits[transportationPermits == "MCRR 7.000: Transportation Impact Study Review"] <- "Transportation Impact Study Review"
transportationPermits[transportationPermits == "27.054 Road Vacation Feasibility"] <- "Road Vacation Feasibility"
transportationPermits[transportationPermits == "Appeal of Administrative Decision- Transportation"] <- "Appeal of Administrative Decision"


pattern_clean<- "^.*§.*?\\)\\s*|^(§.*?)\\s+"
transportationPermits <- sub(pattern_clean, "", transportationPermits)

#temp1 <- as.data.frame(transportationPermits, temp)




# What share of permit applications are of what type of permits in FY 2026
temp1 <- tylerFee |> 
  filter(Fee.Name %in% transportationPermits) |>
  filter(Fiscal.Year == 2026) |> 
  group_by(Fee.Name) |> 
  summarise(Total = n())



temp1 <- tylerFee |> 
  filter(Fee.Name %in% transportationPermits) |>
  filter(Fiscal.Year == 2026) |> 
  group_by(Fee.Name) |> 
  filter(!grepl("Deposit", Fee.Name)) |> 
  summarise(Total = sum(Payment.Amount1))
new_row <- list(Fee.Name = "Overweight/Overdimentional Move", Total = 2566*8/15)
temp1 <- rbind(temp1, new_row)
temp1$percent <- round(temp1$Total*100/sum(temp1$Total), digits = 1)
sum(temp1$Total)
temp1$percent <- (temp1$Total*100/sum(temp1$Total))
temp1$percent <- round(temp1$Total*100/sum(temp1$Total), digits = 1)
ggplot(data = temp1, aes(reorder(Fee.Name, Total), percent)) +
  geom_col() +
  geom_text(aes(label = percent), hjust = -0.2, size = 2.5) +
  coord_flip()+
  labs(title = "Share of Flat-Fee Revenue by Permit Type", 
       subtitle = "(Between July 1, 2025 to Nov 12, 2025)") +
  xlab("Permit Types") +
  ylab("Percent of Flat-Fee Revenue")


#################################################################

# What share of permit applications are of what type of permits in FY 2025 
temp1 <- tylerFee |> 
  filter(Fee.Name %in% transportationPermits) |>
  filter(Fiscal.Year == 2025) |> 
  group_by(Fee.Name) |> 
  filter(!grepl("Deposit", Fee.Name)) |> 
  filter(Fee.Name == "Appeal of Administrative Decision- Transportation")

temp1 <- tylerFee |> 
  filter(Fee.Name %in% transportationPermits) |>
  filter(Fiscal.Year == 2025) |> 
  group_by(Fee.Name) |> 
  filter(!grepl("Deposit", Fee.Name)) |> 
  summarise(Total = sum(Payment.Amount1))
new_row <- list(Fee.Name = "Overweight/Overdimentional Move", Total = 2566*8/5)
temp1 <- rbind(temp1, new_row)
temp1$percent <- round(temp1$Total*100/sum(temp1$Total), digits = 1)
sum(temp1$Total)
temp1 <- subset(temp1, temp1$Fee.Name != "Permit extension")
ggplot(data = temp1, aes(reorder(Fee.Name, Total), percent)) +
  geom_col() +
  geom_text(aes(label = percent), hjust = -0.2, size = 2.5) +
  coord_flip()+
  labs(title = "Share of Flat-Fee Revenue by Permit Type", 
       subtitle = "(Between July 1, 2024 to June 30, 2025)") +
  xlab("Permit Types") +
  ylab("Percent of Flat-Fee Revenue")


#################################################################

temp1 <- tylerFee |> 
  filter(Fee.Name %in% transportationPermits) |> 
  group_by(Fee.Name) |> 
  summarise(Total = n())


ggplot(data = temp1, aes(reorder(Fee.Name, Total), Total)) +
  geom_col() +
  geom_text(aes(label = Total), hjust = -0.2, size = 2.5) +
  coord_flip()+
  labs(title = "Number of Permits processed", 
       subtitle = "(Between Oct 27, 2023 to Nov 9, 2025)") +
  xlab("Permit Types") +
  ylab("Number of permits")
sum(temp1$Total)

temp1 <- tylerFee |> 
  filter(Fee.Name %in% transportationPermits) |> 
  group_by(Fee.Name, Fiscal.Year) |> 
  filter(!grepl("Deposit", Fee.Name)) |> 
  summarise(Total.Amount = sum(Payment.Amount1), .groups = 'drop')

new_row1 <- list(Fee.Name = "Overweight/Overdimentional Move", Fiscal.Year = 2024, Total.Amount = 2566*8/30)
new_row2 <- list(Fee.Name = "Overweight/Overdimentional Move", Fiscal.Year = 2025, Total.Amount = 2566*8/5)
new_row3 <- list(Fee.Name = "Overweight/Overdimentional Move", Fiscal.Year = 2026, Total.Amount = 2566*8/15)
temp1 <- rbind(temp1, new_row1, new_row2, new_row3)

temp_totals <- temp1 |>
  group_by(Fee.Name) |>
  summarise(Total.Sum = sum(Total.Amount))
temp1 <- subset(temp1, temp1$Fee.Name != "Permit extension")

ggplot(data = temp1, aes(x = reorder(Fee.Name, Total.Amount, FUN = sum), y = Total.Amount)) +
  geom_col(aes(fill = as.factor(Fiscal.Year))) +
  coord_flip() +
  labs(title = "Permit Fee revenue by Permit type", 
       subtitle = "Excluding Deposits and between Oct 2023 and Oct 2025", 
       x = "Fee Name", 
       y = "Total Dollar Amount",
       fill = "Fiscal Year") 


temp1 <- tylerFee |> 
  filter(Fee.Name %in% transportationPermits) |> 
  group_by(Fee.Name) |> 
  filter(!grepl("Deposit", Fee.Name)) |> 
  summarise(Total.Amount = sum(Payment.Amount))

ggplot(data = temp1, aes(reorder(Fee.Name, Total.Amount), Total.Amount)) +
  geom_col() +
  geom_text(aes(label = Total.Amount), hjust = -0.2, size = 2.5) +
  coord_flip() +
  labs(title = "Permit Fee revenue by Permit type", subtitle = "Excluding Deposits and between Oct 2023 and Oct 2025", x = "Fee Name", y = "Total Dollar Amount")


temp1 <- tylerFee |> 
  filter(Fee.Name %in% transportationPermits) |> 
  group_by(Fee.Name) |> 
  filter(Fiscal.Year == 2024) |> 
  filter(!grepl("Deposit", Fee.Name)) |> 
  summarise(Total.Amount = sum(Payment.Amount))

ggplot(data = temp1, aes(reorder(Fee.Name, Total.Amount), Total.Amount)) +
  geom_col() +
  geom_text(aes(label = Total.Amount), hjust = -0.2, size = 2.5) +
  coord_flip() +
  labs(title = "FY 2024 Permit Fee revenue by Permit type", subtitle = "Excluding Deposits", x = "Fee Name", y = "Total Dollar Amount")

temp1 <- tylerFee |> 
  filter(Fee.Name %in% transportationPermits) |> 
  group_by(Fee.Name) |> 
  filter(Fiscal.Year == 2025) |> 
  filter(!grepl("Deposit", Fee.Name)) |> 
  summarise(Total.Amount = sum(Payment.Amount))

ggplot(data = temp1, aes(reorder(Fee.Name, Total.Amount), Total.Amount)) +
  geom_col() +
  geom_text(aes(label = Total.Amount), hjust = -0.2, size = 2.5) +
  coord_flip() +
  labs(title = "FY 2025 Permit Fee revenue by Permit type", subtitle = "Excluding Deposits", x = "Fee Name", y = "Total Dollar Amount")



temp1 <- tylerFee |> 
  filter(Fee.Name %in% transportationPermits) |> 
  group_by(Fee.Name) |> 
  filter(Fiscal.Year == 2026) |> 
  filter(!grepl("Deposit", Fee.Name)) |> 
  summarise(Total.Amount = sum(Payment.Amount))

ggplot(data = temp1, aes(reorder(Fee.Name, Total.Amount), Total.Amount)) +
  geom_col() +
  geom_text(aes(label = Total.Amount), hjust = -0.2, size = 2.5) +
  coord_flip() +
  labs(title = "FY 2026 Permit Fee revenue by Permit type", subtitle = "Excluding Deposits", x = "Fee Name", y = "Total Dollar Amount")



temp1 <- tylerFee |> 
  filter(Fee.Name %in% transportationPermits) |> 
  group_by(Fee.Name, Fiscal.Year) |> 
  summarise(Total = n(),
            TotalAmount = sum(Amount.Paid, na.rm = TRUE)) |> 
  ungroup()

temp1 <- subset(temp1, temp1$Fiscal.Year == 2025)
sum(temp1$TotalAmount)

############################################### Old Permits #########################################################

file_path <- file.choose()
OldPermits <- read.csv(file_path)
OldPermits$IssueDate1 <- as.Date(OldPermits$IssueDate)
OldPermits$Fiscal.Year <- ifelse(
  as.numeric(format(OldPermits$IssueDate1, "%m")) >= 7,
  as.numeric(format(OldPermits$IssueDate1, "%Y")) + 1,
  as.numeric(format(OldPermits$IssueDate1, "%Y"))
)

OldPermits$PermitCategory[grep("Bridge Event", OldPermits$PermitCategory)] <- "Bridge Event"
OldPermits$PermitCategory[grep("Film-Video", OldPermits$PermitCategory)] <- "Film-Video"
OldPermits$PermitCategory[grep("Right-Of-Way", OldPermits$PermitCategory)] <- "Right-Of-Way"
OldPermits$PermitCategory[grep("Special Event", OldPermits$PermitCategory)] <- "Special Event"

OldPermits$PermitCategory <- as.factor(OldPermits$PermitCategory)
levels(OldPermits$PermitCategory) <- c("c) Bridge Event", 
                                       "f) Film-Video",
                                       "e) Over Dimension-Overweight", 
                                       "b) Right-Of-Way",
                                       "d) Special Event",
                                       "a) Utility")

desc_levels <- sort(unique(temp1$PermitCategory), decreasing = TRUE)
temp1$PermitCategory <- factor(temp1$PermitCategory, levels = desc_levels)

temp1 <- OldPermits |> 
  group_by(PermitCategory, Fiscal.Year) |> 
  summarise(Total = n()) |> 
  ungroup()
desc_levels <- sort(unique(temp1$PermitCategory), ascending = TRUE)
temp1$PermitCategory <- factor(temp1$PermitCategory, levels = desc_levels)


ggplot(temp1, aes(x = Fiscal.Year, y = Total, fill = PermitCategory)) +
  geom_col(position = "stack") +
  labs(
    title = "Permits by Fiscal Year",
    x = "Fiscal Year",
    y = "Number of permits issued",
    fill = "Permit Category"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#######################################################################################################
#############   SPECIAL EVENTS PERMIT FEE COMPARISON  #################################################
#######################################################################################################
file_path <- file.choose()
SpecialEvents <- read.csv(file_path)
library(tidyverse)
data <- SpecialEvents %>%
  mutate(
    Low = Low,
    High = High,
    Mid = (Low + High) / 2
    ) %>%
  select(County, Low, Mid, High)


plot_data <- data %>%
  # Sort Shelter factor levels by the Cat.mid value
  mutate(Shelter = fct_reorder(County, Mid)) %>%
  # Reshape data from wide to long format
  pivot_longer(
    cols = c(Low, High, Mid),
    names_to = "fee_type",
    values_to = "fee_value"
  ) %>%
  # Remove any rows where the fee_value is NA (for single-number shelters)
  filter(!is.na(fee_value))

# 3. CREATE THE PLOT
ggplot(plot_data, aes(x = fee_value, y = Shelter, color = fee_type)) +
  geom_point(size = 4) + # Add the points
  scale_color_manual( # Manually set the colors
    name = "Fee Type", # Legend title
    labels = c("High" = "High", "Mid" = "Mid", "Low" = "Low"),
    values = c("High" = "red", "Mid" = "orange", "Low" = "green")
  ) +
  labs( # Add titles and labels
    title = "Special Events Permit Fee Range by County",
    subtitle = "Sorted by midpoint Special Events Permit Fee",
    x = "Special Events Permit Fee (in $)",
    y = "County"
  ) +
  scale_x_continuous(labels = scales::label_dollar()) +
  geom_line(aes(group = Shelter), color = "gray")+
  theme_minimal() + # Use a clean theme
  theme(panel.grid.major.y = element_blank()) # Remove horizontal grid lines
