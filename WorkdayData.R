library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(forcats)

# Choose a file using a dialog box
file_path <- file.choose()

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
                        paste0("FY", year(Accounting.Date1) + 1),
                        paste0("FY", year(Accounting.Date1)))
  )
temp <- as.data.frame(feesCollected$Accounting.Date1, feesCollected$FY)
# Display the structure of the data frame and the type of each column
str(feesCollected)

ggplot(feesCollected, aes(x = Journal.Source, y = Amount)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Collected Fees by Journal Source",
       x = "Journal Source",
       y = "Amount Collected") +
  coord_flip() +
  coord_cartesian(ylim = c(-1000, 500)) 

ggplot(feesCollected, aes(x = Project.ID, y = Amount)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Collected Fees by Journal Source",
       x = "Journal Source",
       y = "Amount Collected") +
  coord_flip() +
  coord_cartesian(ylim = c(-10000, 500)) 
  

ggplot(feesCollected, aes(x = Cost.Center, y = Amount)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Collected Fees by Journal Source",
       x = "Journal Source",
       y = "Amount Collected") +
  coord_flip() +
  coord_cartesian(ylim = c(-1000, 500)) 

ggplot(feesCollected, aes(x = Primary.Cost.ObjectCategory, y = Amount)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Collected Fees by Journal Source",
       x = "Journal Source",
       y = "Amount Collected") +
  coord_cartesian(ylim = c(-1000, 500)) + 
  coord_flip()

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

yearly_Ledger.Account <- feesCollected %>%
  group_by(FY, Ledger.Account) %>%
  summarise(
    TotalAmount = sum(Amount, na.rm = TRUE)
  ) %>%
  ungroup()

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

ggplot(yearly_Ledger.Account, aes(x = FY, y = -1*TotalAmount, fill = Ledger.Account)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = dollar(-1*TotalAmount)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Permit Fee collection varies less across years than\ncollection from Charges for Services",
    x = "Fiscal Year",
    y = "Total Amount ($)",
    fill = "Ledger Account"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
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

tylerFee$Amount.Paid <- as.numeric(gsub("[\\$,]", "", tylerFee$Amount.Paid))

tylerFee$Payment.Date1 <- as.Date(tylerFee$Payment.Date, format = "%m/%d/%Y")
tylerFee$Fiscal.Year <- ifelse(
  as.numeric(format(tylerFee$Payment.Date1, "%m")) >= 7,
  as.numeric(format(tylerFee$Payment.Date1, "%Y")) + 1,
  as.numeric(format(tylerFee$Payment.Date1, "%Y"))
)

transportationPermits <- grep("§", levels(as.factor(tylerFee$Fee.Name)), value = TRUE)
transportationPermits <- c(transportationPermits, 
                           grep("Transportation", setdiff(levels(as.factor(tylerFee$Fee.Name)), transportationPermits), value = TRUE, ignore.case = TRUE))

setdiff(levels(as.factor(tylerFee$Fee.Name)), transportationPermits)
transportationPermits <- c(transportationPermits, 
                           "Lane Closure Fee", 
                           "New Driveway Deposit",
                           grep("MCRR", setdiff(levels(as.factor(tylerFee$Fee.Name)), transportationPermits), value = TRUE, ignore.case = TRUE))

setdiff(levels(as.factor(tylerFee$Fee.Name)), transportationPermits)
transportationPermits


temp1 <- tylerFee |> 
  filter(Fee.Name %in% transportationPermits) |> 
  group_by(Fee.Name) |> 
  summarise(Total = n())

ggplot(data = temp1, aes(reorder(Fee.Name, Total), Total)) +
  geom_col() +
  geom_text(aes(label = Total), hjust = -0.2, size = 2.5) +
  coord_flip()
sum(temp1$Total)


temp1 <- tylerFee |> 
  filter(Fee.Name %in% transportationPermits) |> 
  group_by(Fee.Name, Fiscal.Year) |> 
  filter(Fiscal.Year == 2025) |> 
  filter(!grepl("Deposit", Fee.Name)) |> 
  summarise(Total.Amount = sum(Amount.Paid))

ggplot(data = temp1, aes(reorder(Fee.Name, Total.Amount), Total.Amount)) +
  geom_col() +
  geom_text(aes(label = Total.Amount), hjust = -0.2, size = 2.5) +
  coord_flip() +
  labs(title = "FY 2025 Permit Fee revenue by Permit type", subtitle = "Excluding Deposits", x = "Fee Name")





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
