# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- STATS 203: Statistics and Probability for CS/IT Students ----
# ---- Members: Aviles, Artates, Acob, Alapag (BSCS 2-4) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ---- Load Needed Libraries ----
library(readxl) # Read excel file
library(dplyr) # For data manipulation
library(ggplot2) # For visuals

# ---- Import German Credit Risk ----
German_Credit_Risk <- read_excel("German-Credit-Risk.xlsx")
View(German_Credit_Risk)
DATA <- German_Credit_Risk

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- 1. Custom Functions ----

# Function to calculate Monthly payment (amortization)
monthly_payment <- function(amount, months) {
  payment = amount/months
  return(payment)
}

# Demonstration of monthly payment function: 
demo_payment <- monthly_payment(5000, 12)
print(demo_payment)

# Function for Rock, Paper, Scissors 
# Takes the move of player 1 and player 2. They can choose between: 
# Rock, Scissors, Paper (Case sensitive)

evaluate_rps <- function(player1, player2) {
  
  if (player1 == player2) {
    result <- "It's a Tie!"
  } else if ((player1 == "Rock" && player2 == "Scissors") ||
             (player1 == "Paper" && player2 == "Rock") ||
             (player1 == "Scissors" && player2 == "Paper")) {
    result <- "Player 1 Wins!"
  } else if ((player2 == "Rock" && player1 == "Scissors") ||
             (player2 == "Paper" && player1 == "Rock") ||
             (player2 == "Scissors" && player1 == "Paper")) {
    result <- "Player 2 Wins!"
  } else {
    result <- "Invalid Input"
  }
  
  return(result)
}

# Demonstration for Rock, Paper, Scissors function
evaluate_rps("Rock", "Scissors")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- 2. Data Structures ----
# ---- Vector ----
v_members <- c("Aviles", "Acob", "Artates", "Alapag") 
v_age_list <- c(19L, 19L, 20L, 21L) # Vector of Ages (Integers)
v_german_duration <- DATA$Duration # Vector of Duration (Numeric) extracted from imported excel

# ---- Factor: Unordered ----
f_nationality <- factor(c("Filipino", "German", "Chinese")) 
f_os <- factor(c("Windows", "macOS", "Linux", "Android")) 
f_german_housing <- factor(Housing) # Factor of Housing extracted from imported excel
f_german_risk <- factor(Risk) # Factor of Risk extracted from imported excel

# ---- Factor: Ordered ----
f_job <- factor(Job, levels = c(0, 1, 2, 3), ordered = TRUE) # Factor of Job extracted from imported excel
f_Saving_accounts <- factor(Saving_accounts, levels = c("NA", "little", "moderate", "quite rich", "rich"), ordered = TRUE) # Factor of Saving accounts extracted from imported excel
f_generation <- factor(c("Millennial", "Gen Z", "Gen X", "Baby Boomer"), levels = c("Gen Z", "Millennial", "Gen X", "Baby Boomer"), ordered = TRUE)  

# ---- Table ----
# One-variable Table: Duration
t_duration <- table(Duration)
print(t_duration)

# Two-variable Table: Saving accounts of applicant and Credit Risk
t_savings_risk <- table(Saving_accounts, Risk)
print(t_savings_risk)

# Three Variable Table: Duration of credit, Purpose of credit, Credit Risk
t_purpose_duration <- table(Purpose, Duration, Risk)
print(t_purpose_duration)

# ---- Data Frame ----
# Data Frame 1: Vector name, vector age, and section variable
df_students <- data.frame(
  Name = v_members,
  Age = v_age_list,
  Section = "BSCS 2-4"
)

# Data Frame 2: Housing and the Credit Duration 
df_german_summary <- DATA %>% group_by(Housing) %>% summarize(Avg_credit_duration = mean(Duration))


# ---- 3. Data Manipulations ----
generation_summary <- DATA %>%
  
  # ---- Mutate ---- 
  # Create a new column called Generation based on the Age column
  mutate(Generation = ifelse(Age <= 29, "Gen Z",
                             ifelse(Age <= 45, "Millennial",
                                    ifelse(Age <= 61, "Gen X", "Baby Boomer")))) %>%
  
  # ---- Filter ----
  # Filter out Baby Boomer Generation
  filter(Generation != "Baby Boomer") %>%
  
  # ---- Select ----- 
  # Select the needed columns for the new dataset
  select(Generation, Credit_amount, Duration, Risk) %>%
  
  # ---- Mutate ----
  # Calculate the monthly payment using the function
  mutate(installment = monthly_payment(Credit_amount, Duration)) %>% # Using the monthly payment function earlier
  
  # ---- Group By---- 
  # Grouping by Generation
  group_by(Generation) %>%
  
  # ---- Summarize ----
  summarize(
    # What percentage of this generation has a bad risk?
    Bad_Risk_Percent = mean(Risk == "bad") * 100,
    
    # What is the average monthly payment for this generation?
    Avg_Monthly_Payment = mean(installment, na.rm = TRUE),
    
    # How many applicants are in this generation total?
    Total_Count = n()
    )

print(generation_summary)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- 4. Data Visualization ----

# ---- 1. Base R graphics: Risk by Loan Purpose ----
# Question: What is the frequency for good and bad credit risk across different loan purposes?

par(mar = c(7, 4, 2, 4)) 
purpose_risk_tab <- table(DATA$Risk, DATA$Purpose)
barplot(purpose_risk_tab,
        beside = TRUE,
        las = 2,                   
        col = c("#C0392B", "#2E6FAB"),
        main = "Risk Distribution by Loan Purpose",
        xlab = "",                
        ylab = "Frequency",
        legend = rownames(purpose_risk_tab),
        args.legend = list(x = "topright", bty = "n"),
        cex.names = 0.7)

# Analysis: The bar graph shows that loan frequencies for cars and radio/TV 
# are the highest, with "good" loan outcomes significantly outnumbering the 
# "bad" ones across all categories. Across every purpose, "good" loan 
# outcomes significantly outnumber "bad" ones, indicating a general trend of 
# successful repayments. Meanwhile, loans for appliances, repairs, and 
# vacation/others represent the smallest portion in the dataset, making 
# their risk patterns less pronounced visually.

# ---- 2. ggplot2: Savings Tier vs Risk ----
# Question: 
ggplot(DATA, aes(x = f_Saving_accounts, fill = Risk)) + # Using the factor of Savings Account earlier
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("bad" = "#C0392B", "good" = "#2E6FAB")) +
  labs(title = "Credit Risk Distribution by Savings Tier",
       x = "Savings Account", y = "Number of Applicants", fill = "Credit Risk") +
  theme_minimal()

# Analysis: The bar chart indicates that the vast majority of applicants 
# have "little" savings, which also accounts for the highest volume of both 
# "good" and "bad" credit risks. The "NA" category also shows a high ratio 
# of "good" credit risk compared to the "bad" ones, suggesting that overall, 
# across all applicants with varying amounts of savings, majority have 
# "good" credit risk. Additionally, as the savings tier moves from "NA" to 
# "rich," the total number of credit loan applicants drops significantly.


# 3. ggplot2: Housing and Job to Avg Duration
# Question: Which combination of housing type and job skill level shows the highest and lowest average duration?
heatmap_data <- DATA %>%
  group_by(Housing, Job) %>%
  summarize(Avg_Duration = mean(Duration, na.rm = TRUE), .groups = 'drop')

ggplot(heatmap_data, aes(x = Housing, y = factor(Job), fill = Avg_Duration)) +
  geom_tile(color = "white") + 
  geom_text(aes(label = round(Avg_Duration, 1)), color = "black") + 
  scale_fill_gradient(low = "#EAF1F8", high = "#2E6FAB") +
  theme_minimal() +
  labs(title = "Heatmap: Avg Duration by Housing and Job Level",
       x = "Housing", 
       y = "Job Skill Level", 
       fill = "Avg Duration")

# Analysis: Across all skill levels, individuals with free housing 
# consistently have the highest average durations, while those who own homes 
# tend to have the lowest. Renters fall in between but are closer to 
# homeowners. The highest average duration appears among highly skilled 
# workers with free housing, while the lowest is among low-skill workers
# who own homes. This suggests that both job skill level and housing status 
# influence duration, with free housing and higher skills linked to longer 
# averages.


# 4. Base R graphics: Credit Amount by Risk
# Question: Is there a significant difference in credit amounts between borrowers with good and bad credit risk?
boxplot(DATA$Credit_amount[DATA$Risk == "good"],
        DATA$Credit_amount[DATA$Risk == "bad"],
        names = c("Good", "Bad"),
        main = "Credit Amount Distribution by Risk",
        xlab = "Credit Risk",
        ylab = "Credit Amount",
        col = c("#2E6FAB", "#C0392B"))

# Analysis: The boxplot shows that borrowers classified as bad credit risk 
# generally receive higher credit amounts than those with good credit risk,
# as seen from their higher median value. The spread of credit amounts is 
# also wider among bad-risk borrowers, indicating greater variability in 
# the loans they receive. Both groups have several high-value outliers, but 
# extreme large loans are more common in the bad-risk group. Overall, credit 
# amount tends to be larger and less consistent for borrowers with bad 
# credit risk compared to those with good credit risk.


# ---- End of Program ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~