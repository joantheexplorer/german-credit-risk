# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- STATS 203: Statistics and Probability for CS/IT Students ----
# ---- Members: Aviles, Artates, Acob, Alapag (BSCS 2-4) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ---- Load Needed Libraries ----
library(readxl)
library(dplyr)
library(ggplot2)

# ---- Import German Credit Risk ----
German_Credit_Risk <- read_excel("German-Credit-Risk.xlsx")
View(German_Credit_Risk)
DATA <- German_Credit_Risk

# ---- Compute Statistics ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- 1. Custom Functions ----

# Monthly payment 
monthly_payment <- function(amount, months) {
  payment = amount/months
  return(payment)
}

# Demonstration: 
demo_payment <- monthly_payment(5000, 12)
print(demo_payment)

# Rock, Paper, Scissors 
evaluate_rps <- function(player1, player2) {
  
  if (player1 == player2) {
    result <- "It's a Tie!"
  } else if ((player1 == "Rock" && player2 == "Scissors") ||
             (player1 == "Paper" && player2 == "Rock") ||
             (player1 == "Scissors" && player2 == "Paper")) {
    result <- "Player 1 Wins!"
  } else {
    result <- "Player 2 Wins!"
  }
  
  return(result)
}

# Demonstration
evaluate_rps("Rock", "Scissors")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- 2. Data Structures ----
# Vector 
v_members <- c("Aviles", "Acob", "Artates", "Alapag")
v_os_list <- c("Linux", "macOS", "Windows", "Windows")        
v_german_duration <- DATA$Duration

# Factor: Unordered
f_nationality <- factor(c("Filipino", "Filipino", "Filipino")) 
f_os <- factor(c("Windows", "macOS", "Linux", "Android")) 
f_german_housing <- factor(Housing)
f_german_risk <- factor(Risk)

# Factor: Ordered
f_job <- factor(Job, levels = c(0, 1, 2, 3), ordered = TRUE)
f_Saving_accounts <- factor(Saving_accounts, levels = c("little", "moderate", "quite rich", "rich"), ordered = TRUE)
f_generation <- factor(c("Millennial", "Gen Z"), levels = c("Gen Z", "Millennial"), ordered = TRUE)  

# Table
t_savings_risk <- table(Saving_accounts, Risk)
t_purpose_duration <- table(Purpose, Duration, Risk)

# Data Frame
df_students <- data.frame(
  Name = v_members,
  Operating_System = v_os_list,
  Section = "BSCS 2-4"
)

df_german_summary <- DATA %>% group_by(Housing) %>% summarize(Avg_loan_duration = mean(Duration))


# ---- 3. Data Manipulations ----
generation_summary <- DATA %>%
  mutate(Generation = ifelse(Age <= 29, "Gen Z",
                             ifelse(Age <= 45, "Millennial",
                                    ifelse(Age <= 61, "Gen X", "Baby Boomer")))) %>%
  filter(Generation != "Baby Boomer") %>%
  select(Generation, Credit_amount, Duration, Risk) %>%
  mutate(installment = monthly_payment(Credit_amount, Duration)) %>%
  group_by(Generation) %>%
  summarize(
    Bad_Risk_Percent = mean(Risk == "bad") * 100,
    Avg_Monthly_Payment = mean(installment, na.rm = TRUE),
    Total_Count = n()
    )

print(generation_summary)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- 4. Data Visualization ----

# 1. Base R: Risk by Loan Purpose
purpose_risk_tab <- table(DATA$Risk, DATA$Purpose)
barplot(purpose_risk_tab,
        beside = TRUE,
        las = 2,                   
        col = c("#C0392B", "#2E6FAB"),
        main = "Risk Distribution by Loan Purpose",
        xlab = "",                
        ylab = "Frequency",
        legend = rownames(purpose_risk_tab),
        args.legend = list(x = "topright", bty = "n"))

# 2. ggplot2: Savings Tier vs Risk
ggplot(DATA, aes(x = Saving_accounts, fill = Risk)) +
  geom_bar(position = "dodge") +
  labs(title = "Credit Risk Distribution by Savings Tier",
       x = "Savings Account Status", y = "Number of Applicants", fill = "Credit Risk") +
  theme_minimal()

# 3. ggplot2: Housing and Job to Avg Duration
heatmap_data <- DATA %>%
  group_by(Housing, Job) %>%
  summarize(Avg_Duration = mean(Duration, na.rm = TRUE))

ggplot(heatmap_data, aes(x = Housing, y = factor(Job), fill = Avg_Duration)) +
  geom_tile() +
  scale_fill_gradient(low = "#EAF1F8", high = "#2C3E6B") +
  theme_minimal() +
  labs(title = "Heatmap: Avg Duration by Housing and Job Level",
       x = "Housing Status", y = "Job Skill Level", fill = "Avg Duration")

# 4. Base R: Credit Amount by Risk
boxplot(DATA$Credit_amount[DATA$Risk == "good"],
        DATA$Credit_amount[DATA$Risk == "bad"],
        names = c("Good", "Bad"),
        main = "Credit Amount Distribution by Risk",
        xlab = "Credit Risk",
        ylab = "Credit Amount (DM)",
        col = c("#2E6FAB", "#C0392B"))

# ---- End of Program ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~