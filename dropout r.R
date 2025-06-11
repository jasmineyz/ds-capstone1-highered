################################################################################
# STUDENT DROPOUT PREDICTION ANALYSIS
# HarvardX PH125.9x - Data Science Capstone 2
# Data cleaning, feature engineering, EDA, model building, and validation
################################################################################

# 0. Load Packages

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")

################################################################################
# STEP 1: Data cleaning
################################################################################

rawdata <- read_csv2("https://raw.githubusercontent.com/jasmineyz/ds-capstone2-dropoutprediction/refs/heads/main/predict%20student%20dropout%20data.csv")
clean <- rawdata

## 1.1. Target variable (combine enrolled/graduate; relabel/factor)
clean$Target <- tolower(trimws(clean$Target))
clean$Target[clean$Target %in% c("enrolled", "graduate")] <- "on_track"
clean$Target <- factor(clean$Target, levels = c("dropout", "on_track"))

# Check levels
setNames(seq_along(levels(clean$Target)), levels(clean$Target))


## 1.2. Marital Status (map codes to labels, drop old col)
clean$Marital_Status <- factor(
  clean$`Marital status`,
  levels = 1:6,
  labels = c("single", "married", "widower", "divorced", "facto_union", "legally_separated")
)

clean$`Marital status` <- NULL

# check mapping
setNames(seq_along(levels(clean$Marital_Status)), levels(clean$Marital_Status))

## 1.3. Application Mode (code to label, drop old col)
clean$Application_mode <- factor(clean$`Application mode`)

code_labels <- c(
  "1" = "1st_phase_general",
  "2" = "Ordinance_612_93",
  "5" = "1st_phase_special_Azores",
  "7" = "other_higher_courses",
  "10" = "Ordinance_854_B_99",
  "15" = "international_student",
  "16" = "1st_phase_special_Madeira",
  "17" = "2nd_phase_general",
  "18" = "3rd_phase_general",
  "26" = "Ordinance_533_A_b2",
  "27" = "Ordinance_533_A_b3",
  "39" = "over_23",
  "42" = "transfer",
  "43" = "change_of_course",
  "44" = "tech_diploma",
  "51" = "change_institution_course",
  "53" = "short_cycle_diploma",
  "57" = "change_inst_intl"
)
# Convert codes to character for matching
clean$Application_mode <- factor(
  as.character(clean$`Application mode`),
  levels = names(code_labels),
  labels = code_labels
)

setNames(seq_along(levels(clean$Application_mode)), levels(clean$Application_mode))

rm(code_labels)
clean$`Application mode` <- NULL

## 1.4. Application Order (keep as integer for models to recognize the ordinal nature of the variable.)
clean$Application_order <- as.integer(clean$`Application order`)
clean$`Application order` <- NULL

## 1.5. Course/Major (rename and label)
names(clean)[names(clean) == "Course"] <- "Major"

clean$Major <- factor(clean$Major)
major_labels <- c(
  "33" = "Biofuel_Production_Tech",
  "171" = "Animation_Multimedia_Design",
  "8014" = "Social_Service_Evening",
  "9003" = "Agronomy",
  "9070" = "Communication_Design",
  "9085" = "Veterinary_Nursing",
  "9119" = "Informatics_Engineering",
  "9130" = "Equinculture",
  "9147" = "Management",
  "9238" = "Social_Service",
  "9254" = "Tourism",
  "9500" = "Nursing",
  "9556" = "Oral_Hygiene",
  "9670" = "Advertising_Marketing_Mgmt",
  "9773" = "Journalism_Communication",
  "9853" = "Basic_Education",
  "9991" = "Management_Evening"
)

clean$Major <- factor(
  as.character(clean$Major),
  levels = names(major_labels),
  labels = major_labels
)

setNames(levels(clean$Major), names(major_labels))

rm(major_labels)
clean$Course <- NULL

## 1.6. Column renaming and removals (to simplify and avoid redundancy)
names(clean)[names(clean) == "Daytime/evening attendance"] <- "Is_Daytime_Attendance"

# Note: "Previous qualification" and "Previous qualification (grade)" are related.
# To avoid redundancy and for modeling, we will keep only "Previous qualification (grade)".
# We will remove "Previous qualification" and rename "Previous qualification (grade)" for code-friendliness.
clean$`Previous qualification` <- NULL

names(clean)[names(clean) == "Previous qualification (grade)"] <- "PriorGrade"

# Note: We are not going to differentiate by specific nationality.
# Therefore, we will delete the "Nacionality" column and only use the "International" column as the indicator for international status.

clean$Nacionality <- NULL

names(clean)[names(clean) == "International"] <- "Is_International"  # 1 = yes, 0 = no

## 1.7. Parent education: make binary indicator
# Note: The detailed parental qualification and occupation columns are highly granular.
# For modeling, we will use a single binary variable: "Family_Postsecondary_Edu".
# It is 1 if either parent attained postsecondary (college+) education, 0 otherwise.
# Postsecondary codes are: 2, 3, 4, 5, 6, 40, 41, 42, 43, 44
postsec_codes <- c(2, 3, 4, 5, 6, 40, 41, 42, 43, 44)

clean$Parent_Higher_Edu <-
  ifelse(
    clean$`Mother's qualification` %in% postsec_codes |
      clean$`Father's qualification` %in% postsec_codes, 1, 0
  )

# Remove detailed columns
clean$`Mother's qualification` <- NULL
clean$`Father's qualification` <- NULL
clean$`Mother's occupation` <- NULL
clean$`Father's occupation` <- NULL

rm(postsec_codes)

## 1.8. Other column cleanup and renaming
names(clean)[names(clean) == "Admission grade"] <- "AdmissionGrade"

names(clean)[names(clean) == "Educational special needs"] <- "SpecialNeeds"

# Note: "Tuition fees up to date" is being removed because it is not relevant 
# for predicting student dropout or academic success at the time of admission, 
# and may reflect outcomes rather than predictors.
clean$`Tuition fees up to date` <- NULL
names(clean)[names(clean) == "Gender"] <- "Is_Male"  # 1 = male, 0 = female
names(clean)[names(clean) == "Scholarship holder"] <- "HasScholarship"
names(clean)[names(clean) == "Age at enrollment"] <- "AgeAtEnroll"

## 1.9. Curricular units 1st/2nd sem: keep only "grade" and "enrolled"
# Note: For modeling student success, we are keeping only:
# - "Curricular units 1st sem (grade)" (average grade): This reflects the student's overall academic performance in the first semester, which is highly predictive of future success.
# - "Curricular units 1st sem (enrolled)" (number registered): This indicates the student's course load/engagement.
# We are removing the other 1st semester curricular unit columns ("credited", "approved", "evaluations", "without evaluations") because they are either highly correlated, less directly related to current academic progress, or add redundancy.
clean$`Curricular units 1st sem (credited)` <- NULL
clean$`Curricular units 1st sem (evaluations)` <- NULL
clean$`Curricular units 1st sem (approved)` <- NULL
clean$`Curricular units 1st sem (without evaluations)` <- NULL

names(clean)[names(clean) == "Curricular units 1st sem (grade)"] <- "Sem1_AvgGrade"
names(clean)[names(clean) == "Curricular units 1st sem (enrolled)"] <- "Sem1_UnitsEnrolled"

clean$Sem1_AvgGrade <- as.numeric(clean$Sem1_AvgGrade)
clean$Sem1_AvgGrade <- round(clean$Sem1_AvgGrade, 1)

# Do similar thing to the second semester
clean$`Curricular units 2nd sem (credited)` <- NULL
clean$`Curricular units 2nd sem (evaluations)` <- NULL
clean$`Curricular units 2nd sem (approved)` <- NULL
clean$`Curricular units 2nd sem (without evaluations)` <- NULL

names(clean)[names(clean) == "Curricular units 2nd sem (grade)"] <- "Sem2_AvgGrade"
names(clean)[names(clean) == "Curricular units 2nd sem (enrolled)"] <- "Sem2_UnitsEnrolled"

clean$Sem2_AvgGrade <- as.numeric(clean$Sem2_AvgGrade)
clean$Sem2_AvgGrade <- round(clean$Sem2_AvgGrade, 1)

## 1.10. Macroeconomic
# Note: We are removing the "GDP" variable because job opportunity (as reflected by the unemployment rate)
# is a more direct and relevant factor for student outcomes in our analysis.
# Since we are already including the unemployment rate, GDP is not needed.
names(clean)[names(clean) == "Unemployment rate"] <- "UnemploymentRate"

# UnemploymentRate fix
clean$UnemploymentRate <- clean$UnemploymentRate / 10

names(clean)[names(clean) == "Inflation rate"] <- "InflationRate"
clean$InflationRate <- as.numeric(clean$InflationRate)

clean$GDP <- NULL

# We divide our predictors for student success into four categories:
#
# 1. Macroeconomic Context:
#    - UnemploymentRate
#    - InflationRate
#
# 2. Student Demographics:
#    - Is_Male
#    - AgeAtEnroll
#    - Marital_Status
#    - Displaced
#    - Is_International
#    - SpecialNeeds
#    - Parent_Higher_Edu
#    - HasScholarship
#    - Debtor
#
# 3. Academic Performance and Preparation:
#    - PriorGrade
#    - AdmissionGrade
#    - Sem1_AvgGrade
#    - Sem1_UnitsEnrolled
#    - Sem2_AvgGrade
#    - Sem2_UnitsEnrolled
#    - Application_mode
#    - Application_order
#
# 4. Program:
#    - Major
#    - Is_Daytime_Attendance
#

################################################################################
# STEP 2: Data Partition
################################################################################

# Create the partition index 90% train, 10% validation
set.seed(123) 
idx <- createDataPartition(clean$Target, times = 1, p = 0.9, list = FALSE)
train_data <- clean[idx, ]      # 90% for model training/cross-validation
final_holdout <- clean[-idx, ] # 10% for final model evaluation

# Ensure validation data has only seen categories (by "Major", etc.)
temp <- final_holdout

validation <- temp %>%
  semi_join(train_data, by = "Major") %>%
  semi_join(train_data, by = "Application_mode") %>%
  semi_join(train_data, by = "Marital_Status") %>%
  semi_join(train_data, by = "Is_Daytime_Attendance") %>%
  semi_join(train_data, by = "Parent_Higher_Edu") %>%
  semi_join(train_data, by = "SpecialNeeds") %>%
  semi_join(train_data, by = "Is_International")

removed <- anti_join(temp, validation)

train_data <- rbind(train_data, removed)

# Clean up
rm(temp, removed, final_holdout)

################################################################################
# STEP 3: Exploratory Data Analysis
################################################################################

################################################################################
# TABLE 1: Dataset Summary — Demographic overview
################################################################################

# 3.1. Missing Data Check -----------------------------------------------------
anyNA(train_data) # there is no NA

# 3.2. Macroeconomic Context --------------------------------------------------

# Ensure InflationRate is numeric
train_data$InflationRate <- as.numeric(train_data$InflationRate)

# Barplot: Frequency of each unique Inflation Rate value
inflation_counts <- train_data %>%
  count(InflationRate)

ggplot(inflation_counts, aes(x = as.factor(InflationRate), y = n)) +
  geom_bar(stat = "identity", fill = "#b3cde3", color = "white", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  labs(
    title = "Counts by Unique Inflation Rate",
    x = "Inflation Rate (%)",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) 

# variable is not smoothly continuous; 
# it’s a set of discrete steps (likely one value per cohort/year).

# Stacked bar: Target status breakdown by Inflation Rate
inf_target <- train_data %>%
  group_by(InflationRate, Target) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(InflationRate) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup()

ggplot(inf_target, aes(x = factor(InflationRate), y = perc, fill = Target)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  geom_text(
    aes(label = ifelse(perc > 6, paste0(round(perc, 1), "%"), "")),
    position = position_stack(vjust = 0.5), size = 3
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Target Status Breakdown by Inflation Rate",
    x = "Inflation Rate (%)",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 13)

# Stacked bar: Target status by Unemployment Rate
unemp_target <- train_data %>%
  group_by(UnemploymentRate, Target) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(UnemploymentRate) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup()

ggplot(unemp_target, aes(x = factor(UnemploymentRate), y = perc, fill = Target)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(
    aes(label = ifelse(perc > 6, paste0(round(perc, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Target Status Breakdown by Unemployment Rate",
    x = "Unemployment Rate (%)",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 13)

# 3.3. Student Demographics ---------------------------------------------------

# Quick summary table for binary demographic variables
counts <- c(
  Total = nrow(train_data),
  Male = sum(train_data$Is_Male == 1),
  Displaced = sum(train_data$Displaced == 1),
  International = sum(train_data$Is_International == 1),
  SpecialNeeds = sum(train_data$SpecialNeeds == 1),
  ParentCollege = sum(train_data$Parent_Higher_Edu == 1),
  HasScholarship = sum(train_data$HasScholarship == 1),
  Debtor = sum(train_data$Debtor == 1)
)

# Calculate percentages (except Total)
percentages <- round(100 * counts / counts["Total"], 1)

# Combine into a two-row table
summary_table <- rbind(Count = counts, Percentage = percentages)

# Print
print(summary_table)

# Faceted barplots: Each binary variable, breakdown by Target
facet_vars <- c("Is_Male", "Displaced", "Is_International",
                "SpecialNeeds", "Parent_Higher_Edu", "HasScholarship", "Debtor")

# Prepare long data
facet_data <- train_data %>%
  select(Target, all_of(facet_vars)) %>%
  pivot_longer(-Target, names_to = "Variable", values_to = "Value") %>%
  mutate(Value = ifelse(Value == 1, "Yes", "No"))

# Summarize and calculate percentages
facet_summary <- facet_data %>%
  group_by(Variable, Value, Target) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Variable, Value) %>%
  mutate(percentage = 100 * n / sum(n)) %>%
  ungroup()

# Plot faceted barplots
ggplot(facet_summary, aes(x = Value, y = percentage, fill = Target)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = round(percentage, 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 2.8
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  facet_wrap(~Variable, scales = "free_x") +
  labs(
    title = "Percentage of Each Target Status by Student Demographic Variables",
    x = "",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

# Age at Enrollment: Distribution by Target
ggplot(train_data, aes(x = AgeAtEnroll, fill = Target)) +
  geom_density(alpha = 0.6) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Distribution of Age at Enrollment by Target Status",
    x = "Age at Enrollment",
    y = "Density"
  ) +
  theme_minimal()

# Marital Status: Proportion by Target
marital_prop <- train_data %>%
  group_by(Marital_Status, Target) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Marital_Status) %>%
  mutate(prop = n / sum(n),
         perc = round(100 * prop, 1),
         ypos = cumsum(prop) - 0.5 * prop)  # position for labels

ggplot(marital_prop, aes(x = Marital_Status, y = prop, fill = Target)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = ifelse(perc > 2, paste0(perc, "%"), "")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Proportion of Target Status by Marital Status",
    x = "Marital Status",
    y = "Percentage"
  ) +
  theme_minimal()

# 3.4. Academic Performance and Preparation -----------------------------------

# PriorGrade: Density by Target
ggplot(train_data, aes(x = PriorGrade, fill = Target)) +
  geom_density(alpha = 0.4) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Prior Grade Density by Target Status",
    x = "Prior Grade",
    y = "Density"
  ) +
  theme_minimal()

# AdmissionGrade: Density by Target
ggplot(train_data, aes(x = AdmissionGrade, fill = Target)) +
  geom_density(alpha = 0.4) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Admission Grade Density by Target Status",
    x = "Admission Grade",
    y = "Density"
  ) +
  theme_minimal()

# 1st Semester: Units Enrolled vs Average Grade, by Target
summary(train_data$Sem1_AvgGrade) # different grad sys than the us

ggplot(train_data, aes(x = Sem1_UnitsEnrolled, y = Sem1_AvgGrade, color = Target)) +
  geom_point(alpha = 0.2, size = 1.2) +
  geom_smooth(method = "loess", se = FALSE, size = 1.1) +
  scale_color_brewer(palette = "Pastel1") +
  labs(
    title = "Trend: 1st Sem Avg Grade vs. Units Enrolled by Target",
    x = "1st Semester Units Enrolled",
    y = "1st Semester Average Grade"
  ) +
  theme_minimal()

# 2nd Semester: Units Enrolled vs Average Grade, by Target
summary(train_data$Sem2_AvgGrade) # Check summary stats (grade system may differ)

# Scatter plot with trend line
ggplot(train_data, aes(x = Sem2_UnitsEnrolled, y = Sem2_AvgGrade, color = Target)) +
  geom_point(alpha = 0.2, size = 1.2) +
  geom_smooth(method = "loess", se = FALSE, size = 1.1) +
  scale_color_brewer(palette = "Pastel1") +
  labs(
    title = "Trend: 2nd Sem Avg Grade vs. Units Enrolled by Target",
    x = "2nd Semester Units Enrolled",
    y = "2nd Semester Average Grade"
  ) +
  theme_minimal()

# Application Mode (% by Target)
# Summarize percentages for each Target within Application Mode
appmode_plot_data <- train_data %>%
  group_by(Application_mode, Target) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Application_mode) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup()

# Stacked bar plot
ggplot(appmode_plot_data, aes(x = Application_mode, y = perc, fill = Target)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(
    aes(label = ifelse(perc > 5, paste0(round(perc, 1), "%"), "")),
    position = position_stack(vjust = 0.5), size = 3
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Target Result Breakdown by Application Mode",
    x = "Application Mode",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1, size = 10),
    legend.position = "bottom"
  )

# Application Order: Dropout/On-track rates by order (with plot)
rate_df <- train_data %>%
  group_by(Application_order) %>%
  summarise(
    dropout = mean(Target == "dropout") * 100,
    on_track = mean(Target == "on_track") * 100
  )

# Convert to long format for plotting
rate_long <- rate_df %>%
  pivot_longer(
    cols = c(dropout, on_track),
    names_to = "Outcome",
    values_to = "Percent"
  )

# Assign pastel colors
pastel_colors <- c("dropout" = "#FBB4AE",   # Pastel1[1]: light blue
                   "on_track" = "#B3CDE3")  # Pastel1[2]: light pink

# Plot 
ggplot(rate_long, aes(x = Application_order, y = Percent, color = Outcome, group = Outcome)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_text(
    aes(label = round(Percent, 1)),
    vjust = -0.8, fontface = "bold", size = 3.5, show.legend = FALSE
  ) +
  scale_color_manual(
    values = pastel_colors,
    labels = c("Dropout Rate", "On-track Rate")
  ) +
  scale_x_continuous(breaks = 0:9) +
  labs(
    title = "On-track and Dropout Rates by Application Order",
    x = "Application Order (0 = First Choice)",
    y = "Rate (%)",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# 3.5. Program Variables ------------------------------------------------------

# Major: Target breakdown by major (ordered by on-track %)
# Calculate percentages by major and target
major_target <- train_data %>%
  group_by(Major, Target) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Major) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup()

# Find graduation rate for each major and reorder
on_track_order <- major_target %>%
  filter(Target == "on_track") %>%
  arrange(desc(perc)) %>%
  pull(Major)

major_target$Major <- factor(major_target$Major, levels = on_track_order)

# Plot
ggplot(major_target, aes(y = Major, x = perc, fill = Target)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(
    aes(label = ifelse(perc > 7, paste0(round(perc, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 2.8
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Target Status Breakdown by Major (Ordered by Graduation Rate)",
    y = "Major (Ordered by On-track Rate)",
    x = "Percentage"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 10))

# Daytime/Evening Attendance: Target breakdown

# Prepare data: percentages for each target within each attendance type
attendance_target <- train_data %>%
  group_by(Is_Daytime_Attendance, Target) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Is_Daytime_Attendance) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup()

# Convert binary variable to a readable label
attendance_target$AttendanceType <- ifelse(attendance_target$Is_Daytime_Attendance == 1, "Daytime", "Evening")

ggplot(attendance_target, aes(x = AttendanceType, y = perc, fill = Target)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(
    aes(label = ifelse(perc > 7, paste0(round(perc, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Target Status Breakdown by Attendance Type",
    x = "Attendance Type",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 13)

################################################################################
# STEP 4: Modeling
################################################################################

# Train-Test Split
set.seed(123) 
idx <- createDataPartition(train_data$Target, times = 1, p = 0.9, list = FALSE)
train_data_train <- train_data[idx, ]
train_data_val <- train_data[-idx, ]

# Random Forest
rf_fit <- train(
  Target ~ .,
  data = train_data_train,
  method = "rf",           
  ntree = 500,             
  trControl = trainControl(method = "cv") # "cv" for cross-validation
)

rf_pred <- predict(rf_fit, newdata = train_data_val)
rf_conf <- confusionMatrix(rf_pred, train_data_val$Target)
rf_accuracy <- rf_conf$overall["Accuracy"]
print(rf_accuracy)
print(rf_conf$table)  

# KNN
knn_fit <- train(Target ~ ., data = train_data_train, method = "knn", tuneLength = 5)
knn_pred <- predict(knn_fit, newdata = train_data_val)
knn_conf <- confusionMatrix(knn_pred, train_data_val$Target)
knn_accuracy <- knn_conf$overall["Accuracy"]
print(knn_accuracy)
print(knn_conf$table)

# Naive Bayes
nb_fit <- train(Target ~ ., data = train_data_train, method = "naive_bayes")
nb_pred <- predict(nb_fit, newdata = train_data_val)
nb_conf <- confusionMatrix(nb_pred, train_data_val$Target)
nb_accuracy <- nb_conf$overall["Accuracy"]
print(nb_accuracy)
print(nb_conf$table)

# Gradient Boosted Trees (xgboost)
gbm_fit <- train(
  Target ~ .,
  data = train_data_train,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5), # 5-fold cross-validation
  tuneLength = 3
)

gbm_pred <- predict(gbm_fit, newdata = train_data_val)
gbm_conf <- confusionMatrix(gbm_pred, train_data_val$Target)
gbm_accuracy <- gbm_conf$overall["Accuracy"]
print(gbm_accuracy)
print(gbm_conf$table)

# Ensemble (Majority Vote)
ensemble1 <- cbind(rf = rf_pred == "on_track", 
                   knn = knn_pred == "on_track", 
                  nb  = nb_pred == "on_track",
                  gbm = gbm_pred == "on_track")

ensemble_preds1 <- ifelse(rowMeans(ensemble1) > 0.5, "on_track", "dropout")
ensemble_accuracy1 <- mean(ensemble_preds1 == train_data_val$Target)

# Ensemble for the 2nd try: omit KNN model
ensemble2 <- cbind(rf = rf_pred == "on_track", 
                   nb  = nb_pred == "on_track",
                   gbm = gbm_pred == "on_track")

ensemble_preds2 <- ifelse(rowMeans(ensemble2) > 0.5, "on_track", "dropout")
ensemble_accuracy2 <- mean(ensemble_preds2 == train_data_val$Target)

model_accuracies <- data.frame(
  Model    = c("Random Forest", "KNN", "Naive Bayes", "Gradient Boosted Trees", "Ensemble (4 models)", "Ensemble (3 models)"),
  Accuracy = c(rf_accuracy, knn_accuracy, nb_accuracy, gbm_accuracy, ensemble_accuracy1, ensemble_accuracy2)
)

print(model_accuracies)

################################################################################
# STEP 5: Final Validation
################################################################################

# Predict on validation set
rf_pred_val <- predict(rf_fit, newdata = validation)
knn_pred_val <- predict(knn_fit, newdata = validation)
nb_pred_val <- predict(nb_fit, newdata = validation)
gbm_pred_val <- predict(gbm_fit, newdata = validation)

# Two ways of Ensemble
ensemble1 <- cbind(
  rf  = rf_pred_val == "on_track",
  knn = knn_pred_val == "on_track",
  nb  = nb_pred_val == "on_track",
  gbm = gbm_pred_val == "on_track"
)

ensemble_preds1 <- ifelse(rowMeans(ensemble1) > 0.5, "on_track", "dropout")
ensemble_accuracy1 <- mean(ensemble_preds1 == validation$Target)

ensemble2 <- cbind(
  rf  = rf_pred_val == "on_track",
  nb  = nb_pred_val == "on_track",
  gbm = gbm_pred_val == "on_track"
)

ensemble_preds2 <- ifelse(rowMeans(ensemble2) > 0.5, "on_track", "dropout")
ensemble_accuracy2 <- mean(ensemble_preds2 == validation$Target)

# Accuracies for individual models
rf_acc <- mean(rf_pred_val == validation$Target)
knn_acc <- mean(knn_pred_val == validation$Target)
nb_acc <- mean(nb_pred_val == validation$Target)
gbm_acc <- mean(gbm_pred_val == validation$Target)

# Make accuracy table
model_accuracies <- data.frame(
  Model    = c("Random Forest", "KNN", "Naive Bayes", "Gradient Boosted Trees", "Ensemble (4 models)", "Ensemble (3 models)"),
  Accuracy = c(rf_acc, knn_acc, nb_acc, gbm_acc, ensemble_accuracy1, ensemble_accuracy2)
)

print(model_accuracies)

# Show the confusion matrix for ensemble method 2
ensemble_preds2 <- factor(ensemble_preds2, levels = levels(validation$Target))

ensemble_confmat <- confusionMatrix(ensemble_preds2, validation$Target)
print(ensemble_confmat$table)

# validation set is not large enough 
# target variable (Target) is imbalanced
# There are more “on_track” than “dropout” cases, 
# which may influence the model to favor the majority class.

### Conclusion
# There are more “on_track” than “dropout” cases, which may influence the model to favor the majority class.
