
############################################################
# DATA PREPARATION
############################################################

library(ggplot2)
library(tidyr)
library(dplyr)
library(car)
library(agricolae)

# Import dataset
data <- read.csv("bodyPerformance.csv")

# Create age categories
data$age_cat <- cut(
  data$age,
  breaks = c(0, 30, 40, 50, Inf),
  labels = c("<30", "30-39", "40-49", "50+"),
  right = FALSE
)

# Convert categorical variables to factors
data$class <- factor(
  data$class,
  levels = c("A", "B", "C", "D")
)

data$gender <- factor(data$gender)

# Check data structure
str(data)


############################################################
# QUESTION 2(a): DISTRIBUTION ANALYSIS
############################################################

# Prepare data for distribution analysis
q2_data <- data %>%
  select(
    age_cat, class, body.fat_.,
    diastolic, sit.ups.counts, broad.jump_cm
  ) %>%
  pivot_longer(
    cols = -c(age_cat, class),
    names_to = "variable",
    values_to = "value"
  )

print(head(q2_data))


# ----------------------------------------------------------
# Distribution by age group - Histograms
# ----------------------------------------------------------

plot_hist_age <- ggplot(q2_data, aes(x = value)) +
  geom_histogram(
    bins = 25,
    fill = "steelblue",
    color = "white"
  ) +
  facet_wrap(
    ~ variable + age_cat,
    scales = "free",
    ncol = 4
  ) +
  theme_minimal() +
  labs(
    title = "Distribution of variables by age group",
    x = "Value",
    y = "Frequency"
  )

print(plot_hist_age)


# ----------------------------------------------------------
# Distribution by age group - Boxplots
# ----------------------------------------------------------

plot_box_age <- ggplot(
  q2_data,
  aes(x = age_cat, y = value)
) +
  geom_boxplot(
    fill = "steelblue",
    outlier.color = "red"
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    ncol = 2
  ) +
  theme_minimal() +
  labs(
    title = "Variables by age group",
    x = "Age group",
    y = "Value"
  )

print(plot_box_age)


# ----------------------------------------------------------
# Distribution by performance class - Histograms
# ----------------------------------------------------------

plot_hist_class <- ggplot(
  q2_data,
  aes(x = value)
) +
  geom_histogram(
    bins = 25,
    fill = "steelblue",
    color = "white"
  ) +
  facet_wrap(
    ~ variable + class,
    scales = "free",
    ncol = 4
  ) +
  theme_minimal() +
  labs(
    title = "Distribution of variables by performance class",
    x = "Value",
    y = "Frequency"
  )

print(plot_hist_class)


# ----------------------------------------------------------
# Distribution by performance class - Boxplots
# ----------------------------------------------------------

plot_box_class <- ggplot(
  q2_data,
  aes(x = class, y = value)
) +
  geom_boxplot(
    fill = "steelblue",
    outlier.color = "red"
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    ncol = 2
  ) +
  theme_minimal() +
  labs(
    title = "Variables by performance class",
    x = "Performance class",
    y = "Value"
  )

print(plot_box_class)


# ----------------------------------------------------------
# Descriptive statistics by age group
# ----------------------------------------------------------

stats_age <- q2_data %>%
  group_by(variable, age_cat) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = round(mean(value, na.rm = TRUE), 2),
    median = round(median(value, na.rm = TRUE), 2),
    sd = round(sd(value, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(stats_age, n = Inf)


# ----------------------------------------------------------
# Descriptive statistics by performance class
# ----------------------------------------------------------

stats_class <- q2_data %>%
  group_by(variable, class) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = round(mean(value, na.rm = TRUE), 2),
    median = round(median(value, na.rm = TRUE), 2),
    sd = round(sd(value, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(stats_class, n = Inf)



############################################################
# QUESTION 2(b): ONE-WAY ANOVA
############################################################


############################################################
# 1. DIASTOLIC BLOOD PRESSURE BY AGE GROUP
############################################################

# Remove zero blood pressure values
diastolic_data <- data %>%
  filter(diastolic > 0, !is.na(age_cat))

# Initial one-way ANOVA
model_diastolic_age <- aov(
  diastolic ~ age_cat,
  data = diastolic_data
)

print(summary(model_diastolic_age))

# Homogeneity of variances
print(
  leveneTest(
    diastolic ~ age_cat,
    data = diastolic_data
  )
)

# Normality of residuals
res <- residuals(model_diastolic_age)

set.seed(123)
print(shapiro.test(sample(res, 5000)))

qqnorm(
  res,
  main = "Q-Q Plot: Diastolic BP by Age"
)
qqline(res, col = "red", lwd = 2)


# ----------------------------------------------------------
# Examine extreme observations
# ----------------------------------------------------------

print(summary(diastolic_data$diastolic))

# Lowest observations
print(
  diastolic_data %>%
    arrange(diastolic) %>%
    select(age, gender, diastolic, age_cat) %>%
    head(10)
)

# Highest observations
print(
  diastolic_data %>%
    arrange(desc(diastolic)) %>%
    select(age, gender, diastolic, age_cat) %>%
    head(10)
)


# ----------------------------------------------------------
# Remove clearly implausible observations
# ----------------------------------------------------------

diastolic_clean <- diastolic_data %>%
  filter(!diastolic %in% c(6, 8))

# Number of removed observations
print(nrow(diastolic_data) - nrow(diastolic_clean))

# Refit ANOVA
model_diastolic_age_clean <- aov(
  diastolic ~ age_cat,
  data = diastolic_clean
)

print(summary(model_diastolic_age_clean))

# Homogeneity
print(
  leveneTest(
    diastolic ~ age_cat,
    data = diastolic_clean
  )
)

# Normality
res_clean <- residuals(model_diastolic_age_clean)

set.seed(123)
print(shapiro.test(sample(res_clean, 5000)))

qqnorm(
  res_clean,
  main = "Q-Q Plot: Cleaned Diastolic BP"
)
qqline(res_clean, col = "red", lwd = 2)


# ----------------------------------------------------------
# Post hoc comparisons
# ----------------------------------------------------------

# Tukey HSD
tukey_diastolic_age <- TukeyHSD(
  model_diastolic_age_clean,
  "age_cat"
)

print(tukey_diastolic_age)

# Bonferroni
bonferroni_diastolic_age <- with(
  diastolic_clean,
  pairwise.t.test(
    diastolic,
    age_cat,
    p.adjust.method = "bonferroni"
  )
)

print(bonferroni_diastolic_age)

# Fisher's LSD
lsd_diastolic_age <- LSD.test(
  model_diastolic_age_clean,
  "age_cat",
  p.adj = "none",
  group = FALSE
)

print(lsd_diastolic_age$comparison)



############################################################
# 2. DIASTOLIC BLOOD PRESSURE BY PERFORMANCE CLASS
############################################################

model_diastolic_class <- aov(
  diastolic ~ class,
  data = diastolic_clean
)

print(summary(model_diastolic_class))

# Homogeneity
print(
  leveneTest(
    diastolic ~ class,
    data = diastolic_clean
  )
)

# Normality
res_class <- residuals(model_diastolic_class)

set.seed(123)
print(shapiro.test(sample(res_class, 5000)))

qqnorm(
  res_class,
  main = "Q-Q Plot: Diastolic BP by Class"
)
qqline(res_class, col = "red", lwd = 2)


# ----------------------------------------------------------
# Post hoc comparisons
# ----------------------------------------------------------

# Tukey HSD
tukey_diastolic_class <- TukeyHSD(
  model_diastolic_class,
  "class"
)

print(tukey_diastolic_class)

# Bonferroni
bonferroni_diastolic_class <- with(
  diastolic_clean,
  pairwise.t.test(
    diastolic,
    class,
    p.adjust.method = "bonferroni"
  )
)

print(bonferroni_diastolic_class)

# Fisher's LSD
lsd_diastolic_class <- LSD.test(
  model_diastolic_class,
  "class",
  p.adj = "none",
  group = FALSE
)

print(lsd_diastolic_class$comparison)



############################################################
# 3. BODY FAT (%) BY AGE GROUP
############################################################

bodyfat_data <- data %>%
  filter(!is.na(body.fat_.), !is.na(age_cat))

# One-way ANOVA
model_bodyfat_age <- aov(
  body.fat_. ~ age_cat,
  data = bodyfat_data
)

print(summary(model_bodyfat_age))

# Homogeneity
print(
  leveneTest(
    body.fat_. ~ age_cat,
    data = bodyfat_data
  )
)

# Normality
res_bodyfat_age <- residuals(model_bodyfat_age)

set.seed(123)
print(shapiro.test(sample(res_bodyfat_age, 5000)))

# Q-Q plots by age group
plot_qq_bodyfat_age <- ggplot(
  bodyfat_data,
  aes(sample = body.fat_.)
) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(
    ~ age_cat,
    scales = "free"
  ) +
  theme_minimal() +
  labs(
    title = "Q-Q Plots: Body Fat by Age Group",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

print(plot_qq_bodyfat_age)


# ----------------------------------------------------------
# Post hoc comparisons
# ----------------------------------------------------------

# Tukey HSD
tukey_bodyfat_age <- TukeyHSD(
  model_bodyfat_age,
  "age_cat"
)

print(tukey_bodyfat_age)

# Bonferroni
bonferroni_bodyfat_age <- with(
  bodyfat_data,
  pairwise.t.test(
    body.fat_.,
    age_cat,
    p.adjust.method = "bonferroni"
  )
)

print(bonferroni_bodyfat_age)

# Fisher's LSD
lsd_bodyfat_age <- LSD.test(
  model_bodyfat_age,
  "age_cat",
  p.adj = "none",
  group = FALSE
)

print(lsd_bodyfat_age$comparison)



############################################################
# 4. BODY FAT (%) BY PERFORMANCE CLASS
############################################################

model_bodyfat_class <- aov(
  body.fat_. ~ class,
  data = bodyfat_data
)

print(summary(model_bodyfat_class))

# Homogeneity
print(
  leveneTest(
    body.fat_. ~ class,
    data = bodyfat_data
  )
)

# Normality
res_bodyfat_class <- residuals(model_bodyfat_class)

set.seed(123)
print(shapiro.test(sample(res_bodyfat_class, 5000)))

qqnorm(
  res_bodyfat_class,
  main = "Q-Q Plot: Body Fat by Class"
)
qqline(res_bodyfat_class, col = "red", lwd = 2)


# ----------------------------------------------------------
# Post hoc comparisons
# ----------------------------------------------------------

# Tukey HSD
tukey_bodyfat_class <- TukeyHSD(
  model_bodyfat_class,
  "class"
)

print(tukey_bodyfat_class)

# Bonferroni
bonferroni_bodyfat_class <- with(
  bodyfat_data,
  pairwise.t.test(
    body.fat_.,
    class,
    p.adjust.method = "bonferroni"
  )
)

print(bonferroni_bodyfat_class)

# Fisher's LSD
lsd_bodyfat_class <- LSD.test(
  model_bodyfat_class,
  "class",
  p.adj = "none",
  group = FALSE
)

print(lsd_bodyfat_class$comparison)



############################################################
# 5. SIT-UPS BY AGE GROUP
############################################################

situps_data <- data %>%
  filter(!is.na(sit.ups.counts), !is.na(age_cat))

# One-way ANOVA
model_situps_age <- aov(
  sit.ups.counts ~ age_cat,
  data = situps_data
)

print(summary(model_situps_age))

# Homogeneity
print(
  leveneTest(
    sit.ups.counts ~ age_cat,
    data = situps_data
  )
)

# Normality
res_situps_age <- residuals(model_situps_age)

set.seed(123)
print(shapiro.test(sample(res_situps_age, 5000)))

qqnorm(
  res_situps_age,
  main = "Q-Q Plot: Sit-ups by Age"
)
qqline(res_situps_age, col = "red", lwd = 2)


# ----------------------------------------------------------
# Post hoc comparisons
# ----------------------------------------------------------

# Tukey HSD
tukey_situps_age <- TukeyHSD(
  model_situps_age,
  "age_cat"
)

print(tukey_situps_age)

# Bonferroni
bonferroni_situps_age <- with(
  situps_data,
  pairwise.t.test(
    sit.ups.counts,
    age_cat,
    p.adjust.method = "bonferroni"
  )
)

print(bonferroni_situps_age)

# Fisher's LSD
lsd_situps_age <- LSD.test(
  model_situps_age,
  "age_cat",
  p.adj = "none",
  group = FALSE
)

print(lsd_situps_age$comparison)



############################################################
# 6. SIT-UPS BY PERFORMANCE CLASS
############################################################

model_situps_class <- aov(
  sit.ups.counts ~ class,
  data = situps_data
)

print(summary(model_situps_class))

# Homogeneity
print(
  leveneTest(
    sit.ups.counts ~ class,
    data = situps_data
  )
)

# Normality
res_situps_class <- residuals(model_situps_class)

set.seed(123)
print(shapiro.test(sample(res_situps_class, 5000)))

qqnorm(
  res_situps_class,
  main = "Q-Q Plot: Sit-ups by Class"
)
qqline(res_situps_class, col = "red", lwd = 2)


# ----------------------------------------------------------
# Post hoc comparisons
# ----------------------------------------------------------

# Tukey HSD
tukey_situps_class <- TukeyHSD(
  model_situps_class,
  "class"
)

print(tukey_situps_class)

# Bonferroni
bonferroni_situps_class <- with(
  situps_data,
  pairwise.t.test(
    sit.ups.counts,
    class,
    p.adjust.method = "bonferroni"
  )
)

print(bonferroni_situps_class)

# Fisher's LSD
lsd_situps_class <- LSD.test(
  model_situps_class,
  "class",
  p.adj = "none",
  group = FALSE
)

print(lsd_situps_class$comparison)



############################################################
# 7. BROAD JUMP BY AGE GROUP
############################################################

jump_data <- data %>%
  filter(!is.na(broad.jump_cm), !is.na(age_cat))

# Initial one-way ANOVA
model_jump_age <- aov(
  broad.jump_cm ~ age_cat,
  data = jump_data
)

print(summary(model_jump_age))

# Homogeneity
print(
  leveneTest(
    broad.jump_cm ~ age_cat,
    data = jump_data
  )
)

# Normality
res_jump_age <- residuals(model_jump_age)

set.seed(123)
print(shapiro.test(sample(res_jump_age, 5000)))

qqnorm(
  res_jump_age,
  main = "Q-Q Plot: Broad Jump by Age"
)
qqline(res_jump_age, col = "red", lwd = 2)


# ----------------------------------------------------------
# Examine extreme observations
# ----------------------------------------------------------

print(summary(jump_data$broad.jump_cm))

# Count zero values
print(sum(jump_data$broad.jump_cm == 0, na.rm = TRUE))

# Lowest observations
print(
  jump_data %>%
    arrange(broad.jump_cm) %>%
    select(
      age, gender, broad.jump_cm,
      age_cat, class
    ) %>%
    head(15)
)


# ----------------------------------------------------------
# Sensitivity analysis: Exclude zero jump distances
# ----------------------------------------------------------

jump_clean <- jump_data %>%
  filter(broad.jump_cm > 0)

# Number of excluded observations
print(nrow(jump_data) - nrow(jump_clean))

# Refit ANOVA
model_jump_age_clean <- aov(
  broad.jump_cm ~ age_cat,
  data = jump_clean
)

print(summary(model_jump_age_clean))

# Homogeneity
print(
  leveneTest(
    broad.jump_cm ~ age_cat,
    data = jump_clean
  )
)

# Normality
res_jump_clean <- residuals(model_jump_age_clean)

set.seed(123)
print(shapiro.test(sample(res_jump_clean, 5000)))

qqnorm(
  res_jump_clean,
  main = "Q-Q Plot: Broad Jump by Age (without zeros)"
)
qqline(res_jump_clean, col = "red", lwd = 2)


# ----------------------------------------------------------
# Post hoc comparisons
# ----------------------------------------------------------

# Tukey HSD
tukey_jump_age <- TukeyHSD(
  model_jump_age_clean,
  "age_cat"
)

print(tukey_jump_age)

# Bonferroni
bonferroni_jump_age <- with(
  jump_clean,
  pairwise.t.test(
    broad.jump_cm,
    age_cat,
    p.adjust.method = "bonferroni"
  )
)

print(bonferroni_jump_age)

# Fisher's LSD
lsd_jump_age <- LSD.test(
  model_jump_age_clean,
  "age_cat",
  p.adj = "none",
  group = FALSE
)

print(lsd_jump_age$comparison)



############################################################
# 8. BROAD JUMP BY PERFORMANCE CLASS
############################################################

model_jump_class <- aov(
  broad.jump_cm ~ class,
  data = jump_clean
)

print(summary(model_jump_class))

# Homogeneity
print(
  leveneTest(
    broad.jump_cm ~ class,
    data = jump_clean
  )
)

# Normality
res_jump_class <- residuals(model_jump_class)

set.seed(123)
print(shapiro.test(sample(res_jump_class, 5000)))

qqnorm(
  res_jump_class,
  main = "Q-Q Plot: Broad Jump by Class"
)
qqline(res_jump_class, col = "red", lwd = 2)


# ----------------------------------------------------------
# Post hoc comparisons
# ----------------------------------------------------------

# Tukey HSD
tukey_jump_class <- TukeyHSD(
  model_jump_class,
  "class"
)

print(tukey_jump_class)

# Bonferroni
bonferroni_jump_class <- with(
  jump_clean,
  pairwise.t.test(
    broad.jump_cm,
    class,
    p.adjust.method = "bonferroni"
  )
)

print(bonferroni_jump_class)

# Fisher's LSD
lsd_jump_class <- LSD.test(
  model_jump_class,
  "class",
  p.adj = "none",
  group = FALSE
)

print(lsd_jump_class$comparison)



############################################################
# QUESTION 2(c): TWO-WAY ANOVA
############################################################


############################################################
# 1. SIT-UPS BY AGE GROUP AND GENDER
############################################################

# Two-way ANOVA with interaction
model_situps_two_way <- aov(
  sit.ups.counts ~ age_cat * gender,
  data = situps_data
)

print(summary(model_situps_two_way))


# ----------------------------------------------------------
# Interaction plot
# ----------------------------------------------------------

interaction.plot(
  x.factor = situps_data$age_cat,
  trace.factor = situps_data$gender,
  response = situps_data$sit.ups.counts,
  fun = mean,
  type = "b",
  col = c("blue", "red"),
  pch = c(16, 17),
  xlab = "Age group",
  ylab = "Mean number of sit-ups",
  trace.label = "Gender",
  main = "Interaction between Age and Gender on Sit-ups"
)


# ----------------------------------------------------------
# Check ANOVA assumptions
# ----------------------------------------------------------

# Normality
res_situps_two_way <- residuals(model_situps_two_way)

set.seed(123)
print(shapiro.test(sample(res_situps_two_way, 5000)))

qqnorm(
  res_situps_two_way,
  main = "Q-Q Plot: Sit-ups Two-way ANOVA"
)
qqline(res_situps_two_way, col = "red", lwd = 2)

# Homogeneity across age-gender combinations
situps_data$age_gender <- interaction(
  situps_data$age_cat,
  situps_data$gender
)

print(
  leveneTest(
    sit.ups.counts ~ age_gender,
    data = situps_data
  )
)



############################################################
# 2. BROAD JUMP BY AGE GROUP AND GENDER
############################################################

# Two-way ANOVA with interaction
model_jump_two_way <- aov(
  broad.jump_cm ~ age_cat * gender,
  data = jump_clean
)

print(summary(model_jump_two_way))


# ----------------------------------------------------------
# Interaction plot
# ----------------------------------------------------------

interaction.plot(
  x.factor = jump_clean$age_cat,
  trace.factor = jump_clean$gender,
  response = jump_clean$broad.jump_cm,
  fun = mean,
  type = "b",
  col = c("blue", "red"),
  pch = c(16, 17),
  xlab = "Age group",
  ylab = "Mean broad jump distance (cm)",
  trace.label = "Gender",
  main = "Interaction between Age and Gender on Broad Jump"
)


# ----------------------------------------------------------
# Check ANOVA assumptions
# ----------------------------------------------------------

# Normality
res_jump_two_way <- residuals(model_jump_two_way)

set.seed(123)
print(shapiro.test(sample(res_jump_two_way, 5000)))

qqnorm(
  res_jump_two_way,
  main = "Q-Q Plot: Broad Jump Two-way ANOVA"
)
qqline(res_jump_two_way, col = "red", lwd = 2)

# Homogeneity across age-gender combinations
jump_clean$age_gender <- interaction(
  jump_clean$age_cat,
  jump_clean$gender
)

print(
  leveneTest(
    broad.jump_cm ~ age_gender,
    data = jump_clean
  )
)

