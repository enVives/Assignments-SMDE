library(ggplot2)
library(tidyr)
library(dplyr)

# First Question

## Task: Import data set to R assigning the type of each variable correctly.

data <- read.csv("bodyPerformance.csv")
str(data)
print(summary(data))


## Task: Summarize the numerical variables and analyze their distributions.
summary(data)


# We generate the distribution plots and save them as png's.

data_num <- data %>% select(where(is.numeric))
data_num1 <- data_num %>% select(1:2)
data_num2 <- data_num %>% select(3:4)
data_num3 <- data_num %>% select(5:6)
data_num4 <- data_num %>% select(7:8)
data_num5 <- data_num %>% select(9:10)

#Manually change data_num
long_data <- data_num5 %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value")

plot <- ggplot(long_data, aes(x = value)) +
  geom_histogram(bins = 30,
                 fill = "steelblue",
                 color = "white") +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    strip.text = element_text(size = 20, face = "bold")
  ) +
  labs(
    x = "Value",
    y = "Frequency")

#Manually change the name
ggsave("distributions5.png",
       plot = plot,
       width = 16,
       height = 8,
       units = "in",
       dpi = 120)

# We also generate the boxplots to analyze the dispersion of the data

data_num <- data %>%
  select(where(is.numeric))

long_data <- data_num5 %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  )

plot <- ggplot(long_data, aes(x = "", y = value)) +
  geom_boxplot(fill = "steelblue",
               color = "black",
               outlier.color = "red") +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  labs(
    x = "",
    y = "Value")

ggsave("boxplot5.png",
       plot = plot,
       width = 6,
       height = 4,
       units = "in",
       dpi = 120)


## Task: Consider the variables of diastolic and systolic blood pressure. Eliminate the zero values and check their distributions for the gender groups using proper graph(s). Interpret your findings.

print("Number of zero values in systolic:")
print(sum(data$systolic == 0, na.rm = TRUE))
print("Number of zero values in diastolic:")
print(sum(data$diastolic == 0, na.rm = TRUE))

data <- data[data$systolic != 0,]
data <- data[data$diastolic != 0,]
print("Number of zero values in systolic after elimination:")
print(sum(data$systolic == 0, na.rm = TRUE))
print("Number of zero values in diastolic after elimination:")
print(sum(data$diastolic == 0, na.rm = TRUE))

# Systolic and dialotic distributions by gender plots

plot <- ggplot(data,aes(x = gender, y = systolic, fill = gender)) +
  geom_boxplot(outlier.color = "red")+
  scale_fill_manual(values = c(
    "M" = "steelblue",
    "F" = "salmon"
  )) +
  labs(
    title = "Systolic Blood Pressure by Gender",
    x = "Gender",
    y = "Systolic Blood Pressure"
  ) +
  theme(legend.position = "none")

ggsave("boxplot6.png",
       plot = plot,
       width = 6,
       height = 4,
       units = "in",
       dpi = 120)

plot <- ggplot(data,aes(x = gender, y = diastolic, fill = gender)) +
  geom_boxplot(outlier.color = "red")+
  scale_fill_manual(values = c(
    "M" = "steelblue",
    "F" = "salmon"
  )) +
  labs(
    title = "Diastolic Blood Pressure by Gender",
    x = "Gender",
    y = "Diastolic Blood Pressure"
  ) +
  theme(legend.position = "none")

ggsave("boxplot7.png",
       plot = plot,
       width = 6,
       height = 4,
       units = "in",
       dpi = 120)


## Task: Construct 95% confidence intervals for the mean systolic blood pressure of two gender groups and interpret them.


#Gender plot

gender <- data.frame(gender = data$gender)

plot <- ggplot(gender, aes(x = gender, fill = gender)) +
  geom_bar(show.legend = FALSE) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = 1.5,
    color = "white",
    size = 5
  ) +
  scale_fill_manual(values = c(
    "M" = "steelblue",
    "F" = "salmon"
  )) +
  labs(
    title = "Gender Distribution",
    x = "Gender",
    y = "Frequency",
    fill = "Gender"
  ) +
  theme_minimal()

ggsave("gender_dist.png",
       plot = plot,
       width = 3,
       height = 4,
       units = "in",
       dpi = 120)


# Creation of subsets

male <- subset(data, data$gender == "M")
female <- subset(data, data$gender == "F")

#Confidence intervals fos systolic
mu <- mean(female$systolic, na.rm = TRUE)
print(mu)
std <- sd(female$systolic, na.rm = TRUE)
print(std)
n <- sum(complete.cases(female$systolic))
print(n)
print(round(mu + qt(c(0.025, 0.975), n - 1) * std / sqrt(n), 2))

#Confidence intervals for diastolic
mu <- mean(male$systolic, na.rm = TRUE)
print(mu)
std <- sd(male$systolic, na.rm = TRUE)
print(std)
n <- sum(complete.cases(male$systolic))
print(n)
print(round(mu + qt(c(0.025, 0.975), n - 1) * std / sqrt(n), 2))

#Confidence intervals using t-test
print("95% CI for female systolic blood pressure:")
print(t.test(male$systolic, level = 0.95))
print("95% CI for male systolic blood pressure:")
print(t.test(female$systolic, level = 0.95))

# Task: Categorize the variable age into four categories cutting from the values 30,40 and 50 and save it as a new variable into your data frame.

data$categorized_age <- cut(data$age, breaks = c(0,30,40,50,Inf),labels = c("<30","30-40","40-50",">50"),right = FALSE)

# Plot cor categorized_age distribution

categorized_age <- data.frame(categorized_age = data$categorized_age)

plot <- ggplot(categorized_age, aes(x = categorized_age, fill = categorized_age)) +
  geom_bar(show.legend = FALSE) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = 1.5,
    color = "white",
    size = 5
  ) +
  labs(
    title = "Categorized age distribution",
    x = "Ages",
    y = "Frequency",
    fill = "Ages"
  ) +
  theme_minimal()

ggsave("categorized_age_dist.png",
       plot = plot,
       width = 4,
       height = 4,
       units = "in")
       
## Task: Analyze the association first between age groups and class and then between age groups and gender by using proper statistical method.
       

age_class_table <- table(data$categorized_age,data$class)
age_gender_table <- table(data$categorized_age,data$gender)

print(chisq.test(age_class_table))
print(chisq.test(age_gender_table))

## Task: Test whether physical performance test results of the individuals are affected from the gender. Check the assumptions and visualize the relationship between these two characteristics.

# Normality test

print("Shapiro-Wilk test: gripForce - Male")
print(shapiro.test(sample(data$gripForce[data$gender == "M"], 5000)))
print("Shapiro-Wilk test: gripForce - Female")
print(shapiro.test(data$gripForce[data$gender == "F"]))
print("Shapiro-Wilk test: sit.and.bend.forward_cm - Male")
print(shapiro.test(sample(data$sit.and.bend.forward_cm[data$gender == "M"], 5000)))
print("Shapiro-Wilk test: sit.and.bend.forward_cm - Female")
print(shapiro.test(data$sit.and.bend.forward_cm[data$gender == "F"]))
print("Shapiro-Wilk test: sit.ups.counts - Male")
print(shapiro.test(sample(data$sit.ups.counts[data$gender == "M"], 5000)))
print("Shapiro-Wilk test: sit.ups.counts - Female")
print(shapiro.test(data$sit.ups.counts[data$gender == "F"]))
print("Shapiro-Wilk test: broad.jump_cm - Male")
print(shapiro.test(sample(data$broad.jump_cm[data$gender == "M"], 5000)))
print("Shapiro-Wilk test: broad.jump_cm - Female")
print(shapiro.test(data$broad.jump_cm[data$gender == "F"]))

# Homogeneity test

print("Variance test: gripForce")
print(var.test(data$gripForce ~ data$gender, data = data))
print("Variance test: sit.and.bend.forward_cm")
print(var.test(data$sit.and.bend.forward_cm ~ data$gender, data = data))
print("Variance test: sit.ups.counts")
print(var.test(data$sit.ups.counts ~ data$gender, data = data))
print("Variance test: broad.jump_cm")
print(var.test(data$broad.jump_cm ~ data$gender, data = data))

# T-test
print("T-test: gripForce")
print(t.test(data$gripForce ~ data$gender, data = data, var.equal = TRUE))
print("T-test: sit.and.bend.forward_cm")
print(t.test(data$sit.and.bend.forward_cm ~ data$gender, data = data, var.equal = TRUE))
print("T-test: sit.ups.counts")
print(t.test(data$sit.ups.counts ~ data$gender, data = data, var.equal = TRUE))
print("T-test: broad.jump_cm")
print(t.test(data$broad.jump_cm ~ data$gender, data = data, var.equal = TRUE))

# Distributions of the physical performance test by gender

plot <- ggplot(data, aes(x = gender, y = gripForce,
                         fill = gender)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", show.legend = FALSE) +
  scale_fill_manual(values = c(
    "M" = "steelblue",
    "F" = "salmon"
  )) +
  labs(
    title = "Grip force by Gender",
    x = "Gender",
    y = "Grip force"
  ) +
  theme_minimal()

ggsave("boxplot8.png",
       plot = plot,
       width = 6,
       height = 4,
       units = "in",
       dpi = 120)

plot <- ggplot(data, aes(x = gender, y = sit.and.bend.forward_cm,
                         fill = gender)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", show.legend = FALSE) +
  scale_fill_manual(values = c(
    "M" = "steelblue",
    "F" = "salmon"
  )) +
  labs(
    title = "Sit and bend forward by Gender",
    x = "Gender",
    y = "Sit and bend forward"
  ) +
  theme_minimal()

ggsave("boxplot9.png",
       plot = plot,
       width = 6,
       height = 4,
       units = "in",
       dpi = 120)

plot <- ggplot(data, aes(x = gender, y = sit.ups.counts,
                         fill = gender)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", show.legend = FALSE) +
  scale_fill_manual(values = c(
    "M" = "steelblue",
    "F" = "salmon"
  )) +
  labs(
    title = "Sit up counts by Gender",
    x = "Gender",
    y = "Sit up counts"
  ) +
  theme_minimal()

ggsave("boxplot10.png",
       plot = plot,
       width = 6,
       height = 4,
       units = "in",
       dpi = 120)

plot <- ggplot(data, aes(x = gender, y = broad.jump_cm,
                         fill = gender)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", show.legend = FALSE) +
  scale_fill_manual(values = c(
    "M" = "steelblue",
    "F" = "salmon"
  )) +
  labs(
    title = "Broad jump by Gender",
    x = "Gender",
    y = "Broad jump"
  ) +
  theme_minimal()

ggsave("boxplot11.png",
       plot = plot,
       width = 6,
       height = 4,
       units = "in",
       dpi = 120)




       
