################################################################################################
# gendered marathon strategies
################################################################################################

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(readr)
library(kableExtra)
library(lubridate)
library(entropy)

current_user = Sys.info()[["user"]]

if (current_user == "claresuter") {
  data_path = "/Users/claresuter/Documents/harvard/marathons/data"
  pdf_path = "/Users/claresuter/Documents/harvard/marathons/figs"
}

################################################################################################
# load chicago 2000 data (https://github.com/AndrewMillerOnline/marathon-results/tree/main)
################################################################################################
# one year of chicago -- 27,911 runners
data = read.csv(file.path(data_path, "results-2000.csv"))

# how many are at baseline missing --0
sum(is.na(data$time_half))

# convert times from character to numeric (seconds)
data = data %>% 
  mutate(time_full_sec = as.numeric(hms(time_full)),
         time_half_sec = as.numeric(hms(time_half)))
# update missing -- 120 now
sum(is.na(data$time_half_sec))

# calculate difference between first and last halves of marathon
data = data %>%
  mutate(time_second_half = time_full_sec - time_half_sec,
         diff_halves = time_second_half - time_half_sec)

# colors for gender
gender_cols =  c(
  "Female" = "#ef8a62",
  "Male"   = "#998ec3"
)

# kernel density of finish times, different color for each gender
finish_time_by_gender = data %>%
  filter(!is.na(time_full), !is.na(gender)) %>%
  mutate(time_full_hours = time_full_sec / 3600) %>%
  ggplot(aes(x = time_full_hours, color = gender, fill = gender)) +
  scale_fill_manual(values = gender_cols) +
  scale_color_manual(values = gender_cols) +
  geom_density(alpha = 0.2, adjust = 1) +
  labs(
    title = "Finish Time Distribution by Gender",
    x = "Finish time (hours)",
    y = "Density",
    color = "Gender",
    fill  = "Gender"
  ) +
  coord_cartesian(xlim = c(2, 7)) +
  theme_minimal(base_size = 12)
# save for overleaf
ggsave(file.path(pdf_path, "finish_by_gender.pdf"), finish_time_by_gender, width = 5, height = 3.5, device = cairo_pdf)

# kernel density of first and second half split diffs
split_diff_by_gender = data %>%
  filter(!is.na(diff_halves), !is.na(gender)) %>%
  mutate(diff_halves_min = diff_halves / 60) %>%
  ggplot(aes(x = diff_halves_min, color = gender, fill = gender)) +
  scale_fill_manual(values = gender_cols) +
  scale_color_manual(values = gender_cols) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  geom_density(alpha = 0.15, adjust = 1) +
  labs(
    title = "Split Distribution by Gender",
    x = "Split Diff (Second Half - First Half) (minutes)",
    y = "Density",
    color = "Gender",
    fill  = "Gender"
  ) +
  coord_cartesian(xlim = c(-45, 45)) +
  theme_minimal(base_size = 12)
ggsave(file.path(pdf_path, "split_diff_by_gender.pdf"), split_diff_by_gender, width = 5, height = 3.5, device = cairo_pdf)

# percent difference from first split of second split, by gender
data = data %>%
  mutate(pct_diff = (diff_halves / time_half_sec) * 100)

split_pct_diff_by_gender = data %>%
  filter(!is.na(pct_diff), !is.na(gender)) %>%
  ggplot(aes(x = pct_diff, color = gender, fill = gender)) +
  scale_fill_manual(values = gender_cols) +
  scale_color_manual(values = gender_cols) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  geom_density(alpha = 0.15, adjust = 1) +
  labs(
    title = "Split Distribution by Gender",
    x = "Percent Split Diff (Second Half - First Half) (%)",
    y = "Density",
    color = "Gender",
    fill = "Gender"
  ) +
  coord_cartesian(xlim = c(-30, 30)) +
  theme_minimal(base_size = 12)
ggsave(file.path(pdf_path, "split_pct_diff_by_gender.pdf"), split_pct_diff_by_gender, width = 5, height = 3.5, device = cairo_pdf)

### KL divergence
# Suppose your data frame has time_full_hours and gender
male_times = data %>%
  filter(gender == "Male", !is.na(pct_diff)) %>%
  pull(pct_diff)

female_times <- data %>%
  filter(gender == "Female", !is.na(pct_diff)) %>%
  pull(pct_diff)

# 1. Estimate densities on same grid
breaks <- seq(-100, 390, by = 0.1)
male_hist <- hist(male_times, breaks = breaks, plot = FALSE)
female_hist <- hist(female_times, breaks = breaks, plot = FALSE)

# 2. Convert to probabilities
p <- male_hist$density / sum(male_hist$density)
q <- female_hist$density / sum(female_hist$density)

# 3. Compute KL divergence (P || Q)
ks.test(male_times, female_times)

# share of men vs women who have a second half split that's 10% slower than the first half
length(male_times[male_times >=10])/length(male_times)
length(female_times[female_times>=10])/length(female_times)

# 20%?
length(male_times[male_times >=20])/length(male_times)
length(female_times[female_times>=20])/length(female_times)



