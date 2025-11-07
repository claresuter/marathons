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
# load data from pope/wu marathon project
################################################################################################

data = read.csv(file.path(data_path, "full_split_sample.csv"))

# how many are at baseline missing the half split -- none. this may be by construction
sum(is.na(data$split_half))

# calculate difference between first and last halves of marathon
data = data %>%
  mutate(time_second_half = chiptime - split_half,
         diff_halves = time_second_half - split_half)

# colors for gender
gender_cols =  c(
  "F" = "#ef8a62",
  "M"   = "#998ec3"
)

# kernel density of finish times, different color for each gender
finish_time_by_gender = data %>%
  filter(!is.na(chiptime), gender %in% c("M", "F")) %>%
  mutate(time_full_hours = chiptime / 60) %>%
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
ggsave(file.path(pdf_path, "finish_by_gender_all.pdf"), finish_time_by_gender, width = 5, height = 3.5, device = cairo_pdf)

# kernel density of first and second half split diffs
split_diff_by_gender = data %>%
  filter(!is.na(diff_halves), gender %in% c("M", "F")) %>%
  ggplot(aes(x = diff_halves, color = gender, fill = gender)) +
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
  coord_cartesian(xlim = c(-45, 90)) +
  theme_minimal(base_size = 12)
ggsave(file.path(pdf_path, "split_diff_by_gender_all.pdf"), split_diff_by_gender, width = 5, height = 3.5, device = cairo_pdf)

# percent difference from first split of second split, by gender
data = data %>%
  mutate(pct_diff = (diff_halves / split_half) * 100)

split_pct_diff_by_gender = data %>%
  filter(!is.na(pct_diff), gender %in% c("M", "F")) %>%
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
  coord_cartesian(xlim = c(-30, 60)) +
  theme_minimal(base_size = 12)
ggsave(file.path(pdf_path, "split_pct_diff_by_gender_all.pdf"), split_pct_diff_by_gender, width = 5, height = 3.5, device = cairo_pdf)

# share of men vs women who have a second half split that's 10% slower than the first half
nrow(data[data$gender == "M" & data$pct_diff >= 10,])/nrow(data[data$gender == "M",])
nrow(data[data$gender == "F" & data$pct_diff >= 10,])/nrow(data[data$gender == "F",])

# compare 1st and 4th 10k split
data = data %>%
  mutate(split_10k_4 = split_40k - split_30k)

# percent difference of 4th 10k split relative to first 10k split
data = data %>%
  mutate(pct_diff_10k = ((split_10k_4 - split_10k) / split_10k) * 100)

split_pct_diff_by_gender_10k = data %>%
  filter(!is.na(pct_diff), gender %in% c("M", "F")) %>%
  ggplot(aes(x = pct_diff_10k, color = gender, fill = gender)) +
  scale_fill_manual(values = gender_cols) +
  scale_color_manual(values = gender_cols) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  geom_density(alpha = 0.15, adjust = 1) +
  labs(
    title = "How much slower is the 4th 10k split (relative to 1st)",
    x = "Percent Split Diff (%)",
    y = "Density",
    color = "Gender",
    fill = "Gender"
  ) +
  coord_cartesian(xlim = c(-30, 100)) +
  theme_minimal(base_size = 12)
ggsave(file.path(pdf_path, "split_pct_diff_by_gender_all_10k.pdf"), split_pct_diff_by_gender_10k, width = 5, height = 3.5, device = cairo_pdf)


################################################################################################
# filter to the chicago marathon?
################################################################################################
# 255k obs when we filter to chi
chi = data %>%
  filter(marathon == "Chicago Marathon")

chi_split_pct_diff_by_gender = chi %>%
  filter(!is.na(pct_diff), gender %in% c("M", "F")) %>%
  ggplot(aes(x = pct_diff, color = gender, fill = gender)) +
  scale_fill_manual(values = gender_cols) +
  scale_color_manual(values = gender_cols) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  geom_density(alpha = 0.15, adjust = 1) +
  labs(
    title = "Chicago: Split Distribution by Gender",
    x = "Percent Split Diff (Second Half - First Half) (%)",
    y = "Density",
    color = "Gender",
    fill = "Gender"
  ) +
  coord_cartesian(xlim = c(-30, 60)) +
  theme_minimal(base_size = 12)
ggsave(file.path(pdf_path, "split_pct_diff_by_gender_chi.pdf"), chi_split_pct_diff_by_gender, width = 5, height = 3.5, device = cairo_pdf)

# share of men vs women who have a second half split that's 10% slower than the first half
nrow(chi[chi$gender == "M" & chi$pct_diff >= 20,])/nrow(chi[chi$gender == "M",])
nrow(chi[chi$gender == "F" & chi$pct_diff >= 20,])/nrow(chi[chi$gender == "F",])

split_pct_diff_by_gender_10k_chi = chi %>%
  filter(!is.na(pct_diff), gender %in% c("M", "F")) %>%
  ggplot(aes(x = pct_diff_10k, color = gender, fill = gender)) +
  scale_fill_manual(values = gender_cols) +
  scale_color_manual(values = gender_cols) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  geom_density(alpha = 0.15, adjust = 1) +
  labs(
    title = "How much slower is the 4th 10k split (relative to 1st)",
    x = "Percent Split Diff (%)",
    y = "Density",
    color = "Gender",
    fill = "Gender"
  ) +
  coord_cartesian(xlim = c(-30, 100)) +
  theme_minimal(base_size = 12)
ggsave(file.path(pdf_path, "split_pct_diff_by_gender_chi_10k.pdf"), split_pct_diff_by_gender_10k_chi, width = 5, height = 3.5, device = cairo_pdf)


nrow(chi[chi$gender == "M" & chi$pct_diff_10k >= 30,])/nrow(chi[chi$gender == "M",])
nrow(chi[chi$gender == "F" & chi$pct_diff_10k >= 30,])/nrow(chi[chi$gender == "F",])

# what does this look like in a reg
summary(lm(pct_diff ~ gender + chiptime, data = data %>% filter(gender %in% c("M", "F"))))

data = data %>% mutate(blowup = ifelse(pct_diff >= 25, 1, 0))
summary(lm(blowup ~ gender + chiptime + chiptime*gender, data = data %>% filter(gender %in% c("M", "F"))))

