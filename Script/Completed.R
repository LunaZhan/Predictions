
library(openintro)
library(tidyverse)
library(flextable)
library(Hmisc)
?yrbss

view(yrbss)

# and all category labels start with a capital letter

yrbss <- yrbss %>% 
  rename_with(str_to_title)

yrbss$Grade[yrbss$Grade == "other"] <- "Other"
yrbss <- yrbss |> mutate(
  Grade = factor(Grade,  levels = c("9", "10", "11", "12", "Other")),
  Gender = str_to_title(Gender), 
  BMI = Weight/(Height^2)
)

label(yrbss$Physically_active_7d) <- "# days physically active per week"


## # Modify the code below such that the grade shows in increasing order
z <- summarizor(
  yrbss[c("Grade", "Gender")],
  overall_label = NULL
)
ft_1 <- as_flextable(z) 
ft_1


# To understand the pattern of physical activity by grade and gender,
# 1) aggregate  `physically_active_7d` by calculating its mean within each grade and gender
# 2) create a plot showing the average number of physically active days
#      x-axis: grade
#      y-axis: Mean of `physcially_active_7d`
#      Distinguish gender using different colors, symbols, or lines
# *** I would use the following functions: aggregate(), ggplot(), geom_line() but there is 
# no one correct way to do this
# Ensure that the figure is clearly labeled and includes an appropriate legend


yrbss |> 
  group_by(Grade, Gender) |> 
  summarise(mean_physical = mean(Physically_active_7d, na.rm = T))


yrbss |> 
  ggplot(aes(y = Physically_active_7d, x  = Grade, fill = Gender)) + 
  geom_point() + 
  geom_violin(trim = FALSE, drop = F) +
  theme_minimal()

aggregate(Physically_active_7d ~ Grade + Gender, data = yrbss, FUN = mean) |> 
  ggplot(aes(y = Physically_active_7d, x  = Grade, color = Gender, group = Gender)) + 
  geom_line() +
  theme_minimal() + 
  labs( y = "# days physically active per week", 
title = str_wrap("Physical Activity within YRBSS by Grade and Gender"))


# Create a plot that shows the relationship betwen physical activity and bmi
# among female students in grade 12 
# Ensure that the figure is clearly labeled and includes an appropriate legend

dat_fem12 <- yrbss |> filter(Grade == "12" & Gender == "Female")
# 1. Calculate the correlation coefficient value manually
r_value <- round(cor(dat_fem12$Physically_active_7d, dat_fem12$BMI,  use = "pairwise.complete.obs", method = "pearson"), 2)

# 2. Build the plot
ggplot(dat_fem12, aes(x = Physically_active_7d, y = BMI)) +
  geom_point(color = "black", size = 1) +            # Add scatter points
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Add linear trendline
  theme_minimal() +
  labs(title = str_wrap("Association Between Physical Activity and BMI of Grade 12 Females within YRBSS"))
