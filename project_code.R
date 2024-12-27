library(tidyverse) # allows to utilize ggplot
library(plotly)    # allows to make plots interactive
library(rmarkdown) # allows to utilize paged_table() fucntion
ecec = read.csv('ecec.csv') # reading the dataset

# converting "YES"/"NO" to binary values (1/0) for calculating proportions later
ecec <- ecec %>%
  mutate(
    Q3_1 = ifelse(Q3_1 == "YES", 1, 0),
    Q3_2 = ifelse(Q3_2 == "YES", 1, 0),
    Q4 = ifelse(Q4 == "Yes", "Workforce Affected", "Workforce Not Affected")
  )


# pivoting longer to have accessibility/affordability issues in one column
ecec_long <- ecec %>%
  select(Q3_1, Q3_2, Q4) %>%
  pivot_longer(cols = c(Q3_1, Q3_2), 
               names_to = "Issue", 
               values_to = "Reported") %>%
  filter(Reported == 1)  # filtering rows where issues were reported


# renaming the issues for the x-axis
ecec_long <- ecec_long %>%
  mutate(Issue = recode(Issue, 
                        "Q3_1" = "Access Issues", 
                        "Q3_2" = "Affordability Issues"))


# creating a stacked barplot
plot <- ggplot(ecec_long, aes(x = Issue, fill = Q4)) +
  geom_bar(position = "fill", color = 'black') +
  scale_fill_manual(values = c("Workforce Affected" = "#d6b4fc", "Workforce Not Affected" = "#800080")) +
  labs(
    title = "Impact of Accessibility and Affordability Issues on Workforce Participation",
    y = "Proportion that faced an issue",
    fill = "Workforce Participation"
  ) +
  theme_light()


# making the plot interactive
ggplotly(plot)

# filtering out mothers (SQ1 == "Female") and "Not working"
mothers_not_working <- ecec %>%
  filter(SQ1 == "Female" & D1 == "Not working (e.g., students OR home duties)")


# selecting columns for reasons of not working (D8_1 to D8_11) and pivot longer for visualization
mothers_reasons <- mothers_not_working %>%
  select(SQ1, D8_1:D8_11) %>%  # select all D8 columns
  pivot_longer(cols = starts_with("D8"), names_to = "Reason", values_to = "Response") %>%
  filter(Response == "YES")  # keep rows where the response is "YES"


# creating a mapping of column names to reasons for readability
mothers_reasons <- mothers_reasons %>%
  mutate(Reason = recode(Reason,
                         "D8_1" = "Prefer to care for child(ren) personally",
                         "D8_2" = "Financially not worthwhile",
                         "D8_3" = "Own illness or disability",
                         "D8_4" = "Caring for elderly relatives",
                         "D8_5" = "Prefer not to work",
                         "D8_6" = "Studying",
                         "D8_7" = "Other personal or family responsibilities",
                         "D8_8" = "Involved in voluntary work",
                         "D8_9" = "Too difficult finding employment",
                         "D8_10" = "Other",
                         "D8_11" = "Prefer not to answer"))


# count the number of mothers for each reason
count_reasons <- mothers_reasons %>%
  group_by(Reason) %>%
  summarise(count = n())


# creating bar plot
ggplot(count_reasons, aes(x = reorder(Reason, count), y = count, fill = Reason)) +
  geom_bar(stat = "identity", show.legend = F, color = 'black') +
  coord_flip() +  # flipping for better readability
  labs(title = "Reasons for Not Working Among Mothers", 
       y = "Number of Mothers", x = "Reason for Not Working") +
  theme_minimal()


# cleaning the Q4 column to ensure no extra spaces and uniform case
ecec_clean <- ecec %>%
  mutate(Q4 = str_trim(Q4))


# creating new income bins with $50,000 intervals and filtering out 'Prefer not to answer'
income_bins <- ecec_clean %>%
  filter(D13 != "Prefer not to answer") %>%
  mutate(income_group = case_when(
    D13 %in% c("$1 - $19,999 per year ($1 - $379 per week)", 
               "$20,000 - $29,999 per year ($380 - $579 per week)", 
               "$30,000 - $39,999 per year ($580 - $769 per week)", 
               "$40,000 - $49,999 per year ($770 - $959 per week)") ~ "$1 - $49,999",
    D13 %in% c("$50,000 - $59,999 per year ($960 - $1149 per week)", 
               "$60,000 - $79,999 per year ($1150 - $1529 per week)", 
               "$80,000 - $99,999 per year ($1530 - $1919 per week)") ~ "$50,000 - $99,999",
    D13 %in% c("$100,000 - $124,999 per year ($1920 - $2399 per week)", 
               "$125,000 - $149,999 per year ($2400 - $2879 per week)") ~ "$100,000 - $149,999",
    D13 %in% c("$150,000 - $199,999 per year ($2880 - $3839 per week)") ~ "$150,000 - $199,999",
    D13 %in% c("$200,000 - $249,999 per year ($3840 - $4799 per week)", 
               "$250,000 - $299,999 per year ($4800 - $5759 per week)") ~ "$200,000 - $299,999",
    D13 == "$300,000 or more per year ($5760 or more per week)" ~ "$300,000 or more"
  ))


# calculating percentage of people who responded "Workforce Affected"
income_affordability <- income_bins %>%
  group_by(income_group) %>%
  summarise(
    proportion_affordability = paste0(round((sum(Q3_2 == 1, na.rm = TRUE) / n()) * 100, 2), "%"),
    proportion_workforce = paste0(round((sum(Q4 == "Workforce Affected", na.rm = TRUE) / n()) * 100, 2), "%")
  )


# removing NA rows and rearrange the $50,000 - $99,999 row
income_affordability <- income_affordability %>%
  filter(!is.na(income_group)) %>%
  arrange(factor(income_group, levels = c("$1 - $49,999", "$50,000 - $99,999", 
                                          "$100,000 - $149,999", "$150,000 - $199,999", 
                                          "$200,000 - $299,999", "$300,000 or more")))


# renaming columns
colnames(income_affordability) <- c("Income Group", "Percentage Facing Affordability Issues", "Percentage Impact on Workforce Participation")


# creating a paged table with only the relevant columns
paged_table(income_affordability)
