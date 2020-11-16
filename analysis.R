# analyzing election data

# set up
raw_df <- read.csv("https://raw.githubusercontent.com/alex/nyt-2020-election-scraper/master/all-state-changes.csv")
library(tidyverse)

# clean up states
df <- raw_df %>% 
  separate(col = state, into = c("state", "ev"), sep = " \\(") %>% 
  mutate(ev = parse_number(ev)) %>% 
  mutate(biden_votes = if_else(leading_candidate_name == "Biden",
                               leading_candidate_votes,
                               trailing_candidate_votes),
         trump_votes = total_votes_count - biden_votes)

# df exploration
num_cols <- ncol(df)
num_rows <- nrow(df)
num_states <- length(unique(df$state))

# when did Biden take the lead in Georgia?
ga_lead_time <- df %>% 
  filter(state == "Georgia", leading_candidate_name == "Biden") %>% 
  filter(timestamp == min(timestamp)) %>% 
  pull(timestamp)

# when did Biden take the lead in each state?
biden_lead_time <- df %>% 
  filter(leading_candidate_name == "Biden")

# what is the difference in votes in each state?
vote_diff <- df %>% 
  group_by(state) %>% 
  filter(timestamp == max(timestamp)) %>% 
  mutate(diff = biden_votes - trump_votes,
         pct_diff = diff/total_votes_count)

vote_diff_plot <- ggplot(vote_diff)+
  geom_col(mapping = aes(x = diff, y = reorder(state, diff)))

vote_pct_plot <- ggplot(vote_diff)+
  geom_col(mapping = aes(x = pct_diff, y = reorder(state, pct_diff), fill = leading_candidate_name))+
  scale_fill_manual(values = c("blue", "red"))+
  labs(fill = "Leading Candidate")
# How do total votes change over time?