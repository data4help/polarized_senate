

# Preliminaries ----
library("readxl")
library(plyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(caret)
library(maps)
library(mapproj)
library(rstatix)
library(RColorBrewer)

main_path = "/Users/paulmora/Documents/projects/polarized_senate"
raw_path = paste(main_path, "/00 Raw", sep="")
code_path = paste(main_path, "/01 Code", sep="")
data_path = paste(main_path, "/02 Data", sep="")
output_path = paste(main_path, "/03 Output", sep="")

# Importing data
senate_data = read_excel(paste(data_path, "/senate_data.xlsx", sep=""))
state_abb = read_excel(paste(data_path, "/state_abb_dict.xlsx", sep=""))
majority_congress = read_excel(paste(raw_path, "/majority_data.xlsx", sep=""))


# Cleaning ----

"
We start by splitting the data which is currently only in the first column
into different columns for each information. We split by having a empty
spaces.
"

separated_data = senate_data %>%
  separate(info, into=c("name", "rest"), sep=" \\(") %>%
  separate(rest, into=c("party_state", "vote"), sep="\\) ") %>%
  separate(party_state, into=c("party", "state"), sep="-") %>%
  drop_na()

"
We now add the according year to the senate number. We do that by creating
a list starting at 1989 - which is the year the database starts from - and
assign consecutively every congress a year number of time.
"

congresses = sort(unique(separated_data$congress))
start_year = 1989
end_year = start_year + length(congresses) - 1
years = seq(start_year, end_year)
separated_data$year = mapvalues(separated_data$congress,
                                from=congresses,
                                to=years)
separated_data$year = as.numeric(separated_data$year)

"
Now we create a column which contains the full name of a state and not only
a two letter abbreviation. For that we use a separate excel sheet which
contains that match of variables. In the end we delete all rows which
do not contain any state indication.
"

separated_data$region = mapvalues(separated_data$state,
                                  from=state_abb$state_abb,
                                  to=tolower(state_abb$state_name))

"
Some votes are not answered by a simple 'Yea' or 'Nay', but by 'Guilty' or
'Not Guilty'. In order to make those questions comparable as well, we
translate the latter to the former
"

separated_data$vote = mapvalues(separated_data$vote,
                                from=c("Guilty", "Not Guilty"),
                                to=c("Yea", "Nay"))

"
To get the data into a more workable format we bring the data from a long
format into a wider format. Instead
"

vote_summary = separated_data %>%
  group_by(congress, bill_num, party, vote) %>%
  tally()
vote_summary$party_vote = paste(vote_summary$party, vote_summary$vote, sep="_")
vote_wide = vote_summary %>% ungroup () %>%
  select(party_vote, n, congress, bill_num) %>%
  spread(party_vote, n) %>%
  replace(., is.na(.), 0)

"
Now we calculate the the relative amount of people by congress, bill and party
who voted yes out of all attending people. From that we can calculate
polarization as well as indicate what the majority for every single vote was.
"

parties_abb = c("D", "R")
voting_types = c("Yea", "Nay", "Not Voting")
for (party in parties_abb) {
  total_sum = rowSums(vote_wide[, paste(party, voting_types, sep="_")])
  vote_wide[, paste(party, "seats", sep="_")] = total_sum
  rel_yes = vote_wide[, paste(party, "Yea", sep="_")] / total_sum
  vote_wide[, paste(party, "rel_yes", sep="_")] = rel_yes
  vote_wide[, paste(party, "majority", sep="_")] = ifelse(rel_yes>0.5,
                                                          "Yea", "Nay")
}

vote_wide$polarization = abs(vote_wide$D_rel_yes - vote_wide$R_rel_yes)
relevant_data = vote_wide %>% ungroup() %>%
  select(bill_num, congress, D_majority, R_majority, polarization)

majority_data = merge(separated_data, relevant_data,
                      all.x=TRUE, by=c("congress","bill_num"))

"
Now we see how often the senators voted in favor of what the majority said
"

majority_data$voted_w_majority = ifelse(majority_data$party=="D",
       ifelse(majority_data$vote==majority_data$D_majority, 1, 0),
       ifelse(majority_data$vote==majority_data$R_majority, 1, 0))

# Individual cleaning
bool_rhode = str_detect(majority_data$region, "rhode")
majority_data[bool_rhode, "region"] = "rhode island"


"
When somebody does not vote we do not include the observation in our dataset.
Reason for that are examples like Kirk 2012 in Illinois who suffered a stroke
and was unable to attend for the entire year. If we would include him in the
dataset, Illinois would display a high rate of not algining with the party,
whereas actually the Senator was straight up not able to give his vote.
In order to prevent these biases we delete the rows. The deletion does
not weigh very massively, given that we have in a dataset of approx 1mio rows
30.000 which indicate not voting (3%).

Furthermore we are only considering the Democratic and Republican party given
that the majorities of the independent parties is too small to consider.
"

majority_data_clean = majority_data %>% filter(vote!="Not Voting")
majority_data_clean = majority_data_clean %>% filter(party %in% parties_abb)


# Party loyalty summary statistics ----

"
We stary by averaging the loyalty of all senators over the entire time.
Furthermore we draw a regression line in order to show how the general
trend is. Additionally we print the regression results.
"

time_series_majority = majority_data_clean %>%
  group_by(year) %>%
  summarize(mean_majority_vote = mean(voted_w_majority, na.rm=TRUE))

ggplot(time_series_majority, aes(x=year, y=mean_majority_vote, group=1)) +
  geom_point(colour="#7846B4") +
  geom_line() +
  geom_smooth(method='lm', formula=y~x, color="#82B446") +
  ylab("Average Party Loyalty") + xlab("Years") +
  theme_tufte() +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=45, hjust=1)) +
  ggsave(paste(output_path, "regression_line.png", sep="/"))
lmodel = lm(mean_majority_vote ~ year, data=time_series_majority)
summary(lmodel)

"
Now we would be interested to see in which states the average party loyalty
was especially high
"

region_year_majority = majority_data_clean %>%
  group_by(region, year) %>%
  summarize(mean_majority_vote = mean(voted_w_majority, na.rm=TRUE)) %>%
  arrange(desc(mean_majority_vote))
region_year_majority$region = str_to_title(region_year_majority$region)

num_states = length(unique(region_year_majority$region))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
ggplot(region_year_majority, aes(x=region, y=mean_majority_vote,
                                group=region, fill=region)) +
  ylab("Yearly averaged party loyalty") + xlab("States") +
  theme_tufte() +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=90, hjust=1),
        legend.position="none") +
  geom_boxplot(alpha=0.3) +
  scale_fill_manual(values = getPalette(num_states)) +
  ggsave(paste(output_path, "state_boxplots.png", sep="/"),
         width=60, height=20, units="cm")

"
Now we show the average loyalty for each year for both parties for each
state. For better illustration we therefore plot a heatmap.
"

aligned_voting_data = majority_data_clean %>%
  group_by(region, year) %>%
  summarize(mean_majority_vote = mean(voted_w_majority, na.rm=TRUE))

states = map_data("state")
mapping_data = merge(states, aligned_voting_data, by="region", all.x=TRUE)

min_value = min(aligned_voting_data$mean_majority_vote)
max_value = max(aligned_voting_data$mean_majority_vote)
for (year_num in years) {
  year_data = mapping_data %>% filter(year==year_num)
  year_data = year_data[order(year_data$order),]
  ggplot(year_data, aes(x=long, y=lat, group=group)) +
    geom_polygon(aes(fill=mean_majority_vote)) +
    geom_path()+
    theme_tufte() +
    scale_fill_gradient(name="% who voted in line",
                        limits=c(min_value, max_value), low="cornsilk",
                        high="black",
                        guide=guide_colourbar(direction="horizontal",
                                              barheight=unit(10, units="mm"),
                                              barwidth=unit(100, units="mm"),
                                              draw.ulim=F,
                                              title.hjust=0.5,
                                              label.hjust=0.5,
                                              title.position="top")) +
    coord_map() +
    ggtitle(paste("Average Party Loyalty - All Senators in", year_num)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.title=element_text(size=35),
          legend.text=element_text(size=25),
          legend.position="bottom",
          plot.title = element_text(lineheight=.8, face="bold", size=45)) +
    ggsave(paste(output_path, "/maps/both/", year_num, ".png", sep=""))
}


# Party loyalty by state and party ----

"
Now the same is happening but for each party individually. Since we would
like to show countries which do not have a Senator in a certain state as
missing we start by creating a dataframe with all potential possibilities
"

region = unique(majority_data_clean$region)
total_data = expand.grid(region, parties_abb, years, stringsAsFactors=FALSE)
colnames(total_data) = c("region", "party", "year")

aligned_party_data = majority_data_clean %>%
  group_by(region, year, party) %>%
  summarize(mean_majority_vote = mean(voted_w_majority, na.rm=TRUE))


full_aligned_party_data = merge(total_data, aligned_party_data,
                                by=c("region", "year", "party"),
                                all.x=TRUE)

"
Now we plot
"

min_value = min(aligned_party_data$mean_majority_vote)
max_value = max(aligned_party_data$mean_majority_vote)
type_list = c("D"="blue", "R"="red")
naming_list = c("D"="Democratic Party", "R"="Republican Party")
for (party_ind in names(type_list)) {

  aligned_state_data = merge(states,
                             full_aligned_party_data,
                             by="region", all.x=T)
  mapping_party = aligned_state_data %>% filter(party==party_ind)

  for (year_num in years) {
    year_data = mapping_party %>% filter(year==year_num)
    year_data = year_data[order(year_data$order),]
    ggplot(year_data, aes(x=long, y=lat, group=group)) +
      geom_polygon(aes(fill=mean_majority_vote)) +
      geom_path()+
      theme_tufte() +
      scale_fill_gradient(name="% who voted in line",
                          limits=c(min_value, max_value), low="white",
                          high=type_list[party_ind], na.value="grey50",
                          guide=guide_colourbar(direction="horizontal",
                                                barheight=unit(10, units="mm"),
                                                barwidth=unit(100, units="mm"),
                                                draw.ulim=F,
                                                title.hjust=0.5,
                                                label.hjust=0.5,
                                                title.position="top")) +
      coord_map() +
      ggtitle(paste("Average Party Loyalty -", naming_list[party_ind],
                    "in", year_num)) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.title=element_text(size=35),
            legend.text=element_text(size=25),
            legend.position="bottom",
            plot.title = element_text(lineheight=.8, face="bold", size=45)) +
      ggsave(paste(output_path, "/maps/", party_ind, "/",
                   year_num, ".png", sep=""))
  }
}


# Party loyalty as a means of staying on-board ----

"
In order to assess better the correlation between term length and average
loyalty we divide Senators into 5 buckets with different loyalties.
Afterwards we draw boxplots of term length of these senators.

Though we delete everybody who has a term length shorter than 6 years, given
that we do not have enough loyalty information of the party.
"

majority_data_clean$name_region = paste(majority_data_clean$name,
                                        majority_data_clean$region)

mean_loyalty_person_year = majority_data_clean %>%
  group_by(name_region, year) %>%
  summarize(mean_loyalty_year = mean(voted_w_majority, na.rm=TRUE))

count_w_more_6 = mean_loyalty_person_year %>%
  group_by(name_region) %>%
  tally() %>%
  filter(n >= 6)

mean_loyalty_person = mean_loyalty_person_year %>%
  filter(name_region %in% count_w_more_6$name_region) %>%
  group_by(name_region) %>%
  summarize(mean_loyalty = mean(mean_loyalty_year, na.rm=TRUE))
mean_loyalty_person$serving_years = count_w_more_6$n

"
Now the plotting with an additional ANOVA test to check for difference in groups
"
quantile(quintile_data$mean_loyalty, c(0.2, 0.4, 0.6, 0.8))
quintile_data = mean_loyalty_person %>%
  mutate(loyalty_quintile = ntile(mean_loyalty, 5))

ggplot(quintile_data, aes(x=loyalty_quintile, y=serving_years,
                          group=loyalty_quintile)) +
  ylab("Years served") + xlab("Loyalty Quintiles") +
  theme_tufte() +
  theme(text=element_text(size=30),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),) +
  geom_boxplot(color="black", fill="black", alpha=0.2) +
  ggsave(paste(output_path, "loyalty_years.png", sep="/"))

res_aov = quintile_data %>%
  anova_test(serving_years ~ as.factor(loyalty_quintile))
res_hsd = quintile_data %>%
  tukey_hsd(serving_years ~ as.factor(loyalty_quintile))


# Looking at the history of some giants ----

"
Now we pick two example of two long-sitting Senators to see how their loyalty
developed. Furthermore we show in the background whether the Senate or the
house was in power.
"

from_names = c("Graham south carolina", "Schumer new york")
to_names = c("Lindsey Graham", "Chuck Schumer")

example_data = mean_loyalty_person_year %>%
  filter(name_region %in% from_names)

example_data$name = mapvalues(example_data$name_region,
                              from=from_names,
                              to=to_names)

example_data = merge(example_data, majority_congress %>%
                                    filter(type=="Senate") %>%
                                    select(year, majority),
                     all.x=TRUE, by="year")

ggplot(example_data, aes(x=year, y=mean_loyalty_year,
                         group=name)) +
  geom_point() +
  geom_line() +
  geom_tile(aes(fill=majority),
            width=1, height=Inf, alpha=0.2) +
  scale_fill_manual(values=c(Democratic="#0015BC", Republican="#FF0000")) +
  ylab("Average Party Loyalty") + xlab("Years") +
  facet_grid(.~ name) +
  theme_tufte() +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=45, hjust=1)) +
  ggsave(paste(output_path, "examples_names.png", sep="/"))







