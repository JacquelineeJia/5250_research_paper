#Loaddataset
ovarian <-read_xlsx(
  "1_Ovarian_Peritoneal_Fallopian_Patients__FULLYDEIDENTIFIED_DOBaltered_ovarian only_clean.xlsx",
  guess_max = 100000)

other <-read_xlsx(
  "2_Other_Cancer_Patients_FULLYDEIDENTIFIED_DOBaltered_clean.xlsx",
  guess_max = 100000)
noshow <-read_xlsx(
  "3_HCP_Not_Assessed_Patients_FULLY_DEIDENTIFIED_DOBaltered.xlsx",
  guess_max = 100000)
ethn_map <- read_xlsx("HCP Ethnicity Mapping.xlsx",guess_max = 100000)%>% 
  select(original_ethnicity, group, as_client_class)

###Step1: Ethnicity cleaning ('ovarian' and 'other' updated)
ethn_ovarian <- ovarian %>% 
  select(STUDYID2, contains(c('Ethnicity','Other'))) %>% 
  select(-contains(c("also", "Ethnicity3", "Ethnicity4"))) 
#ethn_ovarian[is.na(ethn_ovarian)] <- "0"

ethn_other <- other %>% 
  select(STUDYID, contains(c('Ethnicity','Other'))) %>% 
  select(-contains(c("also", "Ethnicity3", "Ethnicity4"))) 
#ethn_other[is.na(ethn_other)] <- "0"

# levels of patients' self-reported ethnicity
ethn_ovarian_levels <- ethn_ovarian %>% 
  mutate_if(is.character, as.factor) %>% sapply(levels)
ethn_other_levels <- ethn_other %>%
  mutate_if(is.character, as.factor) %>% sapply(levels)

# map to client provided classes
ethn_ovar_clean <- ethn_ovarian %>% select(STUDYID2)
for(i in 2:length(ethn_ovarian)){
  df <- ethn_ovarian %>%
    select(STUDYID2,colnames(ethn_ovarian)[i]) %>%
    merge(y = ethn_map,
          by.x = colnames(ethn_ovarian)[i],
          by.y = c("original_ethnicity"))
  names(df)[1] <- paste(removePunctuation(abbreviate(colnames(ethn_ovarian)[i])),
                        i-1,
                        sep="")
  names(df)[3] <- paste(removePunctuation(abbreviate(colnames(ethn_ovarian)[i])),
                        i-1,
                        "_group",
                        sep="")
  names(df)[4] <- paste(
    removePunctuation(abbreviate(colnames(ethn_ovarian)[i])),
    i-1,
    "_class", 
    sep="")
  ethn_ovar_clean <- merge(ethn_ovar_clean, df, by="STUDYID2",all = TRUE)
}

ethn_other_clean <- ethn_other %>% select(STUDYID)
for(i in 2:length(ethn_other)){
  df2 <- ethn_other %>%
    select(STUDYID,colnames(ethn_other)[i]) %>%
    merge(y = ethn_map,
          by.x = colnames(ethn_other)[i],
          by.y = c("original_ethnicity"))
  names(df2)[1] <- paste(removePunctuation(abbreviate(colnames(ethn_other)[i])),
                         i-1,
                         sep="")
  names(df2)[3] <- paste(removePunctuation(abbreviate(colnames(ethn_other)[i])),
                         i-1,
                         "_group",
                         sep="")
  names(df2)[4] <- paste(removePunctuation(abbreviate(colnames(ethn_other)[i])),
                         i-1,
                         "_class",
                         sep="")
  ethn_other_clean <- merge(ethn_other_clean, df2, by="STUDYID", all = TRUE)
}

# Define a function that returns the mode of each row
# Returns "Mixed" if there's a tie for the mode
# Returns NA if all values in the row are NA
ethnicity_each_row <- function(df) {
  modes <- apply(df, 1, function(x) {
    x <- na.omit(x)
    if (length(x) == 0) {
      return("Ancestry not reported")
    } else {
      freqs <- table(x)
      max_freq <- max(freqs)
      modes <- names(freqs)[freqs == max_freq]
      if (length(modes) == 1) {
        return(modes)
      } else {
        return("Mixed")
      }
    }
  })
  return(modes)
}

# (most voted) votes for patients' ethnicity
ethn_ovar_individual <- 
  ethn_ovar_clean %>% 
  select(contains("class"))%>% 
  ethnicity_each_row()
ethn_ovar_individual <- 
  as.data.frame(cbind(ethn_ovar_clean$STUDYID2,ethn_ovar_individual)) %>% 
  mutate_if(is.character,as.factor)
colnames(ethn_ovar_individual) <- c('STUDYID','Individual_Ethnicity')

ethn_other_individual <- 
  ethn_other_clean %>% 
  select(contains("class"))%>% 
  ethnicity_each_row()
ethn_other_individual <- 
  as.data.frame(cbind(ethn_other_clean$STUDYID,ethn_other_individual)) %>% 
  mutate_if(is.character,as.factor)
colnames(ethn_other_individual) <- c('STUDYID','Individual_Ethnicity')

# merge back to original dataset
ovarian <- ovarian %>% rename(STUDYID = STUDYID2)
ovarian <- ovarian %>% merge(ethn_ovar_individual, by = "STUDYID") %>% 
  mutate(
    consented_for_genetic_testing = if_else(
      is.na(consented_for_genetic_testing), 
      "No", 
      consented_for_genetic_testing),
    eligible_for_genetic_testing = if_else(
      is.na(eligible_for_genetic_testing), 
      "No", 
      eligible_for_genetic_testing))
other <- other %>% 
  merge(ethn_other_individual, by = "STUDYID") %>% 
  mutate(
    consented_for_genetic_testing = if_else(
      is.na(consented_for_genetic_testing),
      "No",
      consented_for_genetic_testing),
    eligible_for_genetic_testing = if_else(
      is.na(eligible_for_genetic_testing),
      "No", 
      eligible_for_genetic_testing))

# Creating a dataframe for objective 3, removing uneccesary columns
hyp3_ovarian <- ovarian %>%
  select(c("consented_for_genetic_testing",
           "Individual_Ethnicity",
           starts_with("latest_age_of_diagnosis_to nearest 5"),
           starts_with("latest_HCP_referral_date"),
           starts_with("latest_diagnosis_date"), 
           starts_with("days from referral_to_date_results_latest"),
           starts_with("date_test_results_received"),
           starts_with("days from  latest_diagnosis_date to referral"),)
  ) %>% 
  rename(
    "consent" = "consented_for_genetic_testing",
    "eth" = "Individual_Ethnicity",
    "age" = starts_with("latest_age_of_diagnosis_to nearest 5"),
    "year_referral" = starts_with("latest_HCP_referral_date"),
    "year_diagnosis" = starts_with("latest_diagnosis_date"),
    "referral_to_results" = 
      starts_with("days from referral_to_date_results_latest"),
    "year_results" = starts_with("date_test_results_received"),
    "diagnosis_to_referral" = 
      starts_with("days from  latest_diagnosis_date to referral")
  )  %>%
  mutate(cancertype='Ovarian')
hyp3_other <- other %>%
  select(c("consented_for_genetic_testing",
           "Individual_Ethnicity",
           starts_with("latest_age_of_diagnosis_to nearest 5"),
           starts_with("latest_HCP_referral_date"),
           starts_with("latest_diagnosis_date"), 
           starts_with("days from referral_to_date_results_latest"),
           starts_with("date_test_results_received"),
           starts_with("days from  latest_diagnosis_date to referral"))
  ) %>% 
  rename(
    "consent" = "consented_for_genetic_testing",
    "eth" = "Individual_Ethnicity",
    "age" = starts_with("latest_age_of_diagnosis_to nearest 5"),
    "year_referral" = starts_with("latest_HCP_referral_date"),
    "year_diagnosis" = starts_with("latest_diagnosis_date"),
    "referral_to_results" = 
      starts_with("days from referral_to_date_results_latest"),
    "year_results" = starts_with("date_test_results_received"),
    "diagnosis_to_referral" = 
      starts_with("days from  latest_diagnosis_date to referral"),
  )  %>%
  mutate(cancertype='Other')
hyp_3 <- bind_rows(hyp3_ovarian, hyp3_other)

##EDA for objective 2
ovarian <- ovarian %>%
  mutate_at(grep("^(`GCOS6|GCOS6|`MICRA)",colnames(.)),list(as.numeric)) %>%
  mutate_at(grep("^(`Mo|`Fa)",colnames(.)),list(as.character))

other <- other %>%
  mutate_at(grep("^(`GCOS6|GCOS6|`MICRA)",colnames(.)),list(as.numeric)) %>%
  mutate_at(grep("^(`Mo|`Fa)",colnames(.)),list(as.character))

#Exploring NAs
ovarian_GCOS<-
  ovarian%>%summarize(
    preGC=sum(!is.na(GCOS6_Pre_GC_Total_Computed)),
    postGC=sum(!is.na(GCOS6_Post_GC_Total_Computed)),
    postGT=sum(!is.na(GCOS6_Post_GT_Total_Computed)),
    pairs_GC=sum(
      !is.na(GCOS6_Pre_GC_Total_Computed)&!is.na(GCOS6_Post_GC_Total_Computed)
    ),
    pairs_GT=sum(
      !is.na(GCOS6_Post_GC_Total_Computed)&!is.na(GCOS6_Post_GT_Total_Computed)
    ),
  )

ovarian_MICRA <- ovarian %>% 
  summarize(
    number_MICRA1=sum(!is.na(`MICRA Table.Aggregate Score1`)),
    number_MICRA2=sum(!is.na(`MICRA Table.Aggregate Score2`)))
ovarian_GCOS
ovarian_MICRA

#other group NA

other_GCOS <- 
  other %>%summarize(
    preGC=sum(!is.na(GCOS6_Pre_GC_Total_Computed)),
    postGC=sum(!is.na(GCOS6_Post_GC_Total_Computed)),
    postGT=sum(!is.na(GCOS6_Post_GT_Total_Computed)),
    pairs_GC=sum(
      !is.na(GCOS6_Pre_GC_Total_Computed)&!is.na(GCOS6_Post_GC_Total_Computed)
    ),
    pairs_GT=sum(
      !is.na(GCOS6_Post_GC_Total_Computed)&!is.na(GCOS6_Post_GT_Total_Computed)
    )
  )

other_MICRA <- other %>%
  summarize(number_MICRA1=sum(!is.na(`MICRA Table.Aggregate Score1`)),
            number_MICRA2=sum(!is.na(`MICRA Table.Aggregate Score2`)))

other_GCOS
other_MICRA

```

```{r include=FALSE}
ova_na <- sum(is.na(ovarian))/(nrow(ovarian)*ncol(ovarian))
non_na <- sum(is.na(other))/(nrow(other)*ncol(other))

tab <- matrix(c(ova_na, non_na), ncol=2, byrow=TRUE)
colnames(tab) <- c("Ovarian", "non-Ovarian")
rownames(tab) <- "NAs"
na_table = as.table(tab)
na_table

tbl <- data.frame(Groups = c("Ovarian Cancer Group", "Non-ovarian Cancer Group"),
                  num_of_patient = c(3741, 19829),
                  preGC = c(83, 1063),
                  postGC = c(26, 732),
                  postGT = c(8, 149),
                  pairs_GC = c(26, 730),
                  pairs_GT = c(5, 118),
                  MICRA1 = c(49, 527),
                  MICRA2 = c(0, 2)
)
tbl <- t(tbl)

##adding age group and relocate columns
ovarian <- ovarian %>%
  rename("age" = "latest_age_of_diagnosis_to nearest 5")
other <- other %>%
  rename("age" = "latest_age_of_diagnosis_to nearest 5")
ovarian["age_group"] = cut(ovarian$age,
                           c(17, 29, 39, 49, 59, 69, Inf), 
                           c("18-29", "30-39","40-49", "50-59", "60-69", "70+"),
                           include.lowest = TRUE)
other["age_group"] = cut(other$age,
                         c(17, 29, 39, 49, 59, 69, Inf), 
                         c("18-29", "30-39","40-49", "50-59", "60-69", "70+"),
                         include.lowest = TRUE)
ovarian <- ovarian %>%
  relocate(age, .after=sex) %>%
  relocate(age_group, .after = age)
other <- other %>%
  relocate(age, .after=sex) %>%
  relocate(age_group, .after = age)

referral_plot <- ggplot(hyp_3, aes(year_referral)) +
  geom_bar() +
  xlab("Year of Latest Referral") +
  ylab("Count")

diagnosis_plot <- ggplot(hyp_3, aes(year_diagnosis)) +
  geom_bar() +
  xlab("Year of Latest Diagnosis") +
  ylab("Count")

diagnosis_to_referral_plot <- hyp_3 %>% 
  filter(365*3 >= diagnosis_to_referral & diagnosis_to_referral >= 0) %>% 
  ggplot(., aes(diagnosis_to_referral)) +
  geom_histogram() +
  ggtitle("Time from diagnosis to referral") +
  ylab("Count") +
  xlab("Time between 0 days and 3 years")

referral_to_results_plot <- hyp_3 %>% 
  filter(365*3 >= referral_to_results & referral_to_results >= 0) %>% 
  ggplot(., aes(referral_to_results)) +
  geom_histogram() +
  ggtitle("Time from referral to results") +
  ylab("Count") +
  xlab("Time between 0 days and 3 years")

grid.arrange(diagnosis_plot, 
             diagnosis_to_referral_plot, 
             referral_plot, 
             referral_to_results_plot,nrow=2, ncol=2)


###Hypothesis 2 ovarian:
#shrink data set for H2 only and remove NA testing consent
ovarian_H2_simple<-ovarian%>%select('STUDYID',
                                    'sex',
                                    age_group,
                                    Individual_Ethnicity,
                                    'consented_for_genetic_testing',
                                    'GCOS6_Pre_GC_Total_Computed',
                                    'GCOS6_Post_GC_Total_Computed',
                                    'GCOS6_Post_GT_Total_Computed',
                                    'MICRA Table.Aggregate Score1')
other_H2_simple<-other%>%select('STUDYID',
                                'sex',
                                age_group,
                                Individual_Ethnicity,
                                'consented_for_genetic_testing',
                                'GCOS6_Pre_GC_Total_Computed',
                                'GCOS6_Post_GC_Total_Computed',
                                'GCOS6_Post_GT_Total_Computed',
                                'MICRA Table.Aggregate Score1')

#rename the following columns
ovarian_H2_simple <- ovarian_H2_simple %>% 
  rename("pre" = "GCOS6_Pre_GC_Total_Computed",
         "postGC" = "GCOS6_Post_GC_Total_Computed",
         "postGT" = "GCOS6_Post_GT_Total_Computed",
         "micra" = "MICRA Table.Aggregate Score1")

other_H2_simple <- other_H2_simple %>% 
  rename("pre" = "GCOS6_Pre_GC_Total_Computed",
         "postGC" = "GCOS6_Post_GC_Total_Computed",
         "postGT" = "GCOS6_Post_GT_Total_Computed",
         "micra" = "MICRA Table.Aggregate Score1")

ovarian_H2_simple <- 
  ovarian_H2_simple[complete.cases(ovarian_H2_simple$consented_for_genetic_testing),]
ovarian_H2_simple$test <- 
  ifelse(ovarian_H2_simple$consented_for_genetic_testing == "Yes", 1, 0)
ovarian_H2_simple <- 
  ovarian_H2_simple %>%relocate(test, .after=age_group)
ovarian_H2_simple$consented_for_genetic_testing <- 
  ordered(ovarian_H2_simple$consented_for_genetic_testing,
          levels = c("Yes", "No"))

other_H2_simple <- 
  other_H2_simple[complete.cases(other_H2_simple$consented_for_genetic_testing),]
other_H2_simple$test <- 
  ifelse(other_H2_simple$consented_for_genetic_testing == "Yes", 1, 0)
other_H2_simple <- 
  other_H2_simple %>%relocate(test, .after=age_group)
other_H2_simple$consented_for_genetic_testing <- 
  ordered(other_H2_simple$consented_for_genetic_testing,
          levels = c("Yes", "No"))

#shrink data set H2 other
ovarian_H2_simple<-ovarian%>%select('STUDYID',
                                    'sex',
                                    age,
                                    age_group,
                                    Individual_Ethnicity,
                                    'consented_for_genetic_testing',
                                    'GCOS6_Pre_GC_Total_Computed',
                                    'GCOS6_Post_GC_Total_Computed',
                                    'GCOS6_Post_GT_Total_Computed',
                                    'MICRA Table.Aggregate Score1')
other_H2_simple<-other%>%select('STUDYID',
                                'sex',
                                age,
                                age_group,
                                Individual_Ethnicity,
                                'consented_for_genetic_testing',
                                'GCOS6_Pre_GC_Total_Computed',
                                'GCOS6_Post_GC_Total_Computed',
                                'GCOS6_Post_GT_Total_Computed',
                                'MICRA Table.Aggregate Score1')
#prepare same patient paired data for H2.1
other_GC_pair<-other_H2_simple%>%
  drop_na('GCOS6_Pre_GC_Total_Computed','GCOS6_Post_GC_Total_Computed')
other_GT_pair<-other_H2_simple%>%
  drop_na('GCOS6_Post_GC_Total_Computed','GCOS6_Post_GT_Total_Computed')
other_GC_pair_score_diff<-other_GC_pair%>%
  mutate(GC_score_diff=GCOS6_Post_GC_Total_Computed-GCOS6_Pre_GC_Total_Computed)
other_GT_pair_score_diff<-other_GT_pair%>%
  mutate(GT_score_diff=GCOS6_Post_GT_Total_Computed-GCOS6_Post_GC_Total_Computed)

#rename column titles and limit age to 18~70
ovarian_simple_renamed<-ovarian_H2_simple%>%
  rename(GCOS1='GCOS6_Pre_GC_Total_Computed',
         GCOS2='GCOS6_Post_GC_Total_Computed',
         GCOS3='GCOS6_Post_GT_Total_Computed',
         MICRA1='MICRA Table.Aggregate Score1')%>%
  filter(age>=18&age<=70)
other_simple_renamed<-other_H2_simple%>%
  rename(GCOS1='GCOS6_Pre_GC_Total_Computed',
         GCOS2='GCOS6_Post_GC_Total_Computed',
         GCOS3='GCOS6_Post_GT_Total_Computed',
         MICRA1='MICRA Table.Aggregate Score1')%>%
  filter(age>=18&age<=70)

###objective 1
library(dplyr)
ovarian_1 <- ovarian %>%
  mutate(Ovarian = "Ovarian") %>%
  select(STUDYID, consented_for_genetic_testing, Individual_Ethnicity, Ovarian)

other_1 <- other %>%
  mutate(Ovarian = "Non-Ovarian") %>%
  select(STUDYID, consented_for_genetic_testing, Individual_Ethnicity, Ovarian)

# Combine the two data frames
all_1 <- rbind(ovarian_1, other_1)

# Create the contingency table
contingency_table1 <- table(all_1$consented_for_genetic_testing, all_1$Ovarian)

print(contingency_table1)

# Perform the Chi-square test of independence
H1_1 <- chisq.test(contingency_table1, correct=FALSE)

# Output the results of the Chi-square test
H1_1

# Create the contingency table
contingency_table2 <- table(all_1$consented_for_genetic_testing, all_1$Individual_Ethnicity)

# Output the contingency table to check the counts before performing the test
print(contingency_table2)

# Perform the Chi-square test of independence
H1_2 <- chisq.test(contingency_table2, correct=FALSE)

# Output the results of the Chi-square test
H1_2

table = t(contingency_table2)
knitr::kable(
  table, 
  caption = "Hypothesis 1 - Scenario 2: Ethnicity and. Acceptance of Genetic Testing" ) %>%
  kable_styling(latex_options = "HOLD_position")

# second take: ignoring the "Ancestry not reported"
all_1_new <- all_1 %>% 
  mutate(Individual_Ethnicity = as.character(Individual_Ethnicity)) %>% 
  subset(Individual_Ethnicity != "Ancestry not reported")
contingency_table2_new <- table(
  all_1_new$consented_for_genetic_testing, 
  all_1_new$Individual_Ethnicity)
H1_2 <- chisq.test(contingency_table2_new, correct=FALSE)

###objective 2:
varian_H2_simple<-ovarian%>%select('STUDYID',
                                   'sex',
                                   age_group,
                                   Individual_Ethnicity,
                                   'consented_for_genetic_testing',
                                   'GCOS6_Pre_GC_Total_Computed',
                                   'GCOS6_Post_GC_Total_Computed',
                                   'GCOS6_Post_GT_Total_Computed',
                                   'MICRA Table.Aggregate Score1')
other_H2_simple<-other%>%select('STUDYID',
                                'sex',
                                age_group,
                                Individual_Ethnicity,
                                'consented_for_genetic_testing',
                                'GCOS6_Pre_GC_Total_Computed',
                                'GCOS6_Post_GC_Total_Computed',
                                'GCOS6_Post_GT_Total_Computed',
                                'MICRA Table.Aggregate Score1')

#rename the following columns
ovarian_H2_simple <- ovarian_H2_simple %>% 
  rename("pre" = "GCOS6_Pre_GC_Total_Computed",
         "postGC" = "GCOS6_Post_GC_Total_Computed",
         "postGT" = "GCOS6_Post_GT_Total_Computed",
         "micra" = "MICRA Table.Aggregate Score1")

other_H2_simple <- other_H2_simple %>% 
  rename("pre" = "GCOS6_Pre_GC_Total_Computed",
         "postGC" = "GCOS6_Post_GC_Total_Computed",
         "postGT" = "GCOS6_Post_GT_Total_Computed",
         "micra" = "MICRA Table.Aggregate Score1")

ovarian_H2_simple <- 
  ovarian_H2_simple[complete.cases(ovarian_H2_simple$consented_for_genetic_testing),]
ovarian_H2_simple$test <- 
  ifelse(ovarian_H2_simple$consented_for_genetic_testing == "Yes", 1, 0)
ovarian_H2_simple <- 
  ovarian_H2_simple %>%relocate(test, .after=age_group)
ovarian_H2_simple$consented_for_genetic_testing <- 
  ordered(ovarian_H2_simple$consented_for_genetic_testing,
          levels = c("Yes", "No"))

other_H2_simple <- 
  other_H2_simple[complete.cases(other_H2_simple$consented_for_genetic_testing),]
other_H2_simple$test <- 
  ifelse(other_H2_simple$consented_for_genetic_testing == "Yes", 1, 0)
other_H2_simple <- 
  other_H2_simple %>%relocate(test, .after=age_group)
other_H2_simple$consented_for_genetic_testing <- 
  ordered(other_H2_simple$consented_for_genetic_testing,
          levels = c("Yes", "No"))

#pivot longer with col 'time', no longer paired
ovarian_H2_simple_tidy <- ovarian_H2_simple %>% 
  select(- micra) %>%
  pivot_longer(cols=c('pre', 'postGC', 'postGT'),
               names_to='time',
               values_to='GCOS_score')


ovarian_H2_simple_tidy$time <- ordered(ovarian_H2_simple_tidy$time,
                                       levels = c("pre", "postGC", "postGT"))

other_H2_simple_tidy <- other_H2_simple %>% 
  select(- micra) %>%
  pivot_longer(cols=c('pre', 'postGC', 'postGT'),
               names_to='time',
               values_to='GCOS_score')

other_H2_simple_tidy$time <- ordered(other_H2_simple_tidy$time,
                                     levels = c("pre", "postGC", "postGT"))

H2_ovarian<-kruskal.test(GCOS_score ~ time, data = ovarian_H2_simple_tidy)
H2_other <- kruskal.test(GCOS_score ~ time, data = other_H2_simple_tidy)

#shrink data set H2 other
ovarian_H2_simple<-ovarian%>%select('STUDYID',
                                    'sex',
                                    age,
                                    age_group,
                                    Individual_Ethnicity,
                                    'consented_for_genetic_testing',
                                    'GCOS6_Pre_GC_Total_Computed',
                                    'GCOS6_Post_GC_Total_Computed',
                                    'GCOS6_Post_GT_Total_Computed',
                                    'MICRA Table.Aggregate Score1')
other_H2_simple<-other%>%select('STUDYID',
                                'sex',
                                age,
                                age_group,
                                Individual_Ethnicity,
                                'consented_for_genetic_testing',
                                'GCOS6_Pre_GC_Total_Computed',
                                'GCOS6_Post_GC_Total_Computed',
                                'GCOS6_Post_GT_Total_Computed',
                                'MICRA Table.Aggregate Score1')

#prepare same patient paired data for H2.1
other_GC_pair<-other_H2_simple%>%
  drop_na('GCOS6_Pre_GC_Total_Computed','GCOS6_Post_GC_Total_Computed')
other_GT_pair<-other_H2_simple%>%
  drop_na('GCOS6_Post_GC_Total_Computed','GCOS6_Post_GT_Total_Computed')
other_GC_pair_score_diff<-other_GC_pair%>%
  mutate(GC_score_diff=GCOS6_Post_GC_Total_Computed-GCOS6_Pre_GC_Total_Computed)
other_GT_pair_score_diff<-other_GT_pair%>%
  mutate(GT_score_diff=GCOS6_Post_GT_Total_Computed-GCOS6_Post_GC_Total_Computed)

#rename column titles and limit age to 18~70
ovarian_simple_renamed<-ovarian_H2_simple%>%
  rename(GCOS1='GCOS6_Pre_GC_Total_Computed',
         GCOS2='GCOS6_Post_GC_Total_Computed',
         GCOS3='GCOS6_Post_GT_Total_Computed',
         MICRA1='MICRA Table.Aggregate Score1')%>%
  filter(age>=18&age<=70)
other_simple_renamed<-other_H2_simple%>%
  rename(GCOS1='GCOS6_Pre_GC_Total_Computed',
         GCOS2='GCOS6_Post_GC_Total_Computed',
         GCOS3='GCOS6_Post_GT_Total_Computed',
         MICRA1='MICRA Table.Aggregate Score1')%>%
  filter(age>=18&age<=70)

#Adding box-plots to check homogeneity of GCOS score diff of Non-ovarian for H2.1
other_paired_GC_diff<-other_GC_pair_score_diff%>%
  select(GC_score_diff)%>%
  mutate(type="GC_score_diff")%>%
  rename(score_diff=GC_score_diff)
other_paired_GT_diff<-other_GT_pair_score_diff%>%
  select(GT_score_diff)%>%
  mutate(type="GT_score_diff")%>%
  rename(score_diff=GT_score_diff)
other_paired_diff_score_only<-rbind(other_paired_GC_diff,other_paired_GT_diff)
plot_other_H2_homo<-other_paired_diff_score_only%>%
  ggplot(aes(x=factor(type),y=score_diff))+
  geom_boxplot()+
  xlab("Score differences by medical service types")

H2_other_pair_GC<-t.test(
  other_GC_pair_score_diff$GC_score_diff,
  alternative=c("two.sided"),mu=0)
H2_other_pair_GT<-t.test(
  other_GT_pair_score_diff$GT_score_diff,
  alternative=c("two.sided"),mu=0)
table_H2_1_GC<-tidy(H2_other_pair_GC)%>%
  mutate(Variable="Other GC score difference")
table_H2_1_GT<-tidy(H2_other_pair_GT)%>%
  mutate(Variable="Other GT score difference")
table_H2_1<-rbind(table_H2_1_GC,table_H2_1_GT)
table_H2_1<-table_H2_1[,c(9,1:8)]
table_print_H2_1<-kable(
  table_H2_1, 
  caption = "Score Difference One-sample t-test")%>%
  kable_styling(font_size = 12)%>% 
  kable_styling(latex_options="scale_down")%>%
  kable_styling(latex_options="HOLD_position")

###objective 3:
###Deaing with NAs in the hyp_3 dataset
hyp_3$consent[is.na(hyp_3$consent)] <- 'Yes'
hyp_3$year_referral[is.na(hyp_3$year_referral)] <- median(hyp_3$year_referral, na.rm = TRUE)

hyp_3$event <- ifelse(is.na(hyp_3$year_results), 0, 1)
# Replace NA in 'referral_to_results' with random values from the same column
#set.seed(1234) 
# hyp_3$referral_to_results[is.na(hyp_3$referral_to_results)] <- 
#   sample(hyp_3$referral_to_results[!is.na(hyp_3$referral_to_results)], 
#          sum(is.na(hyp_3$referral_to_results)), 
#          replace = TRUE)
# Replace NA in 'year_results' with the median of the column
# hyp_3$year_results[is.na(hyp_3$year_results)] <- median(hyp_3$year_results, na.rm = TRUE)

###extract data for hyp_3
referral_results_baseline <- hyp_3 %>%
  filter(!is.na(referral_to_results) &
           !is.na(year_referral) & 
           referral_to_results > 0 & 
           year_referral < 2021 &
           referral_to_results <= 365*27)

library(survival)
surv_obj <- Surv(time = referral_results_baseline$referral_to_results, 
                 event = referral_results_baseline$event)

# Fit a Cox proportional hazards model
cox_model <- coxph(surv_obj ~ as.factor(eth) + as.factor(age) + year_referral 
                   + as.factor(cancertype) , 
                   data = referral_results_baseline)

# Summary of the Cox model
summary_cox <- summary(cox_model)


# Load the survival package
library(survival)
library(survminer)
km_fit <- survfit(surv_obj ~ 1, data = referral_results_baseline)

# Plot the Kaplan-Meier survival curve with the main line in red
plot(km_fit, xlab = "Days from referral to results", ylab = "Survival probability", main = "Kaplan-Meier Survival Curve",
     col = "red", lwd = 2, conf.int = FALSE) # Plot without the CI to ensure the main line is red

# Add the confidence intervals using lines() and the $upper and $lower attributes of the survfit object
lines(km_fit$time, km_fit$upper, lty = 2, col = "blue", lwd = 2)
lines(km_fit$time, km_fit$lower, lty = 2, col = "blue", lwd = 2)

# Adding a legend to the plot if necessary
legend("topright", legend=c("Overall Survival", "95% Confidence Interval"), col=c("red", "blue"), lwd=2, lty=c(1, 2))



