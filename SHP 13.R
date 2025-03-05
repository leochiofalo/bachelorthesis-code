load("/Users/leo/Dropbox/Mac/Desktop/Bachelorarbeit/SHP/swissubase_932_8_0/data/Data_R/SHPPH13.RData")

#keep only people aged 25 - 62
SHP13 <- subset(SHP13P, AGE13 >= 25 & AGE13 <= 62)

rm(SHP13H)

#keep only people employed by private household, not-own private firm or government organisation
SHP13<- subset(SHP13, P13W29 %in% c(1, 5))

#keep only people with a yearly work income, has already been constructed by SHP
SHP13 <- subset(SHP13, I13WYG >= 0)

#summary(SHP21$I21WYG) #Jährliches Erwerbseinkommen brutto 
#plot(density(SHP21$I21WYG))
#boxplot(SHP21$I21WYG) #grosse ausreisser

### Stundenlohn berechnen ###
# keep only people that didn`t change job in the last year bc we don`t know anything about last job
table(SHP13$P13W18)
SHP13 <- subset(SHP13, P13W18 == 4)

# keep only people with one job because we don`t know a lot about 2nd jobs
SHP13 <- subset(SHP13, P13W17 == 1)

#Hourly wage = Annualized work income including extra salaries and bonus/Annualized number of hours (Hours worked weekly at time of interview*52)

SHP13$HWAGE13 <- SHP13$I13WYG / (SHP13$P13W77 * 52)

# keep only people with a positive hourly wage
SHP13 <- subset(SHP13, HWAGE13 >= 0)

summary(SHP13$HWAGE13) #Stundenlöhne
plot(density(SHP13$HWAGE13))
boxplot(SHP13$HWAGE13)
hist(SHP13$HWAGE13, breaks = 100)

# to get rid of the most extreme percentiles
lower_bound13 <- quantile(SHP13$HWAGE13, 0.01)
upper_bound13 <- quantile(SHP13$HWAGE13, 0.99)
# Filter the data to keep only observations within these bounds
SHP13 <- subset(SHP13, HWAGE13 >= lower_bound13 & HWAGE13 <= upper_bound13)

summary(SHP13$HWAGE13)
boxplot(SHP13$HWAGE13)
plot(density(SHP13$HWAGE13))
hist(SHP13$HWAGE13, breaks = 100)


SHP13$SEX13 <- factor(SHP13$SEX13, labels = c("male", "female"))
summary(SHP13$SEX13)

# Convert PERMIT04 to a factor with descriptive labels
table(SHP13$PERMIT13)
SHP13$PERMIT13 <- factor(SHP13$PERMIT13,
                         levels = c(-8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7),
                         labels = c("Other error",
                                    "Filter error",
                                    "SWISS",
                                    "No answer",
                                    "Does not know",
                                    "Residential permit C",
                                    "Annual permit B",
                                    "Seasonal permit A",
                                    "Diplomat or International Status",
                                    "Refugee or applicant for refuge",
                                    "Short-term permit L/B (max.18 months)",
                                    "Other"))
summary(SHP13$PERMIT13)

#Swiss dummy
SHP13$SWISS13 <- ifelse(SHP13$NAT_1_13 == 8100 | SHP13$NAT_2_13 == 8100 | SHP13$NAT_3_13 == 8100, 1, 2)
SHP13$SWISS13 <- factor(SHP13$SWISS13, levels = c(1, 2), labels = c("SWISS", "FOREIGN"))
summary(SHP13$SWISS13)


# Add the NATIONALITY variable to the dataset
nationality_mapping <- c(
  "8100" = "Switzerland", "8201" = "Albania", "8204" = "Belgium", 
  "8206" = "Denmark", "8207" = "Germany", "8211" = "Finland",
  "8212" = "France", "8214" = "Greece", 
  "8215" = "United Kingdom", "8218" = "Italy",
  "8220" = "Yugoslavia", "8222" = "Liechtenstein", 
  "8227" = "Netherlands", "8229" = "Austria", "8230" = "Poland", 
  "8231" = "Portugal", "8232" = "Romania", "8234" = "Sweden", 
  "8236" = "Spain", "8239" = "Turkey", "8240" = "Hungary", 
  "8243" = "Slovakia", "8244" = "Czech Republic",
  "8248" = "Serbia", "8249" = "Serbia and Montenegro", 
  "8250" = "Croatia", "8252" = "Bosnia and Herzegovina", 
  "8255" = "Macedonia", "8256" = "Kosovo", "8264" = "Russia",
  "8266" = "Belarus", "8305" = "Angola", "8310" = "Ivory Coast", 
  "8319" = "Cape Verde", "8322" = "Congo", "8323" = "Dem. Rep. Congo",
  "8327" = "Madagascar", "8331" = "Morocco", "8357" = "Tunisia",
  "8359" = "Egypt", "8362" = "Eritrea", "8406" = "Brazil", 
  "8407" = "Chile", "8410" = "Ecuador", "8418" = "Haiti", 
  "8423" = "Canada", "8424" = "Columbia", "8425" = "Cuba", 
  "8427" = "Mexico", "8429" = "Nicaragua", "8432" = "Peru",
  "8438" = "Venezuela", "8439" = "USA", "8501" = "Afghanistan", 
  "8506" = "Sri Lanka", "8508" = "China", "8510" = "India", 
  "8514" = "Israel", "8515" = "Japan", "8523" = "Lebanon", 
  "8525" = "Malaysia", "8533" = "Pakistan", "8534" = "Philippines", 
  "8537" = "Singapore", "8601" = "Australia", "8998" = "no nationality"
)

SHP13 <- SHP13 %>%
  mutate(
    NATIONALITY_1_13 = factor(recode(as.character(NAT_1_13), !!!nationality_mapping))
  )
summary(SHP13$NATIONALITY_1_13)


SHP13 <- SHP13 %>%
  mutate(
    NATIONALITY13 = factor(ifelse(
      SWISS13 == "SWISS", 
      "Switzerland", 
      as.character(NATIONALITY_1_13)
    ))
  )
summary(SHP13$NATIONALITY13)


# Add the REGION variable to the dataset
region_mapping <- c(
  "8100" = "Switzerland",
  "8207" = "Neighbouring Country", "8212" = "Neighbouring Country", "8218" = "Neighbouring Country", 
  "8222" = "Neighbouring Country", "8229" = "Neighbouring Country",
  "8204" = "EU-15 & EFTA", "8206" = "EU-15 & EFTA", "8211" = "EU-15 & EFTA", 
  "8214" = "EU-15 & EFTA", "8215" = "EU-15 & EFTA", "8227" = "EU-15 & EFTA", 
  "8231" = "EU-15 & EFTA", "8234" = "EU-15 & EFTA", "8236" = "EU-15 & EFTA", 
  "8201" = "Rest of Europe", "8220" = "Rest of Europe",
  "8230" = "Rest of Europe", "8232" = "Rest of Europe", "8239" = "Rest of Europe",
  "8240" = "Rest of Europe", "8243" = "Rest of Europe", "8244" = "Rest of Europe",
  "8248" = "Rest of Europe", "8249" = "Rest of Europe", "8250" = "Rest of Europe",
  "8252" = "Rest of Europe", "8255" = "Rest of Europe", "8256" = "Rest of Europe",
  "8264" = "Rest of Europe", "8266" = "Rest of Europe",
  "8305" = "3rd Country", "8310" = "3rd Country", "8319" = "3rd Country",
  "8322" = "3rd Country", "8323" = "3rd Country", "8327" = "3rd Country",
  "8331" = "3rd Country", "8357" = "3rd Country", "8359" = "3rd Country",
  "8362" = "3rd Country", "8406" = "3rd Country", "8407" = "3rd Country",
  "8410" = "3rd Country", "8418" = "3rd Country", "8423" = "3rd Country",
  "8424" = "3rd Country", "8425" = "3rd Country", "8427" = "3rd Country",
  "8429" = "3rd Country", "8432" = "3rd Country", "8438" = "3rd Country",
  "8439" = "3rd Country", "8501" = "3rd Country", "8506" = "3rd Country",
  "8508" = "3rd Country", "8510" = "3rd Country", "8514" = "3rd Country",
  "8515" = "3rd Country", "8523" = "3rd Country", "8525" = "3rd Country",
  "8533" = "3rd Country", "8534" = "3rd Country", "8537" = "3rd Country",
  "8601" = "3rd Country", "8998" = "3rd Country"
)

SHP13 <- SHP13 %>%
  mutate(
    REGION_1_13 = factor(
      recode(as.character(NAT_1_13), !!!region_mapping),
      levels = c("Switzerland", "Neighbouring Country", "EU-15 & EFTA", "Rest of Europe", "3rd Country")
    )
  )
summary(SHP13$REGION_1_13)

SHP13 <- SHP13 %>%
  mutate(
    REGION13 = factor(ifelse(
      SWISS13 == "SWISS", 
      "Switzerland", 
      as.character(REGION_1_13)
    ))
  )
summary(SHP13$REGION13)


#Recode Education
table(SHP13$ISCED13)
SHP13$ISCED13 <- factor(SHP13$ISCED13,
                        levels = c(10, 20, 31, 32, 33, 41, 51, 52, 60),
                        labels = c("Primary Educ.",
                                   "Lower Secondary Educ. ",
                                   "Upper secondary education (preparation for tertiary education)",
                                   "Upper secondary education (preparation for further prof. education)",
                                   "Upper secondary education (entrance into the labor market)",
                                   "Post-secondary education non-tertiary (preparation for an institution for higher education)",
                                   "First stage of tertiary education (general education)",
                                   "First stage of tertiary education (professional education)",
                                   "Second stage of tertiary education"
                        ))
summary(SHP13$ISCED13)

library(forcats)
SHP13$EDU13 <- fct_collapse(SHP13$ISCED13,
                            ObligatoryEduc = c("Primary Educ.", "Lower Secondary Educ. "),
                            SecondaryEduc = c("Upper secondary education (preparation for tertiary education)",
                                              "Upper secondary education (preparation for further prof. education)",
                                              "Upper secondary education (entrance into the labor market)",
                                              "Post-secondary education non-tertiary (preparation for an institution for higher education)"),
                            TertiaryEduc = c("First stage of tertiary education (general education)",
                                             "First stage of tertiary education (professional education)",
                                             "Second stage of tertiary education"))
detach("package:forcats", unload = TRUE)
summary(SHP13$EDU13)


#Job Qualifications 
table(SHP13$P13W100)
SHP13$P13W100 <- factor(SHP13$P13W100,
                        levels = c(-2, -1, 1, 2, 3, 4),
                        labels = c("No answer",
                                   "Does not know",
                                   "not sufficient qualifications",
                                   "correspond",
                                   "superior qualifications",
                                   "qualifications do not relate"
                        ))
summary(SHP13$P13W100)


#################### DESCRIPTIVE STATISTICS ######################

#log(Hourly Wages) for Regional means with Confidence Intervals
# Compute means and confidence intervals for each region
REG13_mean <- SHP13 %>%
  group_by(REGION13) %>%
  summarise(
    mean = mean(log(HWAGE13), na.rm = TRUE),
    se = sd(log(HWAGE13), na.rm = TRUE) / sqrt(sum(!is.na(log(HWAGE13)))),  # Standard error
    .groups = "drop"
  )
#Table
print(REG13_mean)


round(100*prop.table(table(SHP13$REGION13)), 1)
round(100*prop.table(table(SHP13$REGION13, SHP13$EDU13), margin = 1), 1)
round(100*prop.table(table(SHP13$REGION13, SHP13$SEX13), margin = 1), 1)
round(tapply(SHP13$AGE13, SHP13$REGION13, mean, na.rm = TRUE), 1)


#Hourly Wages for Regional means with Confidence Intervals
# Compute means and confidence intervals for each region
REG13_mean_ci <- SHP13 %>%
  group_by(REGION13) %>%
  summarise(
    mean = mean(HWAGE13, na.rm = TRUE),
    se = sd(HWAGE13, na.rm = TRUE) / sqrt(sum(!is.na(HWAGE13))),  # Standard error
    lower_ci = mean - qnorm(0.975) * se,
    upper_ci = mean + qnorm(0.975) * se,
    .groups = "drop"
  )
# Reorder the factor levels
REG13_mean_ci$REGION13 <- factor(REG13_mean_ci$REGION13, levels = c(
  "Switzerland", "Neighbouring Country", "EU-15 & EFTA", "Rest of Europe", "3rd Country"
))
#Barplot
plot13 <- ggplot(REG13_mean_ci, aes(x = REGION13, y = mean, fill = REGION13)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    width = 0.2
  ) +
  scale_fill_manual(values = c("grey30", "grey40", "grey50", "grey60", "grey70"))+ 
  labs(
    title = "2013",
    x = "Region",
    y = "Mean Hourly Wage"
  ) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#################### INFERENTIAL STATISTICS ######################
#REGION
summary(lm(log(HWAGE13) ~ relevel(factor(REGION13), ref="Switzerland"), data = SHP13))
model13_REGION <- lm(log(HWAGE13) ~ relevel(factor(REGION13), ref="Switzerland"), data = SHP13)
stargazer(model13_REGION, type = "text")


summary(lm(log(HWAGE13) ~ relevel(factor(REGION13), ref="Switzerland") + SEX13 + AGE13 + I(AGE13^2), data = SHP13))


#REGION EDU
summary(lm(log(HWAGE13) ~ relevel(factor(REGION13), ref="Switzerland") + relevel(factor(EDU13), ref="SecondaryEduc") + SEX13 + AGE13 + I(AGE13^2), data = SHP13))
model13_REGIONEDU <- lm(log(HWAGE13) ~ relevel(factor(REGION13), ref="Switzerland") + relevel(factor(EDU13), ref="SecondaryEduc") + SEX13 + AGE13 + I(AGE13^2), data = SHP13)
stargazer(model13_REGIONEDU, type = "text", out = "model13_REGIONEDU.html")

#REGION, separate regressions for EDU categories
summary(lm(log(HWAGE13) ~ relevel(factor(REGION13), ref="Switzerland") + SEX13 + AGE13 + I(AGE13^2), data = subset(SHP13, EDU13 == "ObligatoryEduc")))
model13_REGIONObligEdu <- lm(log(HWAGE13) ~ relevel(factor(REGION13), ref="Switzerland") + SEX13 + AGE13 + I(AGE13^2), data = subset(SHP13, EDU13 == "ObligatoryEduc"))
stargazer(model13_REGIONObligEdu, type = "text")

summary(lm(log(HWAGE13) ~ relevel(factor(REGION13), ref="Switzerland") + SEX13 + AGE13 + I(AGE13^2), data = subset(SHP13, EDU13 == "SecondaryEduc")))
model13_REGIONSecondEdu <- lm(log(HWAGE13) ~ relevel(factor(REGION13), ref="Switzerland") + SEX13 + AGE13 + I(AGE13^2), data = subset(SHP13, EDU13 == "SecondaryEduc"))
stargazer(model13_REGIONSecondEdu, type = "text")

summary(lm(log(HWAGE13) ~ relevel(factor(REGION13), ref="Switzerland") + SEX13 + AGE13 + I(AGE13^2), data = subset(SHP13, EDU13 == "TertiaryEduc")))
model13_REGIONTertiEdu <- lm(log(HWAGE13) ~ relevel(factor(REGION13), ref="Switzerland") + SEX13 + AGE13 + I(AGE13^2), data = subset(SHP13, EDU13 == "TertiaryEduc"))
stargazer(model13_REGIONTertiEdu, type = "text")



summary(lm(log(HWAGE13) ~ relevel(factor(NATIONALITY13), ref="Switzerland") + relevel(factor(EDU13), ref="SecondaryEduc") + SEX13 + AGE13 + I(AGE13^2), data = SHP13))



