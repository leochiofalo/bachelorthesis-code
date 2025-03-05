library(dplyr)
library(ggplot2)
library(stargazer)

load("/Users/leo/Dropbox/Mac/Desktop/Bachelorarbeit/SHP/swissubase_932_8_0/data/Data_R/SHPPH99.RData")

#Wer soll im Sample sein? 
# those in (full?) employment, no self-employed
# (only?) males aged 25 - 64 (or younger?), females aged 25 - 59
# gewisse Einkommenswerte gegen oben und unten ausschliessen, range or percentile?

# round(100 * prop.table(table(SHP99P$WSTAT99)), digits = 2) #Working status
# round(100 * prop.table(table(SHP99P$OCCUPA99)), digits = 2) #Actual occupation, from grid
# round(100 * prop.table(table(SHP99P$P99W29)), digits = 2) #CMJ: Type of employment

#keep only people aged 25 - 62
SHP99 <- subset(SHP99P, AGE99 >= 25 & AGE99 <= 62)

rm(SHP99H)

#keep only people employed by private household, not-own private firm or government organisation
SHP99 <- subset(SHP99, P99W29 %in% c(1, 5))

#keep only people with a yearly work income
SHP99 <- subset(SHP99, I99WYG >= 0)

#summary(SHP99$I99WYG) #Jährliches Erwerbseinkommen brutto 
#plot(density(SHP99$I99WYG))
#boxplot(SHP99$I99WYG) #grosse ausreisser

### Stundenlohn berechnen ###
# keep only people that didn`t change job in the last year bc we don`t know anthing about last job
#table(SHP99$P99W18)
SHP99 <- subset(SHP99, P99W18 == 4)

# keep only people with one job because we don`t know a lot about 2nd jobs
SHP99 <- subset(SHP99, P99W17 == 1)

#Hourly wage = Annualized work income including extra salaries and bonus/Annualized number of hours (Hours worked weekly at time of interview*52)
SHP99$HWAGE99 <- SHP99$I99WYG / (SHP99$P99W77 * 52)

# keep only people with a positive hourly wage
SHP99 <- subset(SHP99, HWAGE99 >= 0)

#summary(SHP99$HWAGE99) #Stundenlöhne
#plot(density(SHP99$HWAGE99))
#boxplot(SHP99$HWAGE99)
#hist(SHP99$HWAGE99, breaks = 100)

# to get rid of the most extreme percentiles
lower_bound99 <- quantile(SHP99$HWAGE99, 0.01)
upper_bound99 <- quantile(SHP99$HWAGE99, 0.99)
# Filter the data to keep only observations within these bounds
SHP99 <- subset(SHP99, HWAGE99 >= lower_bound99 & HWAGE99 <= upper_bound99)

#summary(SHP99$HWAGE99)
#boxplot(SHP99$HWAGE99)
#plot(density(SHP99$HWAGE99))
#hist(SHP99$HWAGE99, breaks = 100)

SHP99$SEX99 <- factor(SHP99$SEX99, labels = c("male", "female"))
#unclass(SHP99$SEX99)

# Convert PERMIT99 to a factor with descriptive labels
SHP99$PERMIT99 <- factor(SHP99$PERMIT99,
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
summary(SHP99$PERMIT99)

#Swiss dummy
SHP99$SWISS99 <- ifelse(SHP99$NAT_1_99 == 8100 | SHP99$NAT_2_99 == 8100 | SHP99$NAT_3_99 == 8100, 1, 2)
SHP99$SWISS99 <- factor(SHP99$SWISS99, levels = c(1, 2), labels = c("SWISS", "FOREIGN"))

#summary(SHP99$SWISS99)
#class(SHP99$SWISS99)


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

SHP99 <- SHP99 %>%
  mutate(
    NATIONALITY_1_99 = factor(recode(as.character(NAT_1_99), !!!nationality_mapping))
  )
summary(SHP99$NATIONALITY_1_99)


SHP99 <- SHP99 %>%
  mutate(
    NATIONALITY99 = factor(ifelse(
      SWISS99 == "SWISS", 
      "Switzerland", 
      as.character(NATIONALITY_1_99)  
    ))
  )
summary(SHP99$NATIONALITY99)


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

SHP99 <- SHP99 %>%
  mutate(
    REGION_1_99 = factor(
      recode(as.character(NAT_1_99), !!!region_mapping),
      levels = c("Switzerland", "Neighbouring Country", "EU-15 & EFTA", "Rest of Europe", "3rd Country")
    )
  )
summary(SHP99$REGION_1_99)

SHP99 <- SHP99 %>%
  mutate(
    REGION99 = factor(ifelse(
      SWISS99 == "SWISS", 
      "Switzerland", 
      as.character(REGION_1_99)  
    ))
  )
summary(SHP99$REGION99)


#Recode Education
table(SHP99$ISCED99)
SHP99$ISCED99 <- factor(SHP99$ISCED99,
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
summary(SHP99$ISCED99)

library(forcats)
SHP99$EDU99 <- fct_collapse(SHP99$ISCED99,
                            ObligatoryEduc = c("Primary Educ.", "Lower Secondary Educ. "),
                            SecondaryEduc = c("Upper secondary education (preparation for tertiary education)",
                                              "Upper secondary education (preparation for further prof. education)",
                                              "Upper secondary education (entrance into the labor market)",
                                              "Post-secondary education non-tertiary (preparation for an institution for higher education)"),
                            TertiaryEduc = c("First stage of tertiary education (general education)",
                                             "First stage of tertiary education (professional education)",
                                             "Second stage of tertiary education"))
detach("package:forcats", unload = TRUE)
summary(SHP99$EDU99) #1,2,3


#################### DESCRIPTIVE STATISTICS ######################

#log(Hourly Wages) for Regional means with Confidence Intervals
# Compute means and confidence intervals for each region
REG99_mean <- SHP99 %>%
  group_by(REGION99) %>%
  summarise(
    mean = mean(log(HWAGE99), na.rm = TRUE),
    se = sd(log(HWAGE99), na.rm = TRUE) / sqrt(sum(!is.na(log(HWAGE99)))),  # Standard error
    .groups = "drop"
  )
#Table
print(REG99_mean)


round(100*prop.table(table(SHP99$REGION99)), 1)
round(100*prop.table(table(SHP99$REGION99, SHP99$EDU99), margin = 1), 1)
round(100*prop.table(table(SHP99$REGION99, SHP99$SEX99), margin = 1), 1)
round(tapply(SHP99$AGE99, SHP99$REGION99, mean, na.rm = TRUE), 1)


#Hourly Wages for Regional means with Confidence Intervals
# Compute means and confidence intervals for each region
REG99_mean_ci <- SHP99 %>%
  group_by(REGION99) %>%
  summarise(
    mean = mean(HWAGE99, na.rm = TRUE),
    se = sd(HWAGE99, na.rm = TRUE) / sqrt(sum(!is.na(HWAGE99))),  # Standard error
    lower_ci = mean - qnorm(0.975) * se,
    upper_ci = mean + qnorm(0.975) * se,
    .groups = "drop"
  )
#Table
print(REG99_mean_ci)
# Reorder the factor levels
REG99_mean_ci$REGION99 <- factor(REG99_mean_ci$REGION99, levels = c(
  "Switzerland", "Neighbouring Country", "EU-15 & EFTA", "Rest of Europe", "3rd Country"
))
# Create the barplot with custom ordering and grey shades
plot99 <- ggplot(REG99_mean_ci, aes(x = REGION99, y = mean, fill = REGION99)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    width = 0.1
  ) +
  scale_fill_manual(values = c("grey30", "grey40", "grey50", "grey60", "grey70")) +  # Different grey shades
  labs(
    title = "1999",
    x = "Region",
    y = "Mean Hourly Wage"
  ) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#################### INFERENTIAL STATISTICS ######################


#REGION
summary(lm(log(HWAGE99) ~ relevel(factor(REGION99), ref="Switzerland"), data = SHP99))
model99_REGION <- lm(log(HWAGE99) ~ relevel(factor(REGION99), ref="Switzerland"), data = SHP99)
stargazer(model99_REGION, type = "text")


summary(lm(log(HWAGE99) ~ relevel(factor(REGION99), ref="Switzerland") + SEX99 + AGE99 + I(AGE99^2), data = SHP99))


#REGION EDU
summary(lm(log(HWAGE99) ~ relevel(factor(REGION99), ref="Switzerland") + relevel(factor(EDU99), ref="SecondaryEduc") + SEX99 + AGE99 + I(AGE99^2), data = SHP99))
model99_REGIONEDU <- lm(log(HWAGE99) ~ relevel(factor(REGION99), ref="Switzerland") + relevel(factor(EDU99), ref="SecondaryEduc") + SEX99 + AGE99 + I(AGE99^2), data = SHP99)
stargazer(model99_REGIONEDU, type = "text")


#REGION, separate regressions for EDU categories
summary(lm(log(HWAGE99) ~ relevel(factor(REGION99), ref="Switzerland") + SEX99 + AGE99 + I(AGE99^2), data = subset(SHP99, EDU99 == "ObligatoryEduc")))
model99_REGIONObligEdu <- lm(log(HWAGE99) ~ relevel(factor(REGION99), ref="Switzerland") + SEX99 + AGE99 + I(AGE99^2), data = subset(SHP99, EDU99 == "ObligatoryEduc"))
stargazer(model99_REGIONObligEdu, type = "text")

summary(lm(log(HWAGE99) ~ relevel(factor(REGION99), ref="Switzerland") + SEX99 + AGE99 + I(AGE99^2), data = subset(SHP99, EDU99 == "SecondaryEduc")))
model99_REGIONSecondEdu <- lm(log(HWAGE99) ~ relevel(factor(REGION99), ref="Switzerland") + SEX99 + AGE99 + I(AGE99^2), data = subset(SHP99, EDU99 == "SecondaryEduc"))
stargazer(model99_REGIONSecondEdu, type = "text")

summary(lm(log(HWAGE99) ~ relevel(factor(REGION99), ref="Switzerland") + SEX99 + AGE99 + I(AGE99^2), data = subset(SHP99, EDU99 == "TertiaryEduc")))
model99_REGIONTertiEdu <- lm(log(HWAGE99) ~ relevel(factor(REGION99), ref="Switzerland") + SEX99 + AGE99 + I(AGE99^2), data = subset(SHP99, EDU99 == "TertiaryEduc"))
stargazer(model99_REGIONTertiEdu, type = "text")



summary(lm(log(HWAGE99) ~ relevel(factor(NATIONALITY99), ref="Switzerland") + relevel(factor(EDU99), ref="SecondaryEduc") + SEX99 + AGE99 + I(AGE99^2), data = SHP99))

