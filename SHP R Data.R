install.packages("foreign", type = "source")
library(foreign)
install.packages("stringr", type = "source")
library(stringr)
setwd("~/Dropbox/Mac/Desktop/Bachelorarbeit/SHP/
      swissubase_932_8_0/data/Data_SPSS")


############################## BASES ########################################

SHPMP = read.spss("./SHP-Data-WA-SPSS/SHP_MP.sav", to.data.frame=T, 
                  use.value.labels=F, use.missings=F)
SHPMH = read.spss("./SHP-Data-WA-SPSS/SHP_MH.sav", to.data.frame=T, 
                  use.value.labels=F, use.missings=F) 
SHPLJ = read.spss("./SHP-Data-WA-SPSS/SHP_LJ.sav", to.data.frame=T, 
                  use.value.labels=F, use.missings=F) 
SHPSO = read.spss("./SHP-Data-WA-SPSS/SHP_SO.sav", to.data.frame=T, 
                  use.value.labels=F, use.missings=F) 
SHPCA = read.spss("./SHP-Data-WA-SPSS/SHP_CA.sav", to.data.frame=T, 
                  use.value.labels=F, use.missings=F) 

SHPMP = SHPMP[order(SHPMP$IDPERS),]
SHPMH = SHPMH[order(SHPMH$IDHOUS..),]
SHPLJ = SHPLJ[order(SHPLJ$IDPERS),]
SHPSO = SHPSO[order(SHPSO$IDPERS),]
SHPCA = SHPCA[order(SHPCA$IDPERS),]

years = as.character(1999:2022)          # change hier
years2 = str_sub(years,start=-2, end=-1) # two digit (from the end) years
nbwaves = length(years)
waves = 1:nbwaves
finwaves = as.character(waves[length(waves)])

save(years, years2, nbwaves, waves, finwaves, file="./ObjectsForLoop.Rdata") 
#("./ObjectsForLoop.Rdata")

for(i in 1:nbwaves){
  y=years
  y2=sprintf("%02s", years2)
  w=sprintf("%1d", waves)
  fw=finwaves
  eval(parse(text=sprintf("SHP%1$sP=read.spss(\"./SHP-Data-W1-W%2$s-SPSS/
  W%3$s_%4$s/SHP%1$s_P_USER.sav\",
  to.data.frame=T,use.value.labels=F, use.missings=F)
				 SHP%1$sH=read.spss(\"./SHP-Data-W1-W%2$s-SPSS/W%3$s_%4$s
				 /SHP%1$s_H_USER.sav\",to.data.frame=T,
				 use.value.labels=F, use.missings=F)
				 SHP%1$sP=SHP%1$sP[order(SHP%1$sP$IDPERS),]
				 SHP%1$sH=SHP%1$sH[order(SHP%1$sH$IDHOUS%1$s),]
				 save(SHP%1$sP,SHP%1$sH, file=\"./SHPPH%1$s.RData\")
				", y2[i], fw[1], w[i], y[i])))			
}

###############################################################################
library(dplyr)
library(ggplot2)
library(stargazer)
library(patchwork)

#################### 1999 ######################
load("/Users/leo/Dropbox/Mac/Desktop/Bachelorarbeit/SHP/swissubase_932_8_0/data/Data_R/SHPPH99.RData")

#keep only people aged 25 - 62
SHP99 <- subset(SHP99P, AGE99 >= 25 & AGE99 <= 62)

rm(SHP99H)

#keep only people employed by private household, not-own private firm or government organisation
SHP99 <- subset(SHP99, P99W29 %in% c(1, 5))

#keep only people with a yearly work income
SHP99 <- subset(SHP99, I99WYG >= 0)

### Compute hourly wage ###
# keep only people that didn`t change job in the last year bc we don`t know anthing about last job
#table(SHP99$P99W18)
SHP99 <- subset(SHP99, P99W18 == 4)

# keep only people with one job because we don`t know a lot about 2nd jobs
SHP99 <- subset(SHP99, P99W17 == 1)

#Hourly wage = Annualized work income including extra salaries and bonus/Annualized number of hours (Hours worked weekly at time of interview*52)
SHP99$HWAGE99 <- SHP99$I99WYG / (SHP99$P99W77 * 52)

# keep only people with a positive hourly wage
SHP99 <- subset(SHP99, HWAGE99 >= 0)

# to get rid of the most extreme percentiles
lower_bound99 <- quantile(SHP99$HWAGE99, 0.01)
upper_bound99 <- quantile(SHP99$HWAGE99, 0.99)
# Filter the data to keep only observations within these bounds
SHP99 <- subset(SHP99, HWAGE99 >= lower_bound99 & HWAGE99 <= upper_bound99)

SHP99$SEX99 <- factor(SHP99$SEX99, labels = c("male", "female"))

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

#Swiss dummy
SHP99$SWISS99 <- ifelse(SHP99$NAT_1_99 == 8100 | SHP99$NAT_2_99 == 8100 | SHP99$NAT_3_99 == 8100, 1, 2)
SHP99$SWISS99 <- factor(SHP99$SWISS99, levels = c(1, 2), labels = c("SWISS", "FOREIGN"))


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


SHP99 <- SHP99 %>%
  mutate(
    NATIONALITY99 = factor(ifelse(
      SWISS99 == "SWISS", 
      "Switzerland", 
      as.character(NATIONALITY_1_99)  
    ))
  )


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


SHP99 <- SHP99 %>%
  mutate(
    REGION99 = factor(ifelse(
      SWISS99 == "SWISS", 
      "Switzerland", 
      as.character(REGION_1_99)  
    ))
  )


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



#################### 2004 ######################
load("/Users/leo/Dropbox/Mac/Desktop/Bachelorarbeit/SHP/swissubase_932_8_0/data/Data_R/SHPPH04.RData")

#keep only people aged 25 - 62
SHP04 <- subset(SHP04P, AGE04 >= 25 & AGE04 <= 62)

rm(SHP04H)

#keep only people employed by private household, not-own private firm or government organisation
SHP04 <- subset(SHP04, P04W29 %in% c(1, 5))

#keep only people with a yearly work income, has already been constructed by SHP
SHP04 <- subset(SHP04, I04WYG >= 0)

# keep only people that didn`t change job in the last year bc we don`t know anything about last job
table(SHP04$P04W18)
SHP04 <- subset(SHP04, P04W18 == 4)

# keep only people with one job because we don`t know a lot about 2nd jobs
SHP04 <- subset(SHP04, P04W17 == 1)

#Hourly wage = Annualized work income including extra salaries and bonus/Annualized number of hours (Hours worked weekly at time of interview*52)

SHP04$HWAGE04 <- SHP04$I04WYG / (SHP04$P04W77 * 52)

# keep only people with a positive hourly wage
SHP04 <- subset(SHP04, HWAGE04 >= 0)

# to get rid of the most extreme percentiles
lower_bound04 <- quantile(SHP04$HWAGE04, 0.01)
upper_bound04 <- quantile(SHP04$HWAGE04, 0.99)
# Filter the data to keep only observations within these bounds
SHP04 <- subset(SHP04, HWAGE04 >= lower_bound04 & HWAGE04 <= upper_bound04)

#Factors for sex
SHP04$SEX04 <- factor(SHP04$SEX04, labels = c("male", "female"))

# Convert PERMIT04 to a factor with descriptive labels
table(SHP04$PERMIT04)
SHP04$PERMIT04 <- factor(SHP04$PERMIT04,
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

#Swiss dummy
SHP04$SWISS04 <- ifelse(SHP04$NAT_1_04 == 8100 | SHP04$NAT_2_04 == 8100 | SHP04$NAT_3_04 == 8100, 1, 2)
SHP04$SWISS04 <- factor(SHP04$SWISS04, levels = c(1, 2), labels = c("SWISS", "FOREIGN"))


# Add the NATIONALITY variable to the dataset
SHP04 <- SHP04 %>%
  mutate(
    NATIONALITY_1_04 = factor(recode(as.character(NAT_1_04), !!!nationality_mapping))
  )


SHP04 <- SHP04 %>%
  mutate(
    NATIONALITY04 = factor(ifelse(
      SWISS04 == "SWISS", 
      "Switzerland", 
      as.character(NATIONALITY_1_04)
    ))
  )


# Add the REGION variable to the dataset
SHP04 <- SHP04 %>%
  mutate(
    REGION_1_04 = factor(
      recode(as.character(NAT_1_04), !!!region_mapping),
      levels = c("Switzerland", "Neighbouring Country", "EU-15 & EFTA", "Rest of Europe", "3rd Country")
    )
  )

SHP04 <- SHP04 %>%
  mutate(
    REGION04 = factor(ifelse(
      SWISS04 == "SWISS", 
      "Switzerland", 
      as.character(REGION_1_04)
    ))
  )


#Recode Education
table(SHP04$ISCED04)
SHP04$ISCED04 <- factor(SHP04$ISCED04,
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

library(forcats)
SHP04$EDU04 <- fct_collapse(SHP04$ISCED04,
                            ObligatoryEduc = c("Primary Educ.", "Lower Secondary Educ. "),
                            SecondaryEduc = c("Upper secondary education (preparation for tertiary education)",
                                              "Upper secondary education (preparation for further prof. education)",
                                              "Upper secondary education (entrance into the labor market)",
                                              "Post-secondary education non-tertiary (preparation for an institution for higher education)"),
                            TertiaryEduc = c("First stage of tertiary education (general education)",
                                             "First stage of tertiary education (professional education)",
                                             "Second stage of tertiary education"))
detach("package:forcats", unload = TRUE)

#################### DESCRIPTIVE STATISTICS ######################


#log(Hourly Wages) for Regional means with Confidence Intervals
# Compute means and confidence intervals for each region
REG04_mean <- SHP04 %>%
  group_by(REGION04) %>%
  summarise(
    mean = mean(log(HWAGE04), na.rm = TRUE),
    se = sd(log(HWAGE04), na.rm = TRUE) / sqrt(sum(!is.na(log(HWAGE04)))),  # Standard error
    .groups = "drop"
  )
#Table
print(REG04_mean)


round(100*prop.table(table(SHP04$REGION04)), 1)
round(100*prop.table(table(SHP04$REGION04, SHP04$EDU04), margin = 1), 1)
round(100*prop.table(table(SHP04$REGION04, SHP04$SEX04), margin = 1), 1)
round(tapply(SHP04$AGE04, SHP04$REGION04, mean, na.rm = TRUE), 1)


#Hourly Wages for Regional means with Confidence Intervals
# Compute means and confidence intervals for each region
REG04_mean_ci <- SHP04 %>%
  group_by(REGION04) %>%
  summarise(
    mean = mean(HWAGE04, na.rm = TRUE),
    se = sd(HWAGE04, na.rm = TRUE) / sqrt(sum(!is.na(HWAGE04))),  # Standard error
    lower_ci = mean - qnorm(0.975) * se,
    upper_ci = mean + qnorm(0.975) * se,
    .groups = "drop"
  )
#Table
print(REG04_mean_ci)
# Reorder the factor levels
REG04_mean_ci$REGION04 <- factor(REG04_mean_ci$REGION04, levels = c(
  "Switzerland", "Neighbouring Country", "EU-15 & EFTA", "Rest of Europe", "3rd Country"
))
#Barplot
plot04 <- ggplot(REG04_mean_ci, aes(x = REGION04, y = mean, fill = REGION04)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    width = 0.2
  ) +
  scale_fill_manual(values = c("grey30", "grey40", "grey50", "grey60", "grey70"))+ 
  labs(
    title = "2004",
    x = "Region",
    y = "Mean Hourly Wage"
  ) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#################### INFERENTIAL STATISTICS ######################

#REGION
summary(lm(log(HWAGE04) ~ relevel(factor(REGION04), ref="Switzerland"), data = SHP04))
model04_REGION <- lm(log(HWAGE04) ~ relevel(factor(REGION04), ref="Switzerland"), data = SHP04)
stargazer(model04_REGION, type = "text")


summary(lm(log(HWAGE04) ~ relevel(factor(REGION04), ref="Switzerland") + SEX04 + AGE04 + I(AGE04^2), data = SHP04))


#REGION EDU
summary(lm(log(HWAGE04) ~ relevel(factor(REGION04), ref="Switzerland") + relevel(factor(EDU04), ref="SecondaryEduc") + SEX04 + AGE04 + I(AGE04^2), data = SHP04))
model04_REGIONEDU <- lm(log(HWAGE04) ~ relevel(factor(REGION04), ref="Switzerland") + relevel(factor(EDU04), ref="SecondaryEduc") + SEX04 + AGE04 + I(AGE04^2), data = SHP04)
stargazer(model04_REGIONEDU, type = "text", out = "model04_REGIONEDU.html")


#REGION, separate regressions for EDU categories
summary(lm(log(HWAGE04) ~ relevel(factor(REGION04), ref="Switzerland") + SEX04 + AGE04 + I(AGE04^2), data = subset(SHP04, EDU04 == "ObligatoryEduc")))
model04_REGIONObligEdu <- lm(log(HWAGE04) ~ relevel(factor(REGION04), ref="Switzerland") + SEX04 + AGE04 + I(AGE04^2), data = subset(SHP04, EDU04 == "ObligatoryEduc"))
stargazer(model04_REGIONObligEdu, type = "text")

summary(lm(log(HWAGE04) ~ relevel(factor(REGION04), ref="Switzerland") + SEX04 + AGE04 + I(AGE04^2), data = subset(SHP04, EDU04 == "SecondaryEduc")))
model04_REGIONSecondEdu <- lm(log(HWAGE04) ~ relevel(factor(REGION04), ref="Switzerland") + SEX04 + AGE04 + I(AGE04^2), data = subset(SHP04, EDU04 == "SecondaryEduc"))
stargazer(model04_REGIONSecondEdu, type = "text")

summary(lm(log(HWAGE04) ~ relevel(factor(REGION04), ref="Switzerland") + SEX04 + AGE04 + I(AGE04^2), data = subset(SHP04, EDU04 == "TertiaryEduc")))
model04_REGIONTertiEdu <- lm(log(HWAGE04) ~ relevel(factor(REGION04), ref="Switzerland") + SEX04 + AGE04 + I(AGE04^2), data = subset(SHP04, EDU04 == "TertiaryEduc"))
stargazer(model04_REGIONTertiEdu, type = "text")


#################### 2013 ######################
load("/Users/leo/Dropbox/Mac/Desktop/Bachelorarbeit/SHP/swissubase_932_8_0/data/Data_R/SHPPH13.RData")

#keep only people aged 25 - 62
SHP13 <- subset(SHP13P, AGE13 >= 25 & AGE13 <= 62)

rm(SHP13H)

#keep only people employed by private household, not-own private firm or government organisation
SHP13<- subset(SHP13, P13W29 %in% c(1, 5))

#keep only people with a yearly work income, has already been constructed by SHP
SHP13 <- subset(SHP13, I13WYG >= 0)

# keep only people that didn`t change job in the last year bc we don`t know anything about last job
table(SHP13$P13W18)
SHP13 <- subset(SHP13, P13W18 == 4)

# keep only people with one job because we don`t know a lot about 2nd jobs
SHP13 <- subset(SHP13, P13W17 == 1)

#Hourly wage = Annualized work income including extra salaries and bonus/Annualized number of hours (Hours worked weekly at time of interview*52)
SHP13$HWAGE13 <- SHP13$I13WYG / (SHP13$P13W77 * 52)

# keep only people with a positive hourly wage
SHP13 <- subset(SHP13, HWAGE13 >= 0)

# to get rid of the most extreme percentiles
lower_bound13 <- quantile(SHP13$HWAGE13, 0.01)
upper_bound13 <- quantile(SHP13$HWAGE13, 0.99)
# Filter the data to keep only observations within these bounds
SHP13 <- subset(SHP13, HWAGE13 >= lower_bound13 & HWAGE13 <= upper_bound13)


SHP13$SEX13 <- factor(SHP13$SEX13, labels = c("male", "female"))


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

#Swiss dummy
SHP13$SWISS13 <- ifelse(SHP13$NAT_1_13 == 8100 | SHP13$NAT_2_13 == 8100 | SHP13$NAT_3_13 == 8100, 1, 2)
SHP13$SWISS13 <- factor(SHP13$SWISS13, levels = c(1, 2), labels = c("SWISS", "FOREIGN"))
summary(SHP13$SWISS13)


# Add the NATIONALITY variable to the dataset
SHP13 <- SHP13 %>%
  mutate(
    NATIONALITY_1_13 = factor(recode(as.character(NAT_1_13), !!!nationality_mapping))
  )


SHP13 <- SHP13 %>%
  mutate(
    NATIONALITY13 = factor(ifelse(
      SWISS13 == "SWISS", 
      "Switzerland", 
      as.character(NATIONALITY_1_13)
    ))
  )

# Add the REGION variable to the dataset
SHP13 <- SHP13 %>%
  mutate(
    REGION_1_13 = factor(
      recode(as.character(NAT_1_13), !!!region_mapping),
      levels = c("Switzerland", "Neighbouring Country", "EU-15 & EFTA", "Rest of Europe", "3rd Country")
    )
  )

SHP13 <- SHP13 %>%
  mutate(
    REGION13 = factor(ifelse(
      SWISS13 == "SWISS", 
      "Switzerland", 
      as.character(REGION_1_13)
    ))
  )


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


#################### 2021 ######################
load("/Users/leo/Dropbox/Mac/Desktop/Bachelorarbeit/SHP/swissubase_932_8_0/data/Data_R/SHPPH21.RData")

#keep only people aged 25 - 62
SHP21 <- subset(SHP21P, AGE21 >= 25 & AGE21 <= 62)

rm(SHP21H)

#keep only people employed by private household, not-own private firm or government organisation
SHP21 <- subset(SHP21, P21W29 %in% c(1, 5))

#keep only people with a yearly work income, has already been constructed by SHP
SHP21 <- subset(SHP21, I21WYG >= 0)

# keep only people that didn`t change job in the last year bc we don`t know anything about last job
table(SHP21$P21W18)
SHP21 <- subset(SHP21, P21W18 == 4)

# keep only people with one job because we don`t know a lot about 2nd jobs
SHP21 <- subset(SHP21, P21W17 == 1)

#Hourly wage = Annualized work income including extra salaries and bonus/Annualized number of hours (Hours worked weekly at time of interview*52)
SHP21$HWAGE21 <- SHP21$I21WYG / (SHP21$P21W77 * 52)

# keep only people with a positive hourly wage
SHP21 <- subset(SHP21, HWAGE21 >= 0)

# to get rid of the most extreme percentiles
lower_bound21 <- quantile(SHP21$HWAGE21, 0.01)
upper_bound21 <- quantile(SHP21$HWAGE21, 0.99)
# Filter the data to keep only observations within these bounds
SHP21 <- subset(SHP21, HWAGE21 >= lower_bound21 & HWAGE21 <= upper_bound21)

#Factors for sex
SHP21$SEX21 <- factor(SHP21$SEX21, labels = c("male", "female"))
summary(SHP21$SEX21)

# Convert PERMIT to a factor with descriptive labels
table(SHP21$PERMIT21)
SHP21$PERMIT21 <- factor(SHP21$PERMIT21,
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
summary(SHP21$PERMIT21)

#Swiss dummy
SHP21$SWISS21 <- ifelse(SHP21$NAT_1_21 == 8100 | SHP21$NAT_2_21 == 8100 | SHP21$NAT_3_21 == 8100, 1, 2)
SHP21$SWISS21 <- factor(SHP21$SWISS21, levels = c(1, 2), labels = c("SWISS", "FOREIGN"))
summary(SHP21$SWISS21)

# Add the NATIONALITY variable to the dataset
SHP21 <- SHP21 %>%
  mutate(
    NATIONALITY_1_21 = factor(recode(as.character(NAT_1_21), !!!nationality_mapping))
  )


SHP21 <- SHP21 %>%
  mutate(
    NATIONALITY21 = factor(ifelse(
      SWISS21 == "SWISS", 
      "Switzerland", 
      as.character(NATIONALITY_1_21)
    ))
  )


# Add the REGION variable to the dataset
SHP21 <- SHP21 %>%
  mutate(
    REGION_1_21 = factor(
      recode(as.character(NAT_1_21), !!!region_mapping),
      levels = c("Switzerland", "Neighbouring Country", "EU-15 & EFTA", "Rest of Europe", "3rd Country")
    )
  )

SHP21 <- SHP21 %>%
  mutate(
    REGION21 = factor(ifelse(
      SWISS21 == "SWISS", 
      "Switzerland", 
      as.character(REGION_1_21)
    ))
  )


#Recode Education
table(SHP21$ISCED21)
SHP21 <- subset(SHP21, ISCED21 >= 0)    #gets rid of two observations that didnt answer/know
SHP21$ISCED21 <- factor(SHP21$ISCED21,
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

library(forcats)
SHP21$EDU21 <- fct_collapse(SHP21$ISCED21,
                            ObligatoryEduc = c("Primary Educ.", "Lower Secondary Educ. "),
                            SecondaryEduc = c("Upper secondary education (preparation for tertiary education)",
                                              "Upper secondary education (preparation for further prof. education)",
                                              "Upper secondary education (entrance into the labor market)",
                                              "Post-secondary education non-tertiary (preparation for an institution for higher education)"),
                            TertiaryEduc = c("First stage of tertiary education (general education)",
                                             "First stage of tertiary education (professional education)",
                                             "Second stage of tertiary education"))
detach("package:forcats", unload = TRUE)

#################### DESCRIPTIVE STATISTICS ####################

#log(Hourly Wages) for Regional means with Confidence Intervals
# Compute means and confidence intervals for each region
REG21_mean <- SHP21 %>%
  group_by(REGION21) %>%
  summarise(
    mean = mean(log(HWAGE21), na.rm = TRUE),
    se = sd(log(HWAGE21), na.rm = TRUE) / sqrt(sum(!is.na(log(HWAGE21)))),  # Standard error
    .groups = "drop"
  )
#Table
print(REG21_mean)


round(100*prop.table(table(SHP21$REGION21)), 1)
round(100*prop.table(table(SHP21$REGION21, SHP21$EDU21), margin = 1), 1)
round(100*prop.table(table(SHP21$REGION21, SHP21$SEX21), margin = 1), 1)
round(tapply(SHP21$AGE21, SHP21$REGION21, mean, na.rm = TRUE), 1)


#Hourly Wages for Regional means with Confidence Intervals
# Compute means and confidence intervals for each region
REG21_mean_ci <- SHP21 %>%
  group_by(REGION21) %>%
  summarise(
    mean = mean(HWAGE21, na.rm = TRUE),
    se = sd(HWAGE21, na.rm = TRUE) / sqrt(sum(!is.na(HWAGE21))),  # Standard error
    lower_ci = mean - qnorm(0.975) * se,
    upper_ci = mean + qnorm(0.975) * se,
    .groups = "drop"
  )
# Reorder the factor levels
REG21_mean_ci$REGION21 <- factor(REG21_mean_ci$REGION21, levels = c(
  "Switzerland", "Neighbouring Country", "EU-15 & EFTA", "Rest of Europe", "3rd Country"
))
#Barplot
plot21 <- ggplot(REG21_mean_ci, aes(x = REGION21, y = mean, fill = REGION21)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    width = 0.2
  ) +
  scale_fill_manual(values = c("grey30", "grey40", "grey50", "grey60", "grey70"))+
  labs(
    title = "2021",
    x = "Region",
    y = "Mean Hourly Wage"
  ) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


combined_plot <- (plot99 | plot04) / (plot13 | plot21)
#ggsave("combined_plot.png", combined_plot, width = 13, height = 10, dpi = 300)


#################### INFERENTIAL STATISTICS ######################
#REGION 
summary(lm(log(HWAGE21) ~ relevel(factor(REGION21), ref="Switzerland"), data = SHP21))
model21_REGION <- lm(log(HWAGE21) ~ relevel(factor(REGION21), ref="Switzerland"), data = SHP21)
stargazer(model21_REGION, type = "text")


summary(lm(log(HWAGE21) ~ relevel(factor(REGION21), ref="Switzerland") + SEX21 + AGE21 + I(AGE21^2), data = SHP21))


#REGION EDU
summary(lm(log(HWAGE21) ~ relevel(factor(REGION21), ref="Switzerland") + relevel(factor(EDU21), ref="SecondaryEduc") + SEX21 + AGE21 + I(AGE21^2), data = SHP21))
model21_REGIONEDU <- lm(log(HWAGE21) ~ relevel(factor(REGION21), ref="Switzerland") + relevel(factor(EDU21), ref="SecondaryEduc") + SEX21 + AGE21 + I(AGE21^2), data = SHP21)
stargazer(model21_REGIONEDU, type = "text")

#REGION, separate regressions for EDU categories
summary(lm(log(HWAGE21) ~ relevel(factor(REGION21), ref="Switzerland") + SEX21 + AGE21 + I(AGE21^2), data = subset(SHP21, EDU21 == "ObligatoryEduc")))
model21_REGIONObligEdu <- lm(log(HWAGE21) ~ relevel(factor(REGION21), ref="Switzerland") + SEX21 + AGE21 + I(AGE21^2), data = subset(SHP21, EDU21 == "ObligatoryEduc"))
stargazer(model21_REGIONObligEdu, type = "text")

summary(lm(log(HWAGE21) ~ relevel(factor(REGION21), ref="Switzerland") + SEX21 + AGE21 + I(AGE21^2), data = subset(SHP21, EDU21 == "SecondaryEduc")))
model21_REGIONSecondEdu <- lm(log(HWAGE21) ~ relevel(factor(REGION21), ref="Switzerland") + SEX21 + AGE21 + I(AGE21^2), data = subset(SHP21, EDU21 == "SecondaryEduc"))
stargazer(model21_REGIONSecondEdu, type = "text")

summary(lm(log(HWAGE21) ~ relevel(factor(REGION21), ref="Switzerland") + SEX21 + AGE21 + I(AGE21^2), data = subset(SHP21, EDU21 == "TertiaryEduc")))
model21_REGIONTertiEdu <- lm(log(HWAGE21) ~ relevel(factor(REGION21), ref="Switzerland") + SEX21 + AGE21 + I(AGE21^2), data = subset(SHP21, EDU21 == "TertiaryEduc"))
stargazer(model21_REGIONTertiEdu, type = "text")
