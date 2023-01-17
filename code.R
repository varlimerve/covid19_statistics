# In this part, we read the data from the GitHub repository.
covid_19 <- read.csv("https://raw.githubusercontent.com/varlimerve/covid19_statistics/main/ClinicalDataOfPositivePatients_COVID-19-NY-SBU.csv")
View(covid_19)
str(covid_19)
# Create a table to see how many female and male in the dataset.
table(covid_19$gender_concept_name)
# Make a histogram of length of stay while covid-19 positive for
# both female and male.
hist(covid_19$length_of_stay)


# distribution of female
dist_female <- covid_19[(covid_19$gender_concept_name=="FEMALE"), ]
hist(dist_female$length_of_stay,
     main = "Female Length of Stay Distribution",
     xlab = "Length of Stay (day)",
    )
# Remove NA from dataset to calculate mean for female.
dist_female_wo_na <- na.omit(dist_female)
mean(dist_female_wo_na$length_of_stay)
# Distribution of male
dist_male <- covid_19[(covid_19$gender_concept_name=="MALE"),]
hist(dist_male$length_of_stay,
     main = "Male Length of Stay Distribution",
     xlab = "Length of Stay (day)",)
# Remove NA from dataset to calculate mean for male. 
dist_male_ma_na <- na.omit(dist_male)
mean(dist_male_ma_na$length_of_stay)


#Summary for the length of stay in the hospital. 
summary(covid_19$length_of_stay)


install.packages("ggplot2")
library(ggplot2)
 #barplot for deceased and discharged in range of age.      
barplot(table(covid_19$last.status))
table(covid_19$age.splits)
barplot(table(covid_19$age.splits))
table_1 <- table(covid_19$last.status, covid_19$age.splits)
addmargins(table_1)
prop.table(table_1, margin = 2)
barplot(table_1, legend.text = TRUE)
 #barplot for visitng room for both female and male. 
barplot(table(covid_19$gender_concept_name))
barplot(table(covid_19$visit_concept_name))
table_2 <- table(covid_19$gender_concept_name, covid_19$visit_concept_name)
addmargins(table_2)
prop.table(table_2, margin = 2)
barplot(table_2, legend.text = TRUE)

table_last_status_dead <- covid_19[(covid_19$last.status== "deceased"),]
summary(table_last_status_dead$Oxygen.saturation.in.Arterial.blood.by.Pulse.oximetry)
table_last_status_live <- covid_19[(covid_19$last.status== "discharged"),]
summary(table_last_status_live$Oxygen.saturation.in.Arterial.blood.by.Pulse.oximetry)

plot(x=covid_19$length_of_stay,
     y=covid_19$Oxygen.saturation.in.Arterial.blood.by.Pulse.oximetry,
     xlab = "Length of Stay",
     ylab = "Oxygen Saturation")


sd(covid_19$Oxygen.saturation.in.Arterial.blood.by.Pulse.oximetry)
sd(covid_19$length_of_stay)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(covid_19$Oxygen.saturation.in.Arterial.blood.by.Pulse.oximetry)

median(covid_19$Oxygen.saturation.in.Arterial.blood.by.Pulse.oximetry)

chisq.test(covid_19$age.splits, covid_19$last.status)
boxplot(covid_19$length_of_stay)
boxplot(dist_female_wo_na$Oxygen.saturation.in.Arterial.blood.by.Pulse.oximetry)
IQR(covid_19$length_of_stay)
summary(covid_19$length_of_stay)


table_2 <- table(covid_19$last.status, covid_19$age.splits)
