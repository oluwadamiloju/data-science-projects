# reading the csv file
library(tidyverse)
file <- read_csv("C:/Sarah's Semicolon files/data-science/project-1/master.csv")

file <-select(file, -"country-year")


#categorizing into smaller dataframes per country
# united states of america 
us_df <- file%>%.[26849:27220, ]
summary(us_df)
# denmark
denmark_df <- file%>%.[7419:7682, ]
# south africa
sa_df <- file%>%.[23289:23528, ]
# norway 
nw_df <- file%>%.[17869:18228, ]
# russian federation
rs_df <- file%>%.[20937:21258, ]
# japan
jp_df <- file%>%.[13365:13736, ]
# brazil
br_df <- file%>%.[4173:4544, ]
# austria
au_df <- file%>%.[1787:2168, ]


# chi-square test on us data between suicide numbers and age in usa
chisq.test(us_df$`suicides/100k pop`, us_df$age, correct=FALSE)

# We have a chi-squared value of 1766. Since we get a p-Value less than 
# the significance level of 0.05 (0.01916), we reject the null hypothesis and conclude 
# that the two variables are in fact dependent


# chi-square test on us data between suicide numbers and sex in usa
chisq.test(us_df$`suicides/100k pop`, us_df$sex, correct=FALSE)

# We have a chi-squared value of 370. Since we get a p-Value greater than 
# the significance level of 0.05 (0.05913), we do not reject the null hypothesis and conclude 
# that the two variables are not related


# chi-square test on us data between suicide numbers and age in denmark
chisq.test(denmark_df$`suicides/100k pop`, denmark_df$age, correct=FALSE)

# We have a chi-squared value of 1278. Since we get a p-Value less than 
# the significance level of 0.05 (0.001347), we reject the null hypothesis and conclude 
# that the two variables are in fact dependent


# chi-square test on us data between suicide numbers and sex in denmark
chisq.test(denmark_df$`suicides/100k pop`, denmark_df$sex, correct=FALSE)

# We have a chi-squared value of 236.33. Since we get a p-Value greater than 
# the significance level of 0.05 (0.305), we do not reject the null hypothesis and conclude 
# that the two variables are not related


# chi-square test on us data between suicide numbers and age in south africa
chisq.test(sa_df$`suicides/100k pop`, sa_df$age, correct=FALSE)

# We have a chi-squared value of 871. Since we get a p-Value less than 
# the significance level of 0.05 (3.115e-05), we reject the null hypothesis and conclude 
# that the two variables are in fact dependent


# chi-square test on us data between suicide numbers and sex in south africa
chisq.test(sa_df$`suicides/100k pop`, sa_df$sex, correct=FALSE)

# We have a chi-squared value of 195.19. Since we get a p-Value less than 
# the significance level of 0.05 (0.002057), we reject the null hypothesis and conclude 
# that the two variables are in fact dependent


# chi-square test on us data between suicide numbers and age in norway
chisq.test(nw_df$`suicides/100k pop`, nw_df$age, correct=FALSE)

# We have a chi-squared value of 1732. Since we get a p-Value less than 
# the significance level of 0.05 (0.005448), we reject the null hypothesis and conclude 
# that the two variables are in fact dependent


# chi-square test on us data between suicide numbers and sex in norway
chisq.test(nw_df$`suicides/100k pop`, nw_df$sex, correct=FALSE)

# We have a chi-squared value of 350.14. Since we get a p-Value greater than 
# the significance level of 0.05 (0.09693), we reject the null hypothesis and conclude 
# that the two variables are not related


# chi-square test on us data between suicide numbers and age in russia federation
chisq.test(rs_df$`suicides/100k pop`, rs_df$age, correct=FALSE)

# We have a chi-squared value of 1592.1. Since we get a p-Value more than 
# the significance level of 0.05 (0.3108), we accept the null hypothesis and conclude 
# that the two variables are not related


# chi-square test on us data between suicide numbers and sex in russia federation
chisq.test(rs_df$`suicides/100k pop`, rs_df$sex, correct=FALSE)

# We have a chi-squared value of 322. Since we get a p-Value more than 
# the significance level of 0.05 (0.3509), we accept the null hypothesis and conclude 
# that the two variables are not related


# chi-square test on us data between suicide numbers and age in japan
chisq.test(jp_df$`suicides/100k pop`, jp_df$age, correct=FALSE)

# We have a chi-squared value of 1804. Since we get a p-Value less than 
# the significance level of 0.05 (0.04697), we reject the null hypothesis and conclude 
# that the two variables are in fact dependent


# chi-square test on us data between suicide numbers and sex in japan
chisq.test(jp_df$`suicides/100k pop`, jp_df$sex, correct=FALSE)

# We have a chi-squared value of 347. Since we get a p-Value greater than 
# the significance level of 0.05 (0.3998), we do not reject the null hypothesis and conclude 
# that the two variables are not related


# chi-square test on us data between suicide numbers and age in brazil
chisq.test(br_df$`suicides/100k pop`, br_df$age, correct=FALSE)

# We have a chi-squared value of 1561. Since we get a p-Value less than 
# the significance level of 0.05 (1.611e-07), we reject the null hypothesis and conclude 
# that the two variables are in fact dependent


# chi-square test on us data between suicide numbers and sex in brazil
chisq.test(br_df$`suicides/100k pop`, br_df$sex, correct=FALSE)

# We have a chi-squared value of 329.87. Since we get a p-Value less than 
# the significance level of 0.05 (0.001436), we reject the null hypothesis and conclude 
# that the two variables are in fact dependent


# chi-square test on us data between suicide numbers and age in austria
chisq.test(au_df$`suicides/100k pop`, au_df$age, correct=FALSE)

# We have a chi-squared value of 1892.1. Since we get a p-Value less than 
# the significance level of 0.05 (0.001658), we reject the null hypothesis and conclude 
# that the two variables are in fact dependent


# chi-square test on us data between suicide numbers and sex in austria
chisq.test(au_df$`suicides/100k pop`, au_df$sex, correct=FALSE)

# We have a chi-squared value of 358. Since we get a p-Value greater than 
# the significance level of 0.05 (0.2776), we reject the null hypothesis and conclude 
# that the two variables are not related