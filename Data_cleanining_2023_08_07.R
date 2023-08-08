install.packages("funModeling")
library(Rtools)
library(readxl)
library(dplyr)
library(funModeling)

# Set the file path and name of the .xlsx file -------
data_path <- "C:/Users/AndreaSanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/Meta_data_2023.06.05.xlsx"

# Use the read_excel() function to read the data into a data frame
data <- read_excel(data_path, sheet = "meta_PCC_votecounting")
data <- data[-1,]


unique(data$limitation, incomparables = FALSE)

#### --- Filter rows for PCC meta-analysis ----
sort(unique(data$effect_size_type))
sort(unique(data$limitation))
str(data)
names(data)
data_PCC<- data%>%
  filter(effect_size_type=="partial correlation")%>%
  filter(limitation == "interaction effect were included")

#### ---- Filter Adoption papers ----
sort(unique(data_PCC$y_metric_recla))
sort(unique(data_PCC$y_metric_recla_2))

table(data_PCC$y_metric_recla)
table(data_PCC$y_metric_recla_2)
names(data_PCC)

data_adoption<- data_PCC%>%
  filter(y_metric_recla_2=="adoption")

length(sort(unique(data_adoption$id))) # Number of articles 84
table(data_adoption$y_metric_recla)

### Pre-processing ----
data_adoption_clean<- data_adoption%>%
  #Convert to numeric the necessary columns
  mutate(coefficient_num= as.numeric(coefficient),
         variance_value_num= as.numeric(variance_value),
         variance_value = as.character(variance_value),
         z_t_value_num= as.numeric(z_t_value),
         p_value_num = as.numeric(p_value),
         n_predictors_num= as.numeric(n_predictors),
         n_samples_num= as.numeric(n_samples),
         country = as.character(country))%>%
  #Select only the columns that you are going to use
  dplyr::select(id,model_id,main_crop, intervention_recla,intervention_recla_detail_1,
                intervention_recla_detail_2,intervention_recla_detail_3,
                y_metric_recla, effect_size_type,x_metric_raw,x_metric_recla, x_metric_unit,x_transformation,
                model_analysis_raw,model_method,coefficient_type, 
                coefficient, coefficient_num,
                variance_metric,variance_value,variance_value_num,
                z_t_value,z_t_value_num, p_value, p_value_num, df_original, n_predictors,n_predictors_num,
                n_samples,n_samples_num, country, limitation_of_use)

str(data_adoption_clean)
### Factors cleaning ----
sort(unique(data_adoption_clean$x_metric_recla))

articles_count <- data_adoption_clean %>%
  group_by(x_metric_recla) %>%
  summarise(n_articles = n_distinct(id))

####### FARMER CHARACTERISTICS -------
##### Socio-demographic ------
#"hh age"
#"hh gender"
#"hh education"
#"h size"

## Household head Age ----
hh_age<- data_adoption_clean%>%
  filter(x_metric_recla== "hh age")

length(sort(unique(hh_age$id))) # Number of articles 56
sort(unique(hh_age$x_metric_unit))
#[1] "(years)^2"                                                 "1= 20–30 yrs, 2= 30–40 yrs, 3= 40–50 yrs, 4= above 50 yrs"
#[3] "sqrt(years)"                                               "years"                                                    
#[5] "years * (10^-2)" 
table(hh_age$x_metric_unit)

# Change the  x_metric_unit_recla
hh_age$x_metric_unit_recla[hh_age$x_metric_recla %in% "hh age"] <- hh_age$x_metric_unit

# Change factor name
hh_age$factor[hh_age$x_metric_recla %in% "hh age"] <- "hh age"

# Factor_metric_unit
hh_age$factor_metric_unit[hh_age$x_metric_recla %in% "hh age"] <- 
  paste(hh_age$factor, " (", hh_age$x_metric_unit_recla, ")", sep="")

sort(unique(hh_age$factor_metric_unit))
str(hh_age)
sort(unique(hh_age$x_metric_raw))

write.csv(hh_age, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_age.csv", row.names=FALSE)

## Household head Gender ----
hh_gender<- data_adoption_clean%>%
  filter(x_metric_recla== "hh gender")

length(sort(unique(hh_gender$id))) # Number of articles 49
sort(unique(hh_gender$x_metric_unit))
#"1= female, 0= male" "1= male, 0= female" "1= male, 2= female" "nd" 
table(hh_gender$x_metric_unit)

# Multiply "1= female, 0= male" AND "1= male, 2= female" * -1 to convert to "1= male, 0= female"
hh_gender$coefficient_num[hh_gender$x_metric_unit %in% c("1= female, 0= male", "1= male, 2= female")] <- 
  hh_gender$coefficient_num[hh_gender$x_metric_unit %in% c("1= female, 0= male", "1= male, 2= female")] * -1

# Convert "1= female, 0= male" and "1= male, 2= female" to "1= male, 0= female"
hh_gender$x_metric_unit_recla[hh_gender$x_metric_recla %in% "hh gender"] <- hh_gender$x_metric_unit
hh_gender$x_metric_unit_recla[hh_gender$x_metric_unit %in% c("1= male, 0= female","1= female, 0= male", "1= male, 2= female")] <- "1= male, 0= female"

# Change factor name
hh_gender$factor[hh_gender$x_metric_recla %in% "hh gender"] <- "hh gender"

# Factor_metric_unit
hh_gender$factor_metric_unit[hh_gender$x_metric_recla %in% "hh gender"] <- 
  paste(hh_gender$factor, " (", hh_gender$x_metric_unit_recla, ")", sep="")

sort(unique(hh_gender$factor_metric_unit))
str(hh_gender)
sort(unique(hh_gender$x_metric_raw))

write.csv(hh_gender, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_gender.csv", row.names=FALSE)

## Household head Education ----
hh_education<- data_adoption_clean%>%
  filter(x_metric_recla== "hh education")

length(sort(unique(hh_education$id))) # Number of articles 68
sort(unique(hh_education$x_metric_unit))
#[1] "0= no formal education, 1= non-formal vocational training, 2= primary school,  3= secondary school, 4= post-secondary."                                                                     
#[2] "0= no, 1= minimal, 2= moderate and 3= high"                                                                                                                                                 
#[3] "0= none, 1= primary, 2= secondary, 3= tertiary"                                                                                                                                             
#[4] "1= at least primary education, 0= no formal education"                                                                                                                                      
#[5] "1= Basic education, 0= no education"                                                                                                                                                        
#[6] "1= Basic, 2= High School, 3= Graduate, 4= Post-graduate"                                                                                                                                    
#[7] "1= college education, 0= no"                                                                                                                                                                
#[8] "1= college education, 0= otherwise"                                                                                                                                                         
#[9] "1= educated, 0= otherwise"                                                                                                                                                                  
#[10] "1= formal schooling, 0= no formal schooling"                                                                                                                                                
#[11] "1= high school level education, 0=otherwise"                                                                                                                                                
#[12] "1= household head had at least but no more than primary level of education, 0= no formal schooling"                                                                                         
#[13] "1= household head had secondary level (or higher) of formal education, 0= otherwise"                                                                                                        
#[14] "1= if farmer completed secondary education or more; 0= otherwise"                                                                                                                           
#[15] "1= illiterate, 2= primary, 3= secondary, 4= tertiary"                                                                                                                                       
#[16] "1= illiterate; 2= can read and write; 3= primary school (primary 1–5); 4= primary school (primary 6–7); 5= junior high school; 6= high school; 7= vocational education; 8= higher education"
#[17] "1= Less than high school; 2= High school; 3= Some college, 4= College degree; 5= Post-graduate degree"                                                                                      
#[18] "1= literacy campaing, 0= 1-2 years"                                                                                                                                                         
#[19] "1= literate, 0= illiterate"                                                                                                                                                                 
#[20] "1= literate, 0= otherwise"                                                                                                                                                                  
#[21] "1= Lower primary (grades 1-4), 0= otherwise"                                                                                                                                                
#[22] "1= Never attended school; 2= Elementary school; 3= Junior high-school; 4= Senior high-school;  5= Bachelor degree;  6= Other higher education"                                              
#[23] "1= No formal education; 2= Adult education; 3= Primary school; 4= Secondary school; 5= Post secondary"                                                                                      
#[24] "1= none, 2= primary, 3= ordinary level, 4= advanced level, 5= tertiary"                                                                                                                     
#[25] "1= none, 2= primary, 3= secondary, and 4= above"                                                                                                                                            
#[26] "1= post-secondary education, 0= no formal education"                                                                                                                                        
#[27] "1= Post-secondary, 0= otherwise"                                                                                                                                                            
#[28] "1= primary education, 0= no formal education"                                                                                                                                               
#[29] "1= primary education, 0= otherwise"                                                                                                                                                         
#[30] "1= primary education, 2= secondary education, 3= tertiary education"                                                                                                                        
#[31] "1= Secondary (grades 8-12), 0= otherwise"                                                                                                                                                   
#[32] "1= secondary education, 0= no formal education"                                                                                                                                             
#[33] "1= secondary, 0=otherwise"                                                                                                                                                                  
#[34] "1= Secondary/Tertiary education, 0= no education"                                                                                                                                           
#[35] "1= the respondent obtained an education level equivalent to a two-year degree or higher;  0= otherwise"                                                                                     
#[36] "1= three plus years, 0= 1-2 years"                                                                                                                                                          
#[37] "1= Upper primary (grades 5-7), 0= otherwise"                                                                                                                                                
#[38] "1=Farmers’ level of education above the secondary school; 0=otherwise"                                                                                                                      
#[39] "level"                                                                                                                                                                                      
#[40] "level (0-4)"                                                                                                                                                                                
#[41] "level (1-6)"                                                                                                                                                                                
#[42] "nd"                                                                                                                                                                                         
#[43] "Ordinal Scale (1 = Less than high-school, 6 = Graduate degree)"                                                                                                                             
#[44] "scale (1-5) 1= Some high school, no diploma to 5= graduate or professional degree"                                                                                                          
#[45] "years"  
##[1] "1= female, 0= male" "1= male, 0= female" "1= male, 2= female" "nd" 
table(hh_education$x_metric_unit)

# Change the  x_metric_unit_recla
hh_education$x_metric_unit_recla[hh_education$x_metric_recla %in% "hh education"] <- hh_education$x_metric_unit

# Change factor name
hh_education$factor[hh_education$x_metric_recla %in% "hh education"] <- "hh education"

# Factor_metric_unit
hh_education$factor_metric_unit[hh_education$x_metric_recla %in% "hh education"] <- 
  paste(hh_education$factor, " (", hh_education$x_metric_unit_recla, ")", sep="")

sort(unique(hh_education$factor_metric_unit))
str(hh_education)
(unique(hh_education$x_metric_raw))

write.csv(hh_education, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_education.csv", row.names=FALSE)

## Household size ----
h_size<- data_adoption_clean%>%
  filter(x_metric_recla== "h size")

length(sort(unique(h_size$id))) # Number of articles 39
sort(unique(h_size$x_metric_unit))
#[1] "1= <4 members, 2= 5–8 member, 3= 9–12 member, 4= above 12 member" "1= 8-15 members, 0= <8 members"                                  
#[3] "1= more than 15 members, 0= <8 members"                           "number of people" 
table(h_size$x_metric_unit)

# Change the  x_metric_unit_recla
h_size$x_metric_unit_recla[h_size$x_metric_recla %in% "h size"] <- h_size$x_metric_unit

# Change factor name
h_size$factor[h_size$x_metric_recla %in% "h size"] <- "h size"

# Factor_metric_unit
h_size$factor_metric_unit[h_size$x_metric_recla %in% "h size"] <- 
  paste(h_size$factor, " (", h_size$x_metric_unit_recla, ")", sep="")

sort(unique(h_size$factor_metric_unit))
str(h_size)
(unique(h_size$x_metric_raw))

write.csv(h_size, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_h_size.csv", row.names=FALSE)




##### Economic and financial capital ------
#"access to credit"
#"hh off-farm income" AND "hh engaged in off-farm activities"
## Access to credit ----
access_credit<- data_adoption_clean%>%
  filter(x_metric_recla== "access to credit")

length(sort(unique(access_credit$id))) # Number of articles 25
sort(unique(access_credit$x_metric_unit))

# Change the  x_metric_unit_recla
access_credit$x_metric_unit_recla[access_credit$x_metric_recla %in% "access to credit"] <- access_credit$x_metric_unit

# Change factor name
access_credit$factor[access_credit$x_metric_recla %in% "access to credit"] <- "access to credit"

# Factor_metric_unit
access_credit$factor_metric_unit[access_credit$x_metric_recla %in% "access to credit"] <- 
  paste(access_credit$factor, " (", access_credit$x_metric_unit_recla, ")", sep="")

sort(unique(access_credit$factor_metric_unit))
str(access_credit)
(unique(access_credit$x_metric_raw))

write.csv(access_credit, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_access_credit.csv", row.names=FALSE)



## Household off-farm income ----
#NOTES: ask Sarah if I should include here:
#"off-farm business as main source of income
#"reliance on external financial sources"
#"agriculture as main source of income"
off_farm_income<- data_adoption_clean%>%
  filter(x_metric_recla== "hh off-farm income"|  x_metric_recla=="hh engaged in off-farm activities")

length(sort(unique(off_farm_income$id))) # Number of articles 37
sort(unique(off_farm_income$x_metric_unit))


# Multiply "off-farm income (1= if full-time farmer; 0= otherwise)" * -1 to convert to "1= off-farm income, 0= full-time farmer"
off_farm_income$coefficient_num[off_farm_income$x_metric_unit %in% c("1= if full-time farmer; 0= otherwise")] <- 
  off_farm_income$coefficient_num[off_farm_income$x_metric_unit %in% c("1= if full-time farmer; 0= otherwise")] * -1


# Change the  x_metric_unit_recla
off_farm_income$x_metric_unit_recla <- off_farm_income$x_metric_unit

#Change to "percentage of income from off-farm activities"
"Percent share of nonfarm income to total household income"                                                       
"percentage of household income"                                                                                  
"Percentage of household income from off-farm employment (1 = <20%, 2 = 20–40%, 3 = 41–60%, 4 = 61–80%, 5 = >80%)"
"percentage of non-agricultural income"

off_farm_income$x_metric_unit_recla[off_farm_income$x_metric_unit %in% c("Percent share of nonfarm income to total household income",
                                                                         "percentage of household income",
                                                                         "Percentage of household income from off-farm employment (1 = <20%, 2 = 20–40%, 3 = 41–60%, 4 = 61–80%, 5 = >80%)",
                                                                         "percentage of non-agricultural income")] <- "percentage of income from off-farm activities"
#Change to "1= yes, 0= no"
#"1= yes, 0= otherwise"
"off-farm income (1= hh has a salaried employment, 0= no)"
"off-farm income (1= salary employment, 0= no secondary income)"

off_farm_income$x_metric_unit_recla[off_farm_income$x_metric_unit %in% c("1= yes, 0= otherwise",
                                                                         "1= hh has a salaried employment, 0= no",
                                                                         "1= if full-time farmer; 0= otherwise",
                                                                         "1= salary employment, 0= no secondary income")] <- "1= yes, 0= no"

#Change to "country currency"
"naira"                                                                                                           
"Nigeria naira"                                                                                                   
"Nigerian currency (N + 0.0051 USS)"
"pesos"                                                                                                           
"SDG/man-day"                                                                                                     
"USS per month/household"
off_farm_income$x_metric_unit_recla[off_farm_income$x_metric_unit %in% c("naira",                                                                                                           
                                                                         "Nigeria naira",                                                                                                   
                                                                         "Nigerian currency (N + 0.0051 USS)",
                                                                         "pesos",                                                                                                           
                                                                         "SDG/man-day",                                                                                                     
                                                                         "USS per month/household")] <- "country currency"
# Change factor name
off_farm_income$factor <- "off-farm income"

# Factor_metric_unit
off_farm_income$factor_metric_unit <- paste(off_farm_income$factor, " (", off_farm_income$x_metric_unit_recla, ")", sep="")

sort(unique(off_farm_income$factor_metric_unit))
str(off_farm_income)
(unique(off_farm_income$x_metric_raw))
table(off_farm_income$factor_metric_unit)

write.csv(off_farm_income, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_off_farm_income.csv", row.names=FALSE)




##### Information/Social capital ------
#"hh farming experience"
#"hh association member"
# "access to agricultural training" AND "agricultural training frequency"
## Household farming experience ----
hh_farming_experience<- data_adoption_clean%>%
  filter(x_metric_recla== "hh farming experience")

length(sort(unique(hh_farming_experience$id))) # Number of articles 18
sort(unique(hh_farming_experience$x_metric_unit))
#[1] "(years)^2"                                        "1= <15 years of farming experience, 0= otherwise" "years"  
table(hh_farming_experience$x_metric_unit)

# Change the  x_metric_unit_recla
hh_farming_experience$x_metric_unit_recla[hh_farming_experience$x_metric_recla %in% "hh farming experience"] <- hh_farming_experience$x_metric_unit

# Change factor name
hh_farming_experience$factor[hh_farming_experience$x_metric_recla %in% "hh farming experience"] <- "hh farming experience"

# Factor_metric_unit
hh_farming_experience$factor_metric_unit[hh_farming_experience$x_metric_recla %in% "hh farming experience"] <- 
  paste(hh_farming_experience$factor, " (", hh_farming_experience$x_metric_unit_recla, ")", sep="")

sort(unique(hh_farming_experience$factor_metric_unit))
str(hh_farming_experience)
(unique(hh_farming_experience$x_metric_raw))

write.csv(hh_farming_experience, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_farming_experience.csv", row.names=FALSE)

## Household head association member ----
hh_association_member<- data_adoption_clean%>%
  filter(x_metric_recla== "hh association member")

length(sort(unique(hh_association_member$id))) # Number of articles 36
sort(unique(hh_association_member$x_metric_unit))

table(hh_association_member$x_metric_unit)

# Change the  x_metric_unit_recla
hh_association_member$x_metric_unit_recla[hh_association_member$x_metric_recla %in% "hh association member"] <- hh_association_member$x_metric_unit

# Change factor name
hh_association_member$factor[hh_association_member$x_metric_recla %in% "hh association member"] <- "hh association member"

# Factor_metric_unit
hh_association_member$factor_metric_unit[hh_association_member$x_metric_recla %in% "hh association member"] <- 
  paste(hh_association_member$factor, " (", hh_association_member$x_metric_unit_recla, ")", sep="")

sort(unique(hh_association_member$factor_metric_unit))
str(hh_association_member)
(unique(hh_association_member$x_metric_raw))

write.csv(hh_association_member, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_association_member.csv", row.names=FALSE)

## Agricultural training ----
agricultural_training<- data_adoption_clean%>%
  filter(x_metric_recla== "access to agricultural training"| 
           x_metric_recla== "agricultural training frequency")

length(sort(unique(agricultural_training$id))) # Number of articles 13
sort(unique(agricultural_training$x_metric_unit))

# Change the  x_metric_unit_recla
agricultural_training$x_metric_unit_recla[agricultural_training$x_metric_recla %in% c("access to agricultural training",
                                                                                      "agricultural training frequency")] <- agricultural_training$x_metric_unit
#Convert to "1= yes, 0= no"
"1= yes, 0= otherwise"
agricultural_training$x_metric_unit_recla[agricultural_training$x_metric_unit %in% c("1= yes, 0= otherwise" )] <- "1= yes, 0= no"

# Change factor name
agricultural_training$factor <- "access to agricultural training"

# Factor_metric_unit
agricultural_training$factor_metric_unit<- paste(agricultural_training$factor, " (", agricultural_training$x_metric_unit_recla, ")", sep="")

sort(unique(agricultural_training$factor_metric_unit))
str(agricultural_training)
(unique(agricultural_training$x_metric_raw))
table(agricultural_training$factor_metric_unit)

write.csv(agricultural_training, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_agricultural_training.csv", row.names=FALSE)



####### FARM CHARACTERISTICS -----
##### Social capital ------
#"farm labour force" AND "farm labour force (hired)" AND "farm labour force (non-hired)"
## Farm labour force ----
farm_labour<- data_adoption_clean%>%
  filter(x_metric_recla== "farm labour force"| x_metric_recla== "farm labour force (hired)"| x_metric_recla== "farm labour force (non-hired)")

length(sort(unique(farm_labour$id))) # Number of articles 17
sort(unique(farm_labour$x_metric_unit))
table(farm_labour$x_metric_unit)

# Convert to "number of people"
"Adult equivalent"                                                                                                              
"man-days per ha"                                                                                                               
"man days per ha"                                                                                                               
"man/days*ha"                                                                                                                   
"number of employees"                                                                                                           
"Number of men in agriculture"                                                                                                  
"number of people"                                                                                                              
"Number of women in agriculture"                                                                                                
"person/rai3 (1 rai=0.16 ha)"

farm_labour$x_metric_unit_recla<- farm_labour$x_metric_unit
farm_labour$x_metric_unit_recla[farm_labour$x_metric_unit %in% c("Adult equivalent" ,                                                                                                             
                                                                 "man-days per ha" ,                                                                                                              
                                                                 "man days per ha" ,                                                                                                              
                                                                 "man/days*ha" ,                                                                                                                  
                                                                 "number of employees" ,                                                                                                          
                                                                 "Number of men in agriculture",                                                                                                  
                                                                 "number of people" ,                                                                                                             
                                                                 "Number of women in agriculture",                                                                                                
                                                                 "person/rai3 (1 rai=0.16 ha)")] <- "number of people"

# Change factor name
farm_labour$factor <- "farm labour force"

# Factor_metric_unit
farm_labour$factor_metric_unit <- paste(farm_labour$factor, " (", farm_labour$x_metric_unit_recla, ")", sep="")

sort(unique(farm_labour$factor_metric_unit))
str(farm_labour)
(unique(farm_labour$x_metric_raw))

write.csv(farm_labour, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_farm_labour.csv", row.names=FALSE)



##### Physical capital ------
#"secured land tenure"
#"units of livestock" AND "livestock owned"
# "h asset"
## Land tenure security ----
land_tenure_security<- data_adoption_clean%>%
  filter(x_metric_recla== "secured land tenure")

length(sort(unique(land_tenure_security$id))) # Number of articles 35
sort(unique(land_tenure_security$x_metric_unit))
table(land_tenure_security$x_metric_unit)

# Convert to "1= secure land tenure, 0= otherwise"
#"1 = family farm, 0 = own farm"
#"1 = share croppers, 0 = own farm"

land_tenure_security$coefficient_num[land_tenure_security$x_metric_unit %in% c("1 = family farm, 0 = own farm",
                                                                               "1 = share croppers, 0 = own farm")] <- 
  land_tenure_security$coefficient_num[land_tenure_security$x_metric_unit %in% c("1 = family farm, 0 = own farm",
                                                                                 "1 = share croppers, 0 = own farm")] * -1

land_tenure_security$x_metric_unit_recla[land_tenure_security$x_metric_recla %in% "secured land tenure"] <- land_tenure_security$x_metric_unit
land_tenure_security$x_metric_unit_recla[land_tenure_security$x_metric_unit %in% c("1 = family farm, 0 = own farm",
                                                                                   "1 = share croppers, 0 = own farm")] <- "1= secure, 0= otherwise"
# Convert to "1= secure land tenure, 0= otherwise"
"1 = family had land right certificate of rubber land, 0 = otherwise"
"1 = farmer has secure tenurial rights, 0 = otherwise."
"1= Fully-owned; 0=partially-owned, shared and leased"
"1= if a household head feels “secured”, 0= otherwise"
"1= land owned, 0= otherwise"                                                                                   
"1= land title, 0=otherwise"
"1= owned, 0= no"                                                                                               
"1= owner, 0= otherwise"                                                                                        
"1= Private ownership through clearance, 0= otherwise"
"1= purchase, 0= otherwise"
"1= single family, 0= otherwise"
"1= yes, 0= no"                                                                                                 
"1= yes, 0= otherwise"                                                                                          
"1=owned, 0=rented"                                                                                             
"1=owned, 0=rented in"
land_tenure_security$x_metric_unit_recla[land_tenure_security$x_metric_unit %in% c("1 = family had land right certificate of rubber land, 0 = otherwise",
                                                                                   "1 = farmer has secure tenurial rights, 0 = otherwise.",
                                                                                   "1= Fully-owned; 0=partially-owned, shared and leased",
                                                                                   "1= if a household head feels “secured”, 0= otherwise",
                                                                                   "1= land owned, 0= otherwise"   ,                                                                                
                                                                                   "1= land title, 0=otherwise",
                                                                                   "1= owned, 0= no"    ,                                                                                           
                                                                                   "1= owner, 0= otherwise"  ,                                                                                      
                                                                                   "1= Private ownership through clearance, 0= otherwise",
                                                                                   "1= purchase, 0= otherwise",
                                                                                   "1= single family, 0= otherwise",
                                                                                   "1= yes, 0= no"       ,                                                                                          
                                                                                   "1= yes, 0= otherwise"  ,                                                                                        
                                                                                   "1=owned, 0=rented" ,                                                                                            
                                                                                   "1=owned, 0=rented in")] <- "1= secure, 0= otherwise"
# Convert to "percentage to secure land"
"percentage of farm acres owned"                                                                                
"percentage of total landholding that is owned"                                                                 
"proportion"                                                                                                    
"proportion of cultivated area with secure tenure"
land_tenure_security$x_metric_unit_recla[land_tenure_security$x_metric_unit %in% c("percentage of farm acres owned",                                                                                
                                                                                   "percentage of total landholding that is owned",                                                                 
                                                                                   "proportion" ,                                                                                                   
                                                                                   "proportion of cultivated area with secure tenure")] <- "percentage to secure land"

# Change factor name
land_tenure_security$factor[land_tenure_security$x_metric_recla %in% "secured land tenure"] <- "secured land tenure"

# Factor_metric_unit
land_tenure_security$factor_metric_unit[land_tenure_security$x_metric_recla %in% "secured land tenure"] <- 
  paste(land_tenure_security$factor, " (", land_tenure_security$x_metric_unit_recla, ")", sep="")

#NOTES land tenure security
# 607 excluded: Not clear which is most secure tenure system
# 750 excluded: Not clear which is most secure tenure system
sort(unique(land_tenure_security$factor_metric_unit))
str(land_tenure_security)
(unique(land_tenure_security$x_metric_raw))

write.csv(land_tenure_security, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_land_tenure_security.csv", row.names=FALSE)

## Livestock ownership ----
livestock_ownership<- data_adoption_clean%>%
  filter(x_metric_recla== "units of livestock" | x_metric_recla== "livestock owned")

length(sort(unique(livestock_ownership$id))) # Number of articles 25
sort(unique(livestock_ownership$x_metric_unit))

#Convert  "TLU (10^-1)" to TLU
livestock_ownership$coefficient_num[livestock_ownership$x_metric_unit %in% "TLU (10^-1)"] <- 
  livestock_ownership$coefficient_num[livestock_ownership$x_metric_unit %in% "TLU (10^-1)"] * 10^-1

livestock_ownership$variance_value_num[livestock_ownership$x_metric_unit %in% "TLU (10^-1)"&
                                         livestock_ownership$variance_metric %in% c("standard error", "robust standard error")] <- 
  livestock_ownership$variance_value_num[livestock_ownership$x_metric_unit %in% "TLU (10^-1)"&
                                           livestock_ownership$variance_metric %in% c("standard error", "robust standard error")] * 10^-1

# Change the  x_metric_unit_recla
livestock_ownership$x_metric_unit_recla[livestock_ownership$x_metric_recla %in% c("units of livestock", "livestock owned")] <- livestock_ownership$x_metric_unit

# Convert to "TLU"
livestock_ownership$x_metric_unit_recla[livestock_ownership$x_metric_unit %in% c("TLU (10^-1)", "TLU/ha")] <- "TLU"

# Convert to "number of animals owned"
"livestock units"
"number"
"number of cattle"
"number of cattle heads per hectare" 
"number of goats and sheep per hectare"
"number of livestock owned"                                                    
"number of other cattle"
"number of oxen"
"number of pack animals"
"number of small rumiants"

livestock_ownership$x_metric_unit_recla[livestock_ownership$x_metric_unit %in% c("livestock units",
                                                                                 "number",
                                                                                 "number of cattle",
                                                                                 "number of cattle heads per hectare", 
                                                                                 "number of goats and sheep per hectare",
                                                                                 "number of livestock owned",                                                    
                                                                                 "number of other cattle",
                                                                                 "number of oxen",
                                                                                 "number of pack animals",
                                                                                 "number of small rumiants")] <- "Units of animal owned"

# Convert to "1= yes, 0= no"
"1= ownership, 0= otherwise"
"1=Farmers’ house holds owned draught cattle for crop production, 0= otherwise"
livestock_ownership$x_metric_unit_recla[livestock_ownership$x_metric_unit %in% c("1= ownership, 0= otherwise",
                                                                                 "1=Farmers’ house holds owned draught cattle for crop production, 0= otherwise")] <- "1= yes, 0= no"

# Change factor name
livestock_ownership$factor <- "livestock ownership"

# Factor_metric_unit
livestock_ownership$factor_metric_unit<- paste(livestock_ownership$factor, " (", livestock_ownership$x_metric_unit_recla, ")", sep="")

sort(unique(livestock_ownership$factor_metric_unit))
str(livestock_ownership)
(unique(livestock_ownership$x_metric_raw))
table(livestock_ownership$factor_metric_unit)

write.csv(livestock_ownership, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_livestock_ownership.csv", row.names=FALSE)

## Household asset ----
h_asset<- data_adoption_clean%>%
  filter(x_metric_recla== "h asset")

length(sort(unique(h_asset$id))) # Number of articles 19
sort(unique(h_asset$x_metric_unit))

#Convert to USD
"Tsh (1US$ = 1,255 Tsh)"
h_asset$coefficient_num[h_asset$x_metric_unit %in% "Tsh (1US$ = 1,255 Tsh)"] <- 
  h_asset$coefficient_num[h_asset$x_metric_unit %in% "Tsh (1US$ = 1,255 Tsh)"] / 1255

h_asset$variance_value_num[h_asset$x_metric_unit %in% "Tsh (1US$ = 1,255 Tsh)"&
                             h_asset$variance_metric %in% c("standard error", "robust standard error")] <- 
  h_asset$variance_value_num[h_asset$x_metric_unit %in% "Tsh (1US$ = 1,255 Tsh)"&
                               h_asset$variance_metric %in% c("standard error", "robust standard error")] /1255

#"USD (10-3)"
h_asset$coefficient_num[h_asset$x_metric_unit %in% "USD (10-3)"] <- 
  h_asset$coefficient_num[h_asset$x_metric_unit %in% "USD (10-3)"] *10^-3

h_asset$variance_value_num[h_asset$x_metric_unit %in% "USD (10-3)"&
                             h_asset$variance_metric %in% c("standard error", "robust standard error")] <- 
  h_asset$variance_value_num[h_asset$x_metric_unit %in% "USD (10-3)"&
                               h_asset$variance_metric %in% c("standard error", "robust standard error")] *10^-3

#"USS 1 = GHC 3.924 (Ghanian currency)"
h_asset$coefficient_num[h_asset$x_metric_unit %in% "USS 1 = GHC 3.924 (Ghanian currency)"] <- 
  h_asset$coefficient_num[h_asset$x_metric_unit %in% "USS 1 = GHC 3.924 (Ghanian currency)"] /3.924

h_asset$variance_value_num[h_asset$x_metric_unit %in% "USS 1 = GHC 3.924 (Ghanian currency)"&
                             h_asset$variance_metric %in% c("standard error", "robust standard error")] <- 
  h_asset$variance_value_num[h_asset$x_metric_unit %in% "USS 1 = GHC 3.924 (Ghanian currency)"&
                               h_asset$variance_metric %in% c("standard error", "robust standard error")] /3.924

# Change the  x_metric_unit_recla
h_asset$x_metric_unit_recla[h_asset$x_metric_recla %in% c("h asset")] <- h_asset$x_metric_unit

# Convert to "USD"
h_asset$x_metric_unit_recla[h_asset$x_metric_unit %in% c("Tsh (1US$ = 1,255 Tsh)", "USD (10-3)","USS 1 = GHC 3.924 (Ghanian currency)" )] <- "USD"

# Change factor name
h_asset$factor <- "h asset"

# Factor_metric_unit
h_asset$factor_metric_unit<- paste(h_asset$factor, " (", h_asset$x_metric_unit_recla, ")", sep="")

sort(unique(h_asset$factor_metric_unit))
str(h_asset)
(unique(h_asset$x_metric_raw))
table(h_asset$factor_metric_unit)

write.csv(h_asset, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_h_asset.csv", row.names=FALSE)





##### Biophysical ------
#"farm size"
#"soil slope" AND "hh perception of farm slope"
## Farm/Plot slope ----
slope<- data_adoption_clean%>%
  filter(x_metric_recla== "hh perception of farm slope" |  x_metric_recla=="soil slope")

length(sort(unique(slope$id))) # Number of articles 20
sort(unique(slope$x_metric_unit))

# Convert to "1= steep slope, 0= otherwise"
"1= flat, 0= steep slope" 
slope$coefficient_num[slope$x_metric_unit %in% "1= flat, 0= steep slope"] <- 
  slope$coefficient_num[slope$x_metric_unit %in% "1= flat, 0= steep slope"] * -1

# Change the  x_metric_unit_recla
slope$x_metric_unit_recla <- slope$x_metric_unit

# Convert to "1= steep slope, 0= otherwise"
"1= flat, 0= steep slope"
"1= steep slope, 0= no"
"1= steep slope, 0= otherwise"
"1= steep, 0= no"
"1= step, 0= otherwise"
slope$x_metric_unit_recla[slope$x_metric_unit %in% c("1= flat, 0= steep slope",
                                                     "1= steep slope, 0= no",
                                                     "1= steep slope, 0= otherwise",
                                                     "1= steep, 0= no",
                                                     "1= step, 0= otherwise")] <- "1= steep slope, 0= otherwise"

# Convert to "1= moderate slope, 0= otherwise"
"1 = moderate, 0 = No"
"1= moderate slope, 0= otherwise"                                                                                    
"1= moderate, 0= no"                                                                                                 
"1= moderate, 0= otherwise"                                                                                          
"1= moderately, 0= steep slope"

slope$x_metric_unit_recla[slope$x_metric_unit %in% c("1= moderate slope, 0= otherwise",                                                                                    
                                                     "1= moderate, 0= no",    
                                                     "1 = moderate, 0 = No",
                                                     "1= moderate, 0= otherwise" ,                                                                                         
                                                     "1= moderately, 0= steep slope")] <- "1= moderate slope, 0= otherwise"

# Convert to "1= medium slope, 0= otherwise"
"1= medium slope plot, 0= otherwise"                                                                                 
"1= medium slope, 0= otherwise"                                                                                      
"1= middle slope, upslope"

slope$x_metric_unit_recla[slope$x_metric_unit %in% c("1= medium slope plot, 0= otherwise",                                                                                 
                                                     "1= medium slope, 0= otherwise",                                                                                      
                                                     "1= middle slope, upslope")] <- "1= medium slope, 0= otherwise"


slope$x_metric_unit_recla[slope$x_metric_unit %in% c("percent", "percentage")] <- "percentage"

# Change factor name
slope$factor <- "farm/plot slope"

# Factor_metric_unit
slope$factor_metric_unit<- paste(slope$factor, " (", slope$x_metric_unit_recla, ")", sep="")

sort(unique(slope$factor_metric_unit))
str(slope)
table(slope$factor_metric_unit)

(unique(slope$x_metric_raw))

write.csv(slope, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_farm_slope.csv", row.names=FALSE)


## Farm size ----
farm_size<- data_adoption_clean%>%
  filter(x_metric_recla== "farm size")

length(sort(unique(farm_size$id))) # Number of articles 53
sort(unique(farm_size$x_metric_unit))
#[1] "(ha)^2"                                                             "1= <2 ha; 2= 2–3.5 ha; 3= 3.5–5 ha; 4= more than 5 ha"             
#[3] "1= <250” acres, 2= 250–749 acres, 3= 750–1449 acres, 4: 1500 acres" "1= >0.50 ha; 0= < 0.50 ha"                                         
#[5] "1= 100-399 fa, 0= <100 fa (1fa=0.42 ha)"                            "1= 1000 acres, 0=otherwise"                                        
#[7] "1= 400-1000 fa; 0= <100 fa (1fa=0.42 ha)"                           "1= Farmers who owned land below 10 ac (4.05 ha); 0=otherwise"      
#[9] "1= more than 1000 fa; 0= <100 fa (1fa=0.42 ha), 0= otherwise"       "1= squared(1000 acres), 0=otherwise"                               
#[11] "acres"                                                              "ha"                                                                
#[13] "ktha (30 ktha=1ha)"                                                 "Mukhamas (1Mukhamas= 0.73 ha)"                                     
#[15] "rai (1 rai = 0.16 ha)" 
table(farm_size$x_metric_unit)

# Convert "acres" to "ha"
farm_size$coefficient_num[farm_size$x_metric_unit %in% "acres"] <- 
  farm_size$coefficient_num[farm_size$x_metric_unit %in% "acres"] * 0.404686

farm_size$variance_value_num[farm_size$x_metric_unit %in% "acres"&
                               farm_size$variance_metric %in% c("standard error", "robust standard error")] <- 
  farm_size$variance_value_num[farm_size$x_metric_unit %in% "acres"&
                                 farm_size$variance_metric %in% c("standard error", "robust standard error")] * 0.404686

# Convert "ktha (30 ktha=1ha)" to "ha"
farm_size$coefficient_num[farm_size$x_metric_unit %in% "ktha (30 ktha=1ha)"] <- 
  farm_size$coefficient_num[farm_size$x_metric_unit %in% "ktha (30 ktha=1ha)"] /30

farm_size$variance_value_num[farm_size$x_metric_unit %in% "ktha (30 ktha=1ha)"&
                               farm_size$variance_metric %in% c("standard error", "robust standard error")] <- 
  farm_size$variance_value_num[farm_size$x_metric_unit %in% "ktha (30 ktha=1ha)"&
                                 farm_size$variance_metric %in% c("standard error", "robust standard error")] /30

# Convert "rai (1 rai = 0.16 ha)" to "ha"
farm_size$coefficient_num[farm_size$x_metric_unit %in% "rai (1 rai = 0.16 ha)"] <- 
  farm_size$coefficient_num[farm_size$x_metric_unit %in% "rai (1 rai = 0.16 ha)"] *0.16

farm_size$variance_value_num[farm_size$x_metric_unit %in% "rai (1 rai = 0.16 ha)"&
                               farm_size$variance_metric %in% c("standard error", "robust standard error")] <- 
  farm_size$variance_value_num[farm_size$x_metric_unit %in% "rai (1 rai = 0.16 ha)"&
                                 farm_size$variance_metric %in% c("standard error", "robust standard error")] *0.16

# Convert "Mukhamas (1Mukhamas= 0.73 ha)" to "ha"
farm_size$coefficient_num[farm_size$x_metric_unit %in% "Mukhamas (1Mukhamas= 0.73 ha)"] <- 
  farm_size$coefficient_num[farm_size$x_metric_unit %in% "Mukhamas (1Mukhamas= 0.73 ha)"] *0.73

farm_size$variance_value_num[farm_size$x_metric_unit %in% "Mukhamas (1Mukhamas= 0.73 ha)"&
                               farm_size$variance_metric %in% c("standard error", "robust standard error")] <- 
  farm_size$variance_value_num[farm_size$x_metric_unit %in% "Mukhamas (1Mukhamas= 0.73 ha)"&
                                 farm_size$variance_metric %in% c("standard error", "robust standard error")] *0.73

# Change the  x_metric_unit_recla
farm_size$x_metric_unit_recla <- farm_size$x_metric_unit

farm_size$x_metric_unit_recla[farm_size$x_metric_unit %in% c("acres", "ktha (30 ktha=1ha)",
                                                               "rai (1 rai = 0.16 ha)","Mukhamas (1Mukhamas= 0.73 ha)")] <- "ha"

# Change factor name
farm_size$factor <- "farm size"

# Factor_metric_unit
farm_size$factor_metric_unit <- paste(farm_size$factor, " (", farm_size$x_metric_unit_recla, ")", sep="")

sort(unique(farm_size$factor_metric_unit))
str(farm_size)
(unique(farm_size$x_metric_raw))

write.csv(farm_size, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_farm_size.csv", row.names=FALSE)



####### CONTEXT CHARACTERISTICS -----

##### Information ------
#"agricultural extension"
## Access to agricultural extension ----
agricultural_extension <- data_adoption_clean%>%
  filter(x_metric_recla== "access to agricultural extension"|x_metric_recla== "agricultural extension frequency")

length(sort(unique(agricultural_extension$id))) # Number of articles 40
sort(unique(agricultural_extension$x_metric_unit))

table(agricultural_extension$x_metric_unit)

# Change the  x_metric_unit_recla
agricultural_extension$x_metric_unit_recla[agricultural_extension$x_metric_recla %in% 
                                             c("access to agricultural extension", "agricultural extension frequency")] <- agricultural_extension$x_metric_unit

# Convert to "number of contacts"
"(number of visits)^2"
"days per month"                                                                                                                                                   
"number of contacts"                                                                                                                                               
"number of contacts with extension staff in 2015"                                                                                                                  
"number of visits"                                                                                                                                                 
"number of visits by extension agents"

agricultural_extension$x_metric_unit_recla[agricultural_extension$x_metric_unit %in% c("(number of visits)^2",
                                                                                       "days per month",                                                                                                                                                   
                                                                                       "number of contacts" ,                                                                                                                                              
                                                                                       "number of contacts with extension staff in 2015",                                                                                                                  
                                                                                       "number of visits" ,                                                                                                                                                
                                                                                       "number of visits by extension agents")] <- "number of contacts"

# Change factor name
agricultural_extension$factor[agricultural_extension$x_metric_recla %in% c("access to agricultural extension", "agricultural extension frequency")] <- "agricultural extension"

# Factor_metric_unit
agricultural_extension$factor_metric_unit[agricultural_extension$x_metric_recla %in% c("access to agricultural extension", "agricultural extension frequency")] <- 
  paste(agricultural_extension$factor, " (", agricultural_extension$x_metric_unit_recla, ")", sep="")

sort(unique(agricultural_extension$factor_metric_unit))
str(agricultural_extension)
(unique(agricultural_extension$x_metric_raw))
table(agricultural_extension$factor_metric_unit)


write.csv(agricultural_extension, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_agricultural_extension.csv", row.names=FALSE)


##### Physical capital ------
#"distance to market" AND "distance to input market"
#"distance to road"

## Distance to market AND Distance to input market ----
distance_market<- data_adoption_clean%>%
  filter(x_metric_recla== "distance to market" | x_metric_recla=="distance to input market")

length(sort(unique(distance_market$id))) # Number of articles 19
sort(unique(distance_market$x_metric_unit))
#[1] "1= 1 to 3 km, 0= less than 1 km"                                                                 
#[2] "1= Farmers’ house holds that could reach market center within 13 km (about 8 miles), 0=otherwise"
#[3] "km"                                                                                              
#[4] "miles"                                                                                           
#[5] "minutes"                                                                                         
#[6] "minutes (10^-2)"                                                                                 
#[7] "number of city visits during the last year"
table(distance_market$x_metric_unit)

# Convert "miles" to "km"
distance_market$coefficient_num[distance_market$x_metric_recla %in% c("distance to market", "distance to input market") &
                                  distance_market$x_metric_unit %in% "miles"] <- 
  distance_market$coefficient_num[distance_market$x_metric_recla %in% c("distance to market", "distance to input market") &
                                    distance_market$x_metric_unit %in% "miles"] * 1.60934

distance_market$variance_value_num[distance_market$x_metric_recla %in% c("distance to market", "distance to input market") &
                                     distance_market$x_metric_unit %in% "miles"&
                                     distance_market$variance_metric %in% c("standard error", "robust standard error")] <- 
  distance_market$variance_value_num[distance_market$x_metric_recla %in% c("distance to market", "distance to input market") &
                                       distance_market$x_metric_unit %in% "miles"&
                                       distance_market$variance_metric %in% c("standard error", "robust standard error")] * 1.60934

# Change the  x_metric_unit_recla
distance_market$x_metric_unit_recla[distance_market$x_metric_recla %in% c("distance to market", "distance to input market")] <- distance_market$x_metric_unit

distance_market$x_metric_unit_recla[distance_market$x_metric_recla %in% c("distance to market", "distance to input market") & 
                                      distance_market$x_metric_unit %in% c("miles")] <- "km"

# Change factor name
distance_market$factor[distance_market$x_metric_recla %in% c("distance to market", "distance to input market")] <- "distance to market"

# Factor_metric_unit
distance_market$factor_metric_unit[distance_market$x_metric_recla %in% c("distance to market", "distance to input market")] <- 
  paste(distance_market$factor, " (", distance_market$x_metric_unit_recla, ")", sep="")

sort(unique(distance_market$factor_metric_unit))
str(distance_market)
(unique(distance_market$x_metric_raw))

write.csv(distance_market, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_distance_market.csv", row.names=FALSE)


## Distance to road (km) ----
distance_road<- data_adoption_clean%>%
  filter(x_metric_recla== "distance to road")

length(sort(unique(distance_road$id))) # Number of articles 10
sort(unique(distance_road$x_metric_unit))
#[1] "1= if access to motor road is > 1 kilometer distance, 0= otherwise" "hour"                                                              
#[3] "km"                                                                 "metres"                                                            
#[5] "miles"                                                              "minutes"

table(distance_road$x_metric_unit)

# Convert "miles" to "km"
distance_road$coefficient_num[distance_road$x_metric_unit %in% "miles"] <- 
  distance_road$coefficient_num[distance_road$x_metric_unit %in% "miles"] * 1.60934

distance_road$variance_value_num[distance_road$x_metric_unit %in% "miles"&
                                   distance_road$variance_metric %in% c("standard error", "robust standard error")] <- 
  distance_road$variance_value_num[distance_road$x_metric_unit %in% "miles"&
                                     distance_road$variance_metric %in% c("standard error", "robust standard error")] * 1.60934

# Change the  x_metric_unit_recla
distance_road$x_metric_unit_recla <- distance_road$x_metric_unit

distance_road$x_metric_unit_recla[distance_road$x_metric_unit %in% c("miles")] <- "km"

# Convert "metres" to "km"
distance_road$coefficient_num[distance_road$x_metric_unit %in% "metres"] <- 
  distance_road$coefficient_num[distance_road$x_metric_unit %in% "metres"] / 1000 

distance_road$variance_value_num[distance_road$x_metric_unit %in% "metres"&
                                   distance_road$variance_metric %in% c("standard error", "robust standard error")] <- 
  distance_road$variance_value_num[distance_road$x_metric_unit %in% "metres"&
                                     distance_road$variance_metric %in% c("standard error", "robust standard error")] / 1000

# Change the  x_metric_unit_recla
distance_road$x_metric_unit_recla[distance_road$x_metric_unit %in% c("metres")] <- "km"

# Convert "hours" to "minutes"
distance_road$coefficient_num[distance_road$x_metric_unit %in% "hour"] <- 
  distance_road$coefficient_num[distance_road$x_metric_unit %in% "hour"] *60

distance_road$variance_value_num[distance_road$x_metric_unit %in% "hour"&
                                   distance_road$variance_metric %in% c("standard error", "robust standard error")] <- 
  distance_road$variance_value_num[distance_road$x_metric_unit %in% "hour"&
                                     distance_road$variance_metric %in% c("standard error", "robust standard error")] *60

# Change the  x_metric_unit_recla
distance_road$x_metric_unit_recla[distance_road$x_metric_unit %in% c("hour")] <- "minutes"

# Change factor name
distance_road$factor <- "distance to road"

# Factor_metric_unit
distance_road$factor_metric_unit <- paste(distance_road$factor, " (", distance_road$x_metric_unit_recla, ")", sep="")

sort(unique(distance_road$factor_metric_unit))
str(distance_road)
(unique(distance_road$x_metric_raw))

write.csv(distance_road, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_distance_road.csv", row.names=FALSE)





