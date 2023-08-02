install.packages("funModeling")
library(Rtools)
library(readxl)
library(dplyr)
library(funModeling)

### Country-Continent list -------------
continent_list <- structure(list(country = c("Afghanistan", "Åland Islands", "Albania", 
                                             "Algeria", "American Samoa", "Andorra", "Angola", "Anguilla", 
                                             "Antarctica", "Antigua and Barbuda", "Argentina", "Armenia", 
                                             "Aruba", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", 
                                             "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", 
                                             "Bermuda", "Bhutan", "Bolivia (Plurinational State of)", "Bonaire, Sint Eustatius and Saba", 
                                             "Bosnia and Herzegovina", "Botswana", "Bouvet Island", "Brazil", 
                                             "British Indian Ocean Territory", "Brunei Darussalam", "Bulgaria", 
                                             "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Canada", 
                                             "Cabo Verde", "Cayman Islands", "Central African Republic", "Chad", 
                                             "Chile", "China", "Christmas Island", "Cocos (Keeling) Islands", 
                                             "Colombia", "Comoros", "Congo", "Congo (Democratic Republic of the)", 
                                             "Cook Islands", "Costa Rica", "Côte d'Ivoire", "Croatia", "Cuba", 
                                             "Curaçao", "Cyprus", "Czech Republic", "Denmark", "Djibouti", 
                                             "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", 
                                             "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Falkland Islands (Malvinas)", 
                                             "Faroe Islands", "Fiji", "Finland", "France", "French Guiana", 
                                             "French Polynesia", "French Southern Territories", "Gabon", "Gambia", 
                                             "Georgia", "Germany", "Ghana", "Gibraltar", "Greece", "Greenland", 
                                             "Grenada", "Guadeloupe", "Guam", "Guatemala", "Guernsey", "Guinea", 
                                             "Guinea-Bissau", "Guyana", "Haiti", "Heard Island and McDonald Islands", 
                                             "Holy See", "Honduras", "Hong Kong", "Hungary", "Iceland", "India", 
                                             "Indonesia", "Iran (Islamic Republic of)", "Iraq", "Ireland", 
                                             "Isle of Man", "Israel", "Italy", "Jamaica", "Japan", "Jersey", 
                                             "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Korea (Democratic People's Republic of)", 
                                             "Korea (Republic of)", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic", 
                                             "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", 
                                             "Lithuania", "Luxembourg", "Macao", "Macedonia (the former Yugoslav Republic of)", 
                                             "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", 
                                             "Marshall Islands", "Martinique", "Mauritania", "Mauritius", 
                                             "Mayotte", "Mexico", "Micronesia (Federated States of)", "Moldova", 
                                             "Monaco", "Mongolia", "Montenegro", "Montserrat", "Morocco", 
                                             "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", 
                                             "New Caledonia", "New Zealand", "Nicaragua", "Niger", "Nigeria", 
                                             "Niue", "Norfolk Island", "Northern Mariana Islands", "Norway", 
                                             "Oman", "Pakistan", "Palau", "Palestine, State of", "Panama", 
                                             "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Pitcairn", 
                                             "Poland", "Portugal", "Puerto Rico", "Qatar", "Réunion", "Romania", 
                                             "Russian Federation", "Rwanda", "Saint Barthélemy", "Saint Helena, Ascension and Tristan da Cunha", 
                                             "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin (French part)", 
                                             "Saint Pierre and Miquelon", "Saint Vincent and the Grenadines", 
                                             "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", 
                                             "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", 
                                             "Sint Maarten (Dutch part)", "Slovakia", "Slovenia", "Solomon Islands", 
                                             "Somalia", "South Africa", "South Georgia and the South Sandwich Islands", 
                                             "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Svalbard and Jan Mayen", 
                                             "Swaziland", "Sweden", "Switzerland", "Syrian Arab Republic", 
                                             "Taiwan, Province of China", "Tajikistan", "Tanzania", 
                                             "Thailand", "Timor-Leste", "Togo", "Tokelau", "Tonga", "Trinidad and Tobago", 
                                             "Tunisia", "Turkey", "Turkmenistan", "Turks and Caicos Islands", 
                                             "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom of Great Britain and Northern Ireland", 
                                             "USA", "United States Minor Outlying Islands", 
                                             "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela (Bolivarian Republic of)", 
                                             "Vietnam", "Virgin Islands (British)", "Virgin Islands (U.S.)", 
                                             "Wallis and Futuna", "Western Sahara", "Yemen", "Zambia", "Zimbabwe"
), continent = c("Asia", "Europe", "Europe", "Africa", "Oceania", 
                 "Europe", "Africa", "Americas", NA, "Americas", "Americas", "Asia", 
                 "Americas", "Oceania", "Europe", "Asia", "Americas", "Asia", 
                 "Asia", "Americas", "Europe", "Europe", "Americas", "Africa", 
                 "Americas", "Asia", "Americas", "Americas", "Europe", "Africa", 
                 NA, "Americas", NA, "Asia", "Europe", "Africa", "Africa", "Asia", 
                 "Africa", "Americas", "Africa", "Americas", "Africa", "Africa", 
                 "Americas", "Asia", NA, NA, "Americas", "Africa", "Africa", "Africa", 
                 "Oceania", "Americas", "Africa", "Europe", "Americas", "Americas", 
                 "Asia", "Europe", "Europe", "Africa", "Americas", "Americas", 
                 "Americas", "Africa", "Americas", "Africa", "Africa", "Europe", 
                 "Africa", "Americas", "Europe", "Oceania", "Europe", "Europe", 
                 "Americas", "Oceania", NA, "Africa", "Africa", "Asia", "Europe", 
                 "Africa", "Europe", "Europe", "Americas", "Americas", "Americas", 
                 "Oceania", "Americas", "Europe", "Africa", "Africa", "Americas", 
                 "Americas", NA, "Europe", "Americas", "Asia", "Europe", "Europe", 
                 "Asia", "Asia", "Asia", "Asia", "Europe", "Europe", "Asia", "Europe", 
                 "Americas", "Asia", "Europe", "Asia", "Asia", "Africa", "Oceania", 
                 "Asia", "Asia", "Asia", "Asia", "Asia", "Europe", "Asia", "Africa", 
                 "Africa", "Africa", "Europe", "Europe", "Europe", "Asia", "Europe", 
                 "Africa", "Africa", "Asia", "Asia", "Africa", "Europe", "Oceania", 
                 "Americas", "Africa", "Africa", "Africa", "Americas", "Oceania", 
                 "Europe", "Europe", "Asia", "Europe", "Americas", "Africa", "Africa", 
                 "Asia", "Africa", "Oceania", "Asia", "Europe", "Oceania", "Oceania", 
                 "Americas", "Africa", "Africa", "Oceania", "Oceania", "Oceania", 
                 "Europe", "Asia", "Asia", "Oceania", "Asia", "Americas", "Oceania", 
                 "Americas", "Americas", "Asia", "Oceania", "Europe", "Europe", 
                 "Americas", "Asia", "Africa", "Europe", "Europe", "Africa", "Americas", 
                 "Africa", "Americas", "Americas", "Americas", "Americas", "Americas", 
                 "Oceania", "Europe", "Africa", "Asia", "Africa", "Europe", "Africa", 
                 "Africa", "Asia", "Americas", "Europe", "Europe", "Oceania", 
                 "Africa", "Africa", NA, "Africa", "Europe", "Asia", "Africa", 
                 "Americas", "Europe", "Africa", "Europe", "Europe", "Asia", "Asia", 
                 "Asia", "Africa", "Asia", "Asia", "Africa", "Oceania", "Oceania", 
                 "Americas", "Africa", "Asia", "Asia", "Americas", "Oceania", 
                 "Africa", "Europe", "Asia", "Europe", "Americas", NA, "Americas", 
                 "Asia", "Oceania", "Americas", "Asia", "Americas", "Americas", 
                 "Oceania", "Africa", "Asia", "Africa", "Africa")), row.names = c(NA, 
                                                                                  -249L), class = c("tbl_df", "tbl", "data.frame"))


# Set the file path and name of the .xlsx file -------
data_path <- "C:/Users/AndreaSanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/Meta_data_2023.06.05.xlsx"

# Use the read_excel() function to read the data into a data frame
data <- read_excel(data_path, sheet = "meta_PCC_votecounting")
data <- data[-1,]

#### --- Filter rows for PCC meta-analysis ----
sort(unique(data$effect_size_type))

data_PCC<- data%>%
  filter(effect_size_type=="partial correlation")

#### ---- Filter Adoption papers ----
sort(unique(data_PCC$y_metric_recla))
sort(unique(data_PCC$y_metric_recla_2))

table(data_PCC$y_metric_recla)
table(data_PCC$y_metric_recla_2)
names(data_PCC)

data_adoption<- data_PCC%>%
  filter(y_metric_recla_2=="adoption")

length(sort(unique(data_adoption$id))) # Number of articles 85
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
                y_metric_recla, effect_size_type,x_metric_recla, x_metric_unit,
                model_analysis_raw,model_method,coefficient_type, 
                coefficient, coefficient_num,
                variance_metric,variance_value,variance_value_num,
                z_t_value,z_t_value_num, p_value, p_value_num, df_original, n_predictors,n_predictors_num,
                n_samples,n_samples_num, country)

str(data_adoption_clean)
### Factors cleaning ----
sort(unique(data_adoption_clean$x_metric_recla))

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

write.csv(hh_age, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_age.csv", row.names=FALSE)

## Household head Gender ----
hh_gender<- data_adoption_clean%>%
  filter(x_metric_recla== "hh gender")

length(sort(unique(hh_gender$id))) # Number of articles 49
sort(unique(hh_gender$x_metric_unit))
#[1] "1= female, 0= male" "1= male, 0= female" "1= male, 2= female" "nd" 
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

write.csv(hh_gender, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_gender.csv", row.names=FALSE)

## Household head Education ----
hh_education<- data_adoption_clean%>%
  filter(x_metric_recla== "hh education")

length(sort(unique(hh_education$id))) # Number of articles 69
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
hh_education$x_metric_unit_recla[hh_education$x_metric_recla %in% "hh education"] <- factors_clean$x_metric_unit

# Change factor name
hh_education$factor[hh_education$x_metric_recla %in% "hh education"] <- "hh education"

# Factor_metric_unit
hh_education$factor_metric_unit[hh_education$x_metric_recla %in% "hh education"] <- 
  paste(hh_education$factor, " (", hh_education$x_metric_unit_recla, ")", sep="")

sort(unique(hh_education$factor_metric_unit))
str(hh_education)

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

write.csv(h_size, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_h_size.csv", row.names=FALSE)

##### Information ------
#"hh farming experience"

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

write.csv(hh_farming_experience, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_hh_farming_experience.csv", row.names=FALSE)

####### FARM CHARACTERISTICS -----
##### Biophysical ------
#"farm size"
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
farm_size$coefficient_num[farm_size$x_metric_recla %in% "farm size" &
                            farm_size$x_metric_unit %in% "acres"] <- 
  farm_size$coefficient_num[farm_size$x_metric_recla %in% "farm size" &
                              farm_size$x_metric_unit %in% "acres"] * 0.404686

farm_size$variance_value_num[farm_size$x_metric_recla %in% "farm size" &
                               farm_size$x_metric_unit %in% "acres"&
                               farm_size$variance_metric %in% c("standard error", "robust standard error")] <- 
  farm_size$variance_value_num[farm_size$x_metric_recla %in% "farm size" &
                                 farm_size$x_metric_unit %in% "acres"&
                                 farm_size$variance_metric %in% c("standard error", "robust standard error")] * 0.404686

# Convert "ktha (30 ktha=1ha)" to "ha"
farm_size$coefficient_num[farm_size$x_metric_recla %in% "farm size" &
                            farm_size$x_metric_unit %in% "ktha (30 ktha=1ha)"] <- 
  farm_size$coefficient_num[farm_size$x_metric_recla %in% "farm size" &
                              farm_size$x_metric_unit %in% "ktha (30 ktha=1ha)"] /30

farm_size$variance_value_num[farm_size$x_metric_recla %in% "farm size" &
                               farm_size$x_metric_unit %in% "ktha (30 ktha=1ha)"&
                               farm_size$variance_metric %in% c("standard error", "robust standard error")] <- 
  farm_size$variance_value_num[farm_size$x_metric_recla %in% "farm size" &
                                 farm_size$x_metric_unit %in% "ktha (30 ktha=1ha)"&
                                 farm_size$variance_metric %in% c("standard error", "robust standard error")] /30

# Convert "rai (1 rai = 0.16 ha)" to "ha"
farm_size$coefficient_num[farm_size$x_metric_recla %in% "farm size" &
                            farm_size$x_metric_unit %in% "rai (1 rai = 0.16 ha)"] <- 
  farm_size$coefficient_num[farm_size$x_metric_recla %in% "farm size" &
                              farm_size$x_metric_unit %in% "rai (1 rai = 0.16 ha)"] *0.16

farm_size$variance_value_num[farm_size$x_metric_recla %in% "farm size" &
                               farm_size$x_metric_unit %in% "rai (1 rai = 0.16 ha)"&
                               farm_size$variance_metric %in% c("standard error", "robust standard error")] <- 
  farm_size$variance_value_num[farm_size$x_metric_recla %in% "farm size" &
                                 farm_size$x_metric_unit %in% "rai (1 rai = 0.16 ha)"&
                                 farm_size$variance_metric %in% c("standard error", "robust standard error")] *0.16

# Convert "Mukhamas (1Mukhamas= 0.73 ha)" to "ha"
farm_size$coefficient_num[farm_size$x_metric_recla %in% "farm size" &
                            farm_size$x_metric_unit %in% "Mukhamas (1Mukhamas= 0.73 ha)"] <- 
  farm_size$coefficient_num[farm_size$x_metric_recla %in% "farm size" &
                              farm_size$x_metric_unit %in% "Mukhamas (1Mukhamas= 0.73 ha)"] *0.73

farm_size$variance_value_num[farm_size$x_metric_recla %in% "farm size" &
                               farm_size$x_metric_unit %in% "Mukhamas (1Mukhamas= 0.73 ha)"&
                               farm_size$variance_metric %in% c("standard error", "robust standard error")] <- 
  farm_size$variance_value_num[farm_size$x_metric_recla %in% "hh age" &
                                 farm_size$x_metric_unit %in% "Mukhamas (1Mukhamas= 0.73 ha)"&
                                 farm_size$variance_metric %in% c("standard error", "robust standard error")] *0.73

# Change the  x_metric_unit_recla
farm_size$x_metric_unit_recla[farm_size$x_metric_recla %in% "farm size"] <- farm_size$x_metric_unit

farm_size$x_metric_unit_recla[farm_size$x_metric_recla %in% "farm size" & 
                                farm_size$x_metric_unit %in% c("acres", "ktha (30 ktha=1ha)",
                                                               "rai (1 rai = 0.16 ha)","Mukhamas (1Mukhamas= 0.73 ha)")] <- "ha"

# Change factor name
farm_size$factor[farm_size$x_metric_recla %in% "farm size"] <- "farm size"

# Factor_metric_unit
farm_size$factor_metric_unit[farm_size$x_metric_recla %in% "farm size"] <- 
  paste(farm_size$factor, " (", farm_size$x_metric_unit_recla, ")", sep="")

sort(unique(farm_size$factor_metric_unit))
str(farm_size)

write.csv(farm_size, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_farm_size.csv", row.names=FALSE)

####### CONTEXT CHARACTERISTICS -----
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

write.csv(distance_market, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_distance_market.csv", row.names=FALSE)

