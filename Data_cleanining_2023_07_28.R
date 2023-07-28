install.packages("funModeling")
library(readxl)
library(dplyr)
library(funModeling)

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


# Set the file path and name of the .xlsx file
data_path <- "C:/Users/AndreaSanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/Meta_data_2023.06.05.xlsx"

# Use the read_excel() function to read the data into a data frame
data <- read_excel(data_path, sheet = "meta_Data_2023.06.05")
data <- data[-1,]

sort(unique(data$y_metric_recla))
sort(unique(data$y_metric_recla_2))

table(data$y_metric_recla)
table(data$y_metric_recla_2)

#### ---- Filter Adoption papers ----
adoption<- data%>%
  filter(y_metric_recla_2=="adoption")%>%
  mutate(coefficient_num= as.numeric(coefficient),
         variance_value_num= as.numeric(variance_value),
         standard_error= as.numeric(standard_error),
         z_t_value= as.numeric(z_t_value),
         n_predictors= as.numeric(n_predictors),
         n_samples= as.numeric(n_samples),
         country = as.character(country))

table(adoption$y_metric_recla)

length(sort(unique(adoption$id))) # Number of articles 109

table(adoption$coefficient_type)

### Pre-processing
adoption_clean<- adoption%>%
  dplyr::select(id,model_id,intervention_recla,effect_size_type,x_metric_recla, x_metric_unit,
                model_analysis_raw,model_method,coefficient_type, coefficient, 
                coefficient_num,variance_metric,variance_value,variance_value_num,z_t_value,n_predictors,
                n_samples)%>%
  filter(x_metric_recla== "hh education")
filter(x_metric_unit == "1= educated, 0= otherwise")

filter(effect_size_type == "vote counting")

filter(coefficient_type == "nd")

sort(unique(adoption_clean$effect_size_type))
length(sort(unique(adoption_clean$id))) # Number of articles 109
sort(unique(adoption_clean$model_method))
table(adoption_clean$model_analysis_raw, adoption_clean$model_method)

####### Farmer characteristics -----
##### Socio-demographic ------
## Household head Gender ----
sort(unique(adoption_clean$x_metric_unit))
sort(unique(adoption_clean$coefficient_type))
sort(unique(adoption_clean$effect_size_type))

# Multiply "1= female, 0= male" AND "1= male, 2= female" * -1 to convert to "1= male, 0= female"
adoption_clean$coefficient_num[adoption_clean$x_metric_unit %in% c("1= female, 0= male", "1= male, 2= female")] <- 
  adoption_clean$coefficient_num[adoption_clean$x_metric_unit %in% c("1= female, 0= male", "1= male, 2= female")] * -1

# Convert "1= female, 0= male" and "1= male, 2= female" to "1= male, 0= female"
adoption_clean$x_metric_unit_recla[adoption_clean$x_metric_recla %in% "hh gender"] <- adoption_clean$x_metric_unit
adoption_clean$x_metric_unit_recla[adoption_clean$x_metric_unit %in% c("1= male, 0= female","1= female, 0= male", "1= male, 2= female")] <- "1= male, 0= female"

# Change factor name
adoption_clean$factor[adoption_clean$x_metric_recla %in% "hh gender"] <- "hh gender"

# Factor_metric_unit
adoption_clean$factor_metric_unit[adoption_clean$x_metric_recla %in% "hh gender"] <- 
  paste(adoption_clean$factor, " (", adoption_clean$x_metric_unit_recla, ")", sep="")

sort(unique(adoption_clean$factor_metric_unit))

## Household head Age ----
sort(unique(adoption_clean$x_metric_unit))
sort(unique(adoption_clean$coefficient_type))
sort(unique(adoption_clean$effect_size_type))

# "years * (10^-2)" * 10^-2 to convert to "years"
adoption_clean$coefficient_num[adoption_clean$x_metric_recla %in% "hh age" &
                                 adoption_clean$x_metric_unit %in% "years * (10^-2)"] <- 
  adoption_clean$coefficient_num[adoption_clean$x_metric_recla %in% "hh age" &
                                   adoption_clean$x_metric_unit %in% "years * (10^-2)"] * 10^-2

adoption_clean$variance_value_num[adoption_clean$x_metric_recla %in% "hh age" &
                                    adoption_clean$x_metric_unit %in% "years * (10^-2)"&
                                    adoption_clean$variance_metric %in% "standard error"] <- 
  adoption_clean$variance_value_num[adoption_clean$x_metric_recla %in% "hh age" &
                                      adoption_clean$x_metric_unit %in% "years * (10^-2)"&
                                      adoption_clean$variance_metric %in% "standard error"] * 10^-2

# Change the  x_metric_unit_recla
adoption_clean$x_metric_unit_recla[adoption_clean$x_metric_recla %in% "hh age"] <- adoption_clean$x_metric_unit

adoption_clean$x_metric_unit_recla[adoption_clean$x_metric_recla %in% "hh age" & 
                                     adoption_clean$x_metric_unit %in% c("years","years * (10^-2)")] <- "years"

# Change factor name
adoption_clean$factor[adoption_clean$x_metric_recla %in% "hh age"] <- "hh age"

# Factor_metric_unit
adoption_clean$factor_metric_unit[adoption_clean$x_metric_recla %in% "hh age"] <- 
  paste(adoption_clean$factor, " (", adoption_clean$x_metric_unit_recla, ")", sep="")

sort(unique(adoption_clean$factor_metric_unit))

## Household head Education ----
sort(unique(adoption_clean$x_metric_unit))
sort(unique(adoption_clean$coefficient_type))
sort(unique(adoption_clean$effect_size_type))

# Change the  x_metric_unit_recla
adoption_clean$x_metric_unit_recla[adoption_clean$x_metric_recla %in% "hh education"] <- adoption_clean$x_metric_unit

# Change factor name
adoption_clean$factor[adoption_clean$x_metric_recla %in% "hh education"] <- "hh education"

# Factor_metric_unit
adoption_clean$factor_metric_unit[adoption_clean$x_metric_recla %in% "hh education"] <- 
  paste(adoption_clean$factor, " (", adoption_clean$x_metric_unit_recla, ")", sep="")

sort(unique(adoption_clean$factor_metric_unit))









#### ---- Filter the Adoption (1= yes, 0= no) papers ----
adoption_yes_no<- data%>%
  filter(y_metric_recla=="diversity adoption (1=yes, 0=no)")%>%
  mutate(coefficient_num= as.numeric(coefficient),
         variance_value= as.numeric(variance_value),
         standard_error= as.numeric(standard_error),
         z_t_value= as.numeric(z_t_value),
         n_predictors= as.numeric(n_predictors),
         n_samples= as.numeric(n_samples),
         country = as.character(country))

adoption_yes_no$variance_value<- ifelse(adoption_yes_no$variance_metric %in% "p value" &
                                          is.na(adoption_yes_no$variance_value) ,0.9,adoption_yes_no$variance_value)

length(sort(unique(adoption_yes_no$id))) # Number of articles79

### Pre-processing
adoption_yes_no_proc<- adoption%>%
  dplyr::select(id,model_id,intervention_recla,effect_size_type,x_metric_recla, x_metric_unit,
                model_analysis_raw,model_method,coefficient_type, coefficient, 
                coefficient_num,variance_metric,variance_value,z_t_value,n_predictors,
                n_samples)
filter(x_metric_recla== "hh age")

sort(unique(adoption_yes_no_proc$effect_size_type))
length(sort(unique(adoption_yes_no_proc$id))) # Number of articles
sort(unique(adoption_yes_no_proc$model_method))
table(adoption_yes_no_proc$model_analysis_raw, adoption_yes_no_proc$model_method)

sort(unique(adoption_yes_no_proc$x_metric_unit))

## Household head Gender ----
# Gender binary "1= female, 0= male" AND "1= male, 2= female" TO "1= male, 0= female"
adoption_yes_no_proc$coefficient_num[adoption_yes_no_proc$x_metric_unit %in% c("1= female, 0= male", "1= male, 2= female")] <- 
  adoption_yes_no_proc$coefficient_num[adoption_yes_no_proc$x_metric_unit %in% c("1= female, 0= male", "1= male, 2= female")] * -1

adoption_yes_no_proc$x_metric_unit_recla[adoption_yes_no_proc$x_metric_unit %in% c("1= male, 0= female","1= female, 0= male", "1= male, 2= female")] <- "1= male, 0= female"

adoption_yes_no_proc$factor[adoption_yes_no_proc$x_metric_unit_recla %in% c("1= male, 0= female")] <- "Gender (1= Male, 0= Female)"

## Household head Age ----
adoption_yes_no_proc$coefficient_num[adoption_yes_no_proc$x_metric_recla %in% "hh age" &
                                       adoption_yes_no_proc$x_metric_unit %in% "years * (10^-2)"] <- 
  adoption_yes_no_proc$coefficient_num[adoption_yes_no_proc$x_metric_recla %in% "hh age" &
                                         adoption_yes_no_proc$x_metric_unit %in% "years * (10^-2)"] * 10^-2

adoption_yes_no_proc$variance_value[adoption_yes_no_proc$x_metric_recla %in% "hh age" &
                                      adoption_yes_no_proc$x_metric_unit %in% "years * (10^-2)"&
                                      adoption_yes_no_proc$variance_metric %in% "standard error"] <- 
  adoption_yes_no_proc$variance_value[adoption_yes_no_proc$x_metric_recla %in% "hh age" &
                                        adoption_yes_no_proc$x_metric_unit %in% "years * (10^-2)"&
                                        adoption_yes_no_proc$variance_metric %in% "standard error"] * 10^-2

adoption_yes_no_proc$x_metric_unit_recla[adoption_yes_no_proc$x_metric_recla %in% "hh age" &
                                           adoption_yes_no_proc$x_metric_unit %in% c("years * (10^-2)","years")] <- "years"

adoption_yes_no_proc$factor[adoption_yes_no_proc$x_metric_recla %in% "hh age" &
                              adoption_yes_no_proc$x_metric_unit_recla %in% "years"] <- "hh age (years)"

## Farm size ----
# Farm size continuous "ktha (30 ktha=1ha)" to  "ha"
adoption_yes_no_proc$coefficient_num[adoption_yes_no_proc$x_metric_recla %in% "farm size" & adoption_yes_no_proc$x_metric_unit %in% "ktha (30 ktha=1ha)"] <- 
  adoption_yes_no_proc$coefficient_num[adoption_yes_no_proc$x_metric_recla %in% "farm size" &adoption_yes_no_proc$x_metric_unit %in% "ktha (30 ktha=1ha)"]/30

adoption_yes_no_proc$variance_value[adoption_yes_no_proc$x_metric_recla %in% "farm size" & 
                                      adoption_yes_no_proc$x_metric_unit %in% "ktha (30 ktha=1ha)"&
                                      adoption_yes_no_proc$variance_metric %in% "standard error"] <- 
  adoption_yes_no_proc$variance_value[adoption_yes_no_proc$x_metric_recla %in% "farm size" &adoption_yes_no_proc$x_metric_unit %in% "ktha (30 ktha=1ha)"&
                                        adoption_yes_no_proc$variance_metric %in% "standard error"]/30

adoption_yes_no_proc$x_metric_unit_recla[adoption_yes_no_proc$x_metric_recla %in% "farm size" & adoption_yes_no_proc$x_metric_unit %in% "ktha (30 ktha=1ha)"] <- "ha"

# Farm size continuous "acres" to  ha
adoption_yes_no_proc$coefficient_num[adoption_yes_no_proc$x_metric_recla %in% "farm size" & adoption_yes_no_proc$x_metric_unit %in% "acres"] <- 
  adoption_yes_no_proc$coefficient_num[adoption_yes_no_proc$x_metric_recla %in% "farm size" & adoption_yes_no_proc$x_metric_unit %in% "acres"]*0.404686

adoption_yes_no_proc$variance_value[adoption_yes_no_proc$x_metric_recla %in% "farm size" & 
                                      adoption_yes_no_proc$x_metric_unit %in% "acres" &
                                      adoption_yes_no_proc$variance_metric %in% "standard error"] <- 
  adoption_yes_no_proc$variance_value[adoption_yes_no_proc$x_metric_recla %in% "farm size" & 
                                        adoption_yes_no_proc$x_metric_unit %in% "acres"&
                                        adoption_yes_no_proc$variance_metric %in% "standard error"]*0.404686

adoption_yes_no_proc$x_metric_unit_recla[adoption_yes_no_proc$x_metric_recla %in% "farm size" &adoption_yes_no_proc$x_metric_unit %in% "acres"] <- "ha"

# Farm size continuous "rai (1 rai = 0.16 ha)" to  ha
adoption_yes_no_proc$coefficient_num[adoption_yes_no_proc$x_metric_recla %in% "farm size" & adoption_yes_no_proc$x_metric_unit %in% "rai (1 rai = 0.16 ha)"] <- 
  adoption_yes_no_proc$coefficient_num[adoption_yes_no_proc$x_metric_recla %in% "farm size" &adoption_yes_no_proc$x_metric_unit %in% "rai (1 rai = 0.16 ha)"]*0.16

adoption_yes_no_proc$variance_value[adoption_yes_no_proc$x_metric_recla %in% "farm size" & 
                                      adoption_yes_no_proc$x_metric_unit %in% "rai (1 rai = 0.16 ha)" &
                                      adoption_yes_no_proc$variance_metric %in% "standard error"] <- 
  adoption_yes_no_proc$variance_value[adoption_yes_no_proc$x_metric_recla %in% "farm size" &
                                        adoption_yes_no_proc$x_metric_unit %in% "rai (1 rai = 0.16 ha)"&
                                        adoption_yes_no_proc$variance_metric %in% "standard error"]*0.16

adoption_yes_no_proc$x_metric_unit_recla[adoption_yes_no_proc$x_metric_recla %in% "farm size" & adoption_yes_no_proc$x_metric_unit %in% "rai (1 rai = 0.16 ha)"] <- "ha"

adoption_yes_no_proc$x_metric_unit_recla[adoption_yes_no_proc$x_metric_recla %in% "farm size" & 
                                           adoption_yes_no_proc$x_metric_unit %in% "ha"] <- "ha"
adoption_yes_no_proc$factor[adoption_yes_no_proc$x_metric_recla %in% "farm size" & 
                              adoption_yes_no_proc$x_metric_unit_recla %in% "ha"] <- "Farm size (ha)"


### Filter articles for meta-analysis ----
adoption_yes_no_proc$coefficient_type[adoption_yes_no_proc$coefficient_type %in% "coefficient value"] <- "B"
adoption_yes_no_proc$coefficient_type[adoption_yes_no_proc$coefficient_type %in% "marginal effect"] <- "ME"
adoption_yes_no_proc$coefficient_type[adoption_yes_no_proc$coefficient_type %in% c("odds ratio")] <- "OD"


adoption_yes_no_proc$variance_metric[adoption_yes_no_proc$variance_metric %in% c("standard error", "robust standard error")] <- "SE"
adoption_yes_no_proc$variance_metric[adoption_yes_no_proc$variance_metric %in% c("t value", "t ratio")] <- "T"
adoption_yes_no_proc$variance_metric[adoption_yes_no_proc$variance_metric %in% c("z value")] <- "Z"
adoption_yes_no_proc$variance_metric[adoption_yes_no_proc$variance_metric %in% c("p value")] <- "P"

partial_correlation<- adoption_yes_no_proc%>%filter(!is.na(factor))%>%
  mutate(factor_level= paste(coefficient_type, variance_metric,sep = "_"))

### Calculate t value or z value ----

length(sort(unique(partial_correlation$id))) # Number of articles
table(partial_correlation$factor_level)
sort(unique(partial_correlation$factor_level))
sort(unique(partial_correlation$model_method))
table(partial_correlation$factor_level,partial_correlation$model_method )

# factor_level == "logit_coefficient value_t value", "logit_coefficient value_z value", "probit_coefficient value_t ratio",
# "tobit_coefficient value_t value"                                        

# model_method == ANY
# factor_level == c("B_SE")
# z= B/SE, t= B/SE
t_z_B_SE <- function (b, se) {  
  result<- (b/se)
  return(result)
}

# model_method == c("probit", "logit")
# factor_level == c("ME_SE")
# z= B/SE, t= B/SE
t_z_ME_SE <- function (me, se) {  
  result<- (me/se)
  return(result)
}

# model_method == c("probit", "logit")
# factor_level == c("B_P", ME_P)
# coefficient_num > 0 
# CHECK: Ref available: Kleinbaum, D. G., & Klein, M. (2010). Logistic regression: a self-learning text (3rd ed.). Springer Science & Business Media.
# SE = B/z; z=  Φ^−1(1-p/2) ∗ sign(B)
t_z_probit_logit_B_P <- function (p) {  
  result<- (qnorm(1-p/2))
  return(result)
}

# model_method == c("probit", "logit")
# factor_level == "B_P"
# coefficient_num < 0
# SE = B/z; z=  Φ^−1(p/2) ∗ sign(B)
t_z_probit_logit_B_P_2 <- function (p) {  
  result<- (qnorm(p/2))
  return(result)
}

# model_method == "non-parametric correlation coefﬁcient of Phi"
# variance_metric == "B_P"
# "Nonparametric Statistical Methods" by Myles Hollander and Douglas A. Wolfe (3rd edition, 2013)
# CHECK: Ref available: SE = sqrt((1 - B^2) / (n - 1)); z = B/SE
t_z_npc_phi_B_P <- function (b, n) {  
  result<- b/(sqrt((1 - b^2) / (n - 1)))
  return(result)
}


# model_method == "logit"
# variance_metric == "ME_P"

# model_method == ANY
# factor_level == "B_SE"
# z= B/SE, t= B/SE
partial_correlation$z_t_value_recal[partial_correlation$factor_level%in% "B_SE"] <-  
  t_z_B_SE(partial_correlation$coefficient_num[partial_correlation$factor_level %in% "B_SE"],
           partial_correlation$variance_value[partial_correlation$factor_level %in% "B_SE"])


# model_method == c("probit", "logit")
# factor_level == c("ME_SE")
# z= B/SE, t= B/SE
partial_correlation$z_t_value_recal[partial_correlation$model_method %in% c("probit", "logit") & partial_correlation$factor_level %in% "ME_SE"] <-  
  t_z_ME_SE(partial_correlation$coefficient_num[partial_correlation$model_method %in% c("probit", "logit") & partial_correlation$factor_level %in% "ME_SE"],
            partial_correlation$variance_value[partial_correlation$model_method %in% c("probit", "logit") & partial_correlation$factor_level %in% "ME_SE"])

sort(unique(partial_correlation$factor_level))

# model_method == c("probit", "logit")
# factor_level == c("B_P", "ME_P")
# coefficient_num > 0 
# z=  Φ^−1(1-p/2) ∗ sign(B)
partial_correlation$z_t_value_recal[partial_correlation$model_method %in%  c("probit", "logit") & 
                                      partial_correlation$factor_level %in% c("B_P","ME_P") &
                                      partial_correlation$coefficient_num > 0 ] <- 
  t_z_probit_logit_B_P(partial_correlation$variance_value[partial_correlation$model_method %in%  c("probit", "logit") & 
                                                            partial_correlation$factor_level %in% c("B_P","ME_P") &
                                                            partial_correlation$coefficient_num > 0 ])

# model_method == c("probit", "logit")
# factor_level == "B_P"
# coefficient_num < 0
# z=  Φ^−1(p/2) ∗ sign(B)
partial_correlation$z_t_value_recal[partial_correlation$model_method %in%  c("probit", "logit") & 
                                      partial_correlation$factor_level %in% "B_P" &
                                      partial_correlation$coefficient_num < 0 ] <- 
  t_z_probit_logit_B_P_2(partial_correlation$variance_value[partial_correlation$model_method %in%  c("probit", "logit") & 
                                                              partial_correlation$factor_level %in% "B_P" &
                                                              partial_correlation$coefficient_num < 0 ])

########CHECK###########
# model_method == "non-parametric correlation coefﬁcient of Phi"
# "Nonparametric Statistical Methods" by Myles Hollander and Douglas A. Wolfe (3rd edition, 2013)
# factor_level == "B_P"
# SE = sqrt((1 - B^2) / (n - 1)); z = B/SE
partial_correlation$z_t_value_recal[partial_correlation$model_method %in% "non-parametric correlation coefﬁcient of Phi" &
                                      partial_correlation$factor_level %in% "B_P" ] <- 
  t_z_npc_phi_B_P(partial_correlation$coefficient_num[partial_correlation$model_method %in% "non-parametric correlation coefﬁcient of Phi" &
                                                        partial_correlation$factor_level %in% "B_P" ],
                  partial_correlation$n_samples[partial_correlation$model_method %in% "non-parametric correlation coefﬁcient of Phi" &
                                                  partial_correlation$factor_level %in% "B_P"] )

# model_method == ANY
# factor_level == c("B_T", "B_Z")
partial_correlation$z_t_value_recal[partial_correlation$factor_level %in% c("B_T","B_Z")] <- partial_correlation$variance_value[partial_correlation$factor_level %in% c("B_T","B_Z")]
partial_correlation$z_t_value_recal[!is.na(partial_correlation$z_t_value)] <- partial_correlation$z_t_value[!is.na(partial_correlation$z_t_value)]


table(partial_correlation$factor_level,partial_correlation$model_method )

## Calculate the partial correlation effect size
#https://wviechtb.github.io/metadat/reference/dat.aloe2013.html
#install.packages("metafor")
library(metafor)

sort(unique(adoption_yes_no_proc$intervention_recla))

adoption_yes_no_meta<-escalc(measure="PCOR", ti= z_t_value_recal, ni=n_samples, mi=n_predictors, data=partial_correlation)

### Meta-analysis function
run_meta_analysis <- function(subset_arg) {
  rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id), data = adoption_yes_no_meta,
         method = "REML", tdist = TRUE, subset = subset_arg)
}

###------ Gender: binary (1= male, 0= female) -------------
table(adoption_yes_no_meta$intervention_recla,adoption_yes_no_meta$factor )

## 
adoption_yes_no_meta<- adoption_yes_no_meta%>%
  filter(intervention_recla!="pull-push")%>%
  filter(!is.na(yi))%>%
  mutate(factor_intervention= paste(factor, intervention_recla, sep="_"))


#Farm size (ha)
farm_size_agroforestry<- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id), data = adoption_yes_no_meta,
                                method = "REML", tdist = TRUE,subset = (factor=="Farm size (ha)"& intervention_recla=="agroforestry"))

summary(farm_size_agroforestry)

farm_size_intercropping<- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id), data = adoption_yes_no_meta,
                                 method = "REML", tdist = TRUE,subset = (factor=="Farm size (ha)"& intervention_recla=="intercropping"))

summary(farm_size_intercropping)

farm_size_rotation<- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id), data = adoption_yes_no_meta,
                            method = "REML", tdist = TRUE,subset = (factor=="Farm size (ha)"& intervention_recla=="crop rotation"))

summary(farm_size_rotation)

#"Gender (1= Male, 0= Female)"
gender_agroforestry<- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id), data = adoption_yes_no_meta,
                             method = "REML", tdist = TRUE,subset = (factor=="Gender (1= Male, 0= Female)"& intervention_recla=="agroforestry"))

summary(gender_agroforestry)

gender_intercropping<- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id), data = adoption_yes_no_meta,
                              method = "REML", tdist = TRUE,subset = (factor=="Gender (1= Male, 0= Female)"& intervention_recla=="intercropping"))

summary(gender_intercropping)

gender_rotation<- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id), data = adoption_yes_no_meta,
                         method = "REML", tdist = TRUE,subset = (factor=="Gender (1= Male, 0= Female)"& intervention_recla=="crop rotation"))

summary(gender_rotation)



unique_factors <- unique(subset(adoption_yes_no_meta, factor == "Farm size (ha)")$intervention_recla)
unique_factors
factor_intervention

#this code works!
run_models <- function(data) {
  interventions <- unique(subset(data, factor == "Farm size (ha)")$intervention_recla)
  results <- list()
  
  for (model in models) {
    subset_data <- subset(data, factor == "Farm size (ha)" & intervention_recla == interventions)
    model_name <- paste("farm_size", model, sep = "_")
    model_result <- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id),
                           data = subset_data,
                           method = "REML", tdist = TRUE)
    results[[model_name]] <- summary(model_result)
  }
  
  return(results)
}

# Usage
results <- run_models(adoption_yes_no_meta)

results

results_df <- do.call(rbind, results)
results_df



unique(subset(adoption_yes_no_meta, factor == factor)$intervention_recla)

#______________________
run_models <- function(data, factor_var) {
  interventions <- unique(subset(data, factor == factor_var)$intervention_recla)
  results <- list()
  
  for (intervention_var in interventions) {
    subset_data <- subset(data, factor == factor_var & intervention_recla == intervention_var)
    model_name <- paste(factor_var, intervention_var, sep = "_")
    model_result <- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id),
                           data = subset_data,
                           method = "REML", tdist = TRUE)
    results[[model_name]] <- summary(model_result)
  }
  
  return(results)
}

# Usage
results_farm_size <- run_models(adoption_yes_no_meta, "Farm size (ha)")
results_hh_gender <- run_models(adoption_yes_no_meta, "Gender (1= Male, 0= Female)")
results_hh_age <- run_models(adoption_yes_no_meta, "hh age (years)")

# Combine results into a single dataframe
results_all <- do.call(rbind, c(results_farm_size, results_hh_gender,results_hh_age))


#_______________________





# Get unique factors and intervention_recla
unique_factors <- unique(adoption_yes_no_meta$factor)
unique_interventions <- unique(adoption_yes_no_meta$intervention_recla)
unique_factors
unique_interventions

# Fit the models

results_list <- list()

for (factor in unique_factors) {
  for (intervention in unique_interventions) {
    # Filter the data for the current factor and intervention
    filtered_data <- adoption_yes_no_meta %>% filter(factor == factor & intervention_recla == intervention)
    
    # Fit the model if there is any data for the current combination
    if (!is.na(nrow(filtered_data)) && nrow(filtered_data) > 1) {
      model <- rma.mv(yi, vi, random = list(~ 1 | model_id, ~ 1 | id),
                      data = filtered_data, method = "REML", tdist = TRUE)
      attr(model, "intervention_recla") <- intervention
      attr(model, "factor") <- factor
      results_list[[paste0(factor, "_", intervention)]] <- coef(summary(model))
    }
  }
}


articles_count <- adoption_yes_no_meta %>%
  group_by(factor, intervention_recla) %>%
  summarise(n_articles = n_distinct(id))

install.packages("tibble")
library(tibble)
results<- do.call(rbind, results_list)%>%
  rownames_to_column(., var = "row_id")
#mutate(factors= unique(gender_adoption_yes_no_meta$factor),
#      intervention_recla = unique(gender_adoption_yes_no_meta$intervention_recla))
left_join(articles_count, by = "intervention_recla")%>%
  mutate(significance = if_else(pval <=0.001,"***",
                                if_else(pval>0.001&pval<0.01,"**",
                                        if_else(pval>0.01&pval<=0.05,"*",
                                                if_else(pval>0.05&pval<=0.1,"","")))))
mutate(intervention_recla_2 = c("Fallow", "Agroforestry","Intercropping","Crop rotation", 
                                "Integrated crop-livestock", "Mixed practices"))%>%
  mutate(label = paste(significance, " (", n_articles, ")", sep = ""))

install.packages("pals")
library(ggplot2)
library(pals)

ggplot(data=gender_results, aes(y=factors,x=estimate,xmin=ci.lb, xmax=ci.ub,
                                colour = factor(intervention_recla_2)))+
  geom_vline(xintercept=0, colour = "grey20",linetype = 3, size=0.7)+
  geom_errorbar(width=0.2,size=1, position = (position_dodge(width = -0.2)))+
  geom_point(size = 4, position = (position_dodge(width = -0.2)))+
  geom_text(aes(label=label, x=ci.ub, group=intervention_recla_2), vjust=0.3, hjust=-0.09,
            color="black", size=4, family="sans",position = (position_dodge(width = -0.2)))+
  scale_colour_brewer(palette = "Paired")+
  labs(x="PCOR",colour = "Diversified farming systems")+
  theme(axis.text.x = element_text(color="black",size=12,  family = "sans",
                                   margin = margin(t = 5, r = 0, b = 5, l = 0)),
        axis.text.y = element_text(color="black",size=12, family = "sans",face="bold",
                                   margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color="black",size=12, family = "sans",face="bold",
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
        legend.title = element_text(color="black",size=12, family = "sans",face="bold",
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
        legend.text = element_text(color="black",size=11, family = "sans",
                                   margin = margin(t = 0, r = 5, b = 0, l = 0)),
        legend.key = element_rect(fill = "white"),
        legend.box.background = element_rect(color="black", size=0.5),
        legend.box.margin = margin(3, 3, 3, 3),
        plot.background = element_rect(fill = "White", color = "White"),
        panel.background = element_rect(fill = "White", color = "White"),
        panel.spacing = unit(2.5, "lines"),
        axis.line = element_line(colour = "black"))

### Figure: Number of articles by country adoption yes=1, 0=no
#Study locations
#install.packages("countrycode")
library(countrycode)
library(ggplot2)
sort(unique(UN_subregion$Country.or.Area))

adoption_yes_no$country[adoption_yes_no$country %in% "Vietnam, Thailand"] <- "Vietnam"
adoption_yes_no$country[adoption_yes_no$country %in% "Ethiopia, Ghana, Kenya, Malawi,  Mozambique, Nigeria, Tanzania, Uganda,  Zambia"] <- "Ethiopia"

length(unique(adoption_yes_no$id))
unique(adoption_yes_no$intervention_recla)

country<- adoption_yes_no%>%
  select("id", "country", "intervention_recla", "x_metric_recla")%>%
  group_by(country)%>%
  mutate(n_articles = n_distinct(id))%>%
  group_by(country,n_articles)%>%
  tally()%>%
  left_join(continent_list, by = "country") 

country$continent[country$country %in% "Vietnam, Thailand"] <- "Asia"
country$continent[country$country %in% "Ethiopia, Ghana, Kenya, Malawi,  Mozambique, Nigeria, Tanzania, Uganda,  Zambia"] <- "Africa"

length(sort(unique(country$country))) #total number of countries #30
length(sort(unique(country$continent)))  #total number of continents #4
sort(unique(country$continent))
sort(unique(country$country))


world <- ggplot2::map_data("world")%>%filter(region != "Antarctica")

world_map <- ggplot2::map_data("world")%>%filter(region != "Antarctica")%>%
  left_join(country, by =  c("region" ="country"))%>%
  mutate_all(~replace(., is.na(.), 0))
mutate(region= if_else(continent=="0","",region))
#world_map$N_articles_frequency <- cut(world_map$n_articles,breaks = c(0,1,2,3,4,5,6,7))
sort(unique(world_map$N_articles_frequency))
View(world_map)


sort(unique(world_map$UN_subregion))

ggplot()+
  geom_polygon(data = world_map,mapping = aes(x = long, y = lat,group = group,fill= n_articles),
               color="grey10",size =0.05, show.legend = T)+
  coord_fixed()+
  scale_fill_gradient(low = "white", high = "turquoise4")+
  labs(fill = "Number of articles")+
  theme(legend.position = "bottom",
        legend.title =element_text(color="black",size=12, family = "sans",face="bold",
                                   margin = margin(t = 0, r = 5, b = 0, l = 0)), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(color="black", size=9, family = "sans",face="bold",hjust = 0.01,vjust = -7),
        plot.margin = margin(-7, 0, -4, 0, "cm"))+
  labs(x = NULL, y = NULL)

intervention<- adoption_yes_no %>%
  group_by(intervention_recla) %>%
  summarise(n_articles = n_distinct(id))

# Spider diagram showing the number of articles per factor, per system
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(scales)
library(ggplot2)
library(stringr)
library("grafify")

sort(unique(factors$x_metric_recla))
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "access to agricultural extension"] <- "Access to extension"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "access or use of irrigation"] <- "Access to irrigation"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "access to agricultural training"] <- "Access to training"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "administrative region assessed"] <- "Region assessed"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "farm labour force (household members)"] <- "farm labour force"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "h size"] <- "household size"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "distance from house to farm"] <- "Distancen\ farm-house"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "hh association member"] <- "Association member"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "hh education"] <- "Farmer education"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "hh engaged in off-farm activities"] <- "off-farm activities"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "hh farming experience"] <- "Farming experience"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "hh gender"] <- "Farmer gender"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "hh is native"] <- "Farmer is native"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "secured land tenure"] <- "Land tenure"
adoption_yes_no$x_metric_recla[adoption_yes_no$x_metric_recla%in% "hh off-farm income"] <- "off-farm income"


factors<- adoption_yes_no %>%
  mutate(x_metric_recla = if_else(is.na(x_metric_recla),"Others",x_metric_recla))%>%
  mutate(x_metric_recla= str_to_sentence(x_metric_recla))%>%
  group_by(x_metric_recla)%>%
  mutate(n_articles = n_distinct(id))%>%ungroup()%>%
  mutate(x_metric_recla = if_else(n_articles>=10,x_metric_recla,"Others"))%>%
  group_by(x_metric_recla,intervention_recla)%>%
  summarise(n_articles = n_distinct(id))%>%
  select(x_metric_recla,intervention_recla,n_articles)%>%
  filter(x_metric_recla!= "Others")%>%
  filter(intervention_recla!="contour farming")%>%
  pivot_wider(names_from = x_metric_recla, values_from = "n_articles")%>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
  add_row(intervention_recla = "articles_total")%>%
  mutate_if(is.numeric, ~replace(., is.na(.), 32))%>%
  mutate(intervention_recla= c("Agroforestry", "Crop rotation", "Intercropping",
                               "Integrated crop-livestock", "Mixed practices",
                               "Fallow", "Pull-push", "Embedded semi-natural","articles_total"))


p_data <- factors %>% rename(group = "intervention_recla")

circle_coords <- function(r, n_axis = ncol(p_data) - 1){
  fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
  x <- r*cos(fi)
  y <- r*sin(fi)
  
  tibble(x, y, r)
}
central_distance <- 0.15

step_1 <- map_df(seq(0, 1, 0.25) + central_distance, circle_coords) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data = circle_coords(1 + central_distance), 
               alpha = 1, fill = "gray97") +
  geom_path(aes(group = r), lty = 2, alpha = 0.5) +
  theme_void()+
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"))
step_1

axis_coords <- function(n_axis){
  fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2
  x1 <- central_distance*cos(fi)
  y1 <- central_distance*sin(fi)
  x2 <- (1 + central_distance)*cos(fi)
  y2 <- (1 + central_distance)*sin(fi)
  
  tibble(x = c(x1, x2), y = c(y1, y2), id = rep(1:n_axis, 2))
}

step_2 <- step_1 + geom_line(data = axis_coords(ncol(p_data) - 1), 
                             aes(x, y, group = id), alpha = 0.3)
step_2

text_data <- p_data %>%
  select(-group) %>%
  map_df(~ min(.) + (max(.) - min(.)) * seq(0, 1, 0.25)) %>%
  mutate(r = seq(0, 1, 0.25)) %>%
  pivot_longer(-r, names_to = "parameter", values_to = "value")

text_coords <- function(r, n_axis = ncol(p_data) - 1){
  fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2 + 0.01*2*pi/r
  x <- r*cos(fi)
  y <- r*sin(fi)
  
  tibble(x, y, r = r - central_distance)
}

labels_data <- map_df(seq(0, 1, 0.25) + central_distance, text_coords) %>%
  bind_cols(text_data %>% select(-r))%>%
  filter(value<=45)%>%
  mutate(value= if_else(parameter == "Access to credit", value, NA))

step_3 <- step_2 + 
  geom_text(data = labels_data, aes(x, y, label = value), alpha = 0.65,fontface = "bold") +
  geom_text(data = text_coords(1 + central_distance + 0.17), 
            aes(x, y), label = labels_data$parameter[1:(ncol(p_data)-1)],fontface = "bold")

step_3

rescaled_coords <- function(r, n_axis){
  fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
  tibble(r, fi) %>% mutate(x = r*cos(fi), y = r*sin(fi)) %>% select(-fi)
}

rescaled_data <- p_data %>% 
  mutate(across(-group, rescale))%>%
  mutate(copy = pull(., 2)) %>% 
  pivot_longer(-group, names_to = "parameter", values_to = "value") %>%
  group_by(group) %>%
  mutate(coords = rescaled_coords(value + central_distance, ncol(p_data) - 1)) %>%
  unnest%>%
  filter(group!="articles_total")

typeof(rescaled_data)

step_4<-step_3 + 
  geom_point(data = rescaled_data, 
             aes(x, y, group = group, col = group), 
             size = 3) +
  geom_path(data = rescaled_data, 
            aes(x, y, group = group, col = group), 
            size = 1)+
  scale_colour_grafify()+
  labs(col = "Diversified farming systems")+
  theme(legend.position = "none")
legend.title =element_text(color="black",size=12, family = "sans",face="bold",
                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
plot.margin = margin(1,1,1.5,1.2, "cm"))
step_4













