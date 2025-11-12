# Syntesis-Report-WWBI-Figures
# 01. Data processing

### Libraries

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(data.table)
library(readxl)
library(readr)
library(purrr)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(data.table)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(DT)
library(maps)
library(mapdata)
library(leaflet)
library(rnaturalearth)
library(sf)
library(plotly)
library(officer)
library(flextable)
library(viridis)
library(here)
library(glue)
library(colourpicker)
library(wbstats)
library(htmlwidgets)
library(bs4Dash)
library(countrycode)
library(bslib)
library(lubridate)
library(scales)
library(patchwork)
library(zoo)
library(scales)
library(purrr)
library(forcats)
library(readxl)
library(stringi)  
library(readxl)


#Data paths 


data_path <- "C:/WBG/GitHub/Syntesis-Report-WWBI-Figures"


data_path_in <- "C:/WBG/GitHub/Syntesis-Report-WWBI-Figures/Data/in"

### Loading data ----

# Load the data correctly

data_wwbi <- read_dta(file.path(data_path, "Data/in", "data_wwbi.dta"))

data_hour <- read_excel(
  path = file.path(data_path, "Data/in", "graphs for report chapter 3 (1).xlsx"),
  sheet = "weeklyhourly regression"
)

data_people <- read_excel(
  path = file.path(data_path, "Data/in", "graphs for report chapter 3 (1).xlsx"),
  sheet = "size of public sector pc"
)


data_educgender <- read_excel(
  path = file.path(data_path, "Data/in", "graphs for report chapter 3 (1).xlsx"),
  sheet = "education_attainment"
)

#Data gdp 

data_gdp <- read_excel(file.path(data_path_in, "gdp_capita_2015.xls"))


#View data main data 

View(data_wwbi)
head(data_wwbi)
n_distinct(data_wwbi)
nrow(data_wwbi) # 61004 observations 
glimpse(data_wwbi)



# Ensure data_wwbi is a data.table


setDT(data_wwbi)  

# Assign World Bank regions using countrycode()

data_wwbi[, wb_region := countrycode(country_name, origin = "country.name", destination = "region")]

unique(data_wwbi[, .(country_name, wb_region)])

# Manually assign continent for unmatched countries


data_wwbi[is.na(wb_region) & country_name == "Micronesia", wb_region := "East Asia & Pacific"]


# Get the latest World Bank country metadata, including income groups

wb_metadata <- wb_cachelist$countries[, c("iso3c", "income_level")]

# Ensure your data set has ISO3 country codes

data_wwbi[, iso3c := countrycode(country_name, origin = "country.name", destination = "iso3c")]

# Manually assign missing ISO3C codes

data_wwbi[country_name == "Kosovo", iso3c := "XKX"]
data_wwbi[country_name == "Micronesia", iso3c := "FSM"]

wb_metadata <- wb_metadata %>% rename(income_group = income_level)

# Merge income group data

data_wwbi <- merge(data_wwbi, wb_metadata, by = "iso3c", all.x = TRUE)

# Rename column for clarity

setnames(data_wwbi, "income_group", "income_level")

#Countries 

countries <- c(
  "Aruba", "Afghanistan", "Angola", "Anguilla", "Albania", "United Arab Emirates", 
  "Argentina", "Armenia", "Antigua and Barbuda", "Australia", "Austria", "Azerbaijan", 
  "Burundi", "Belgium", "Benin", "Burkina Faso", "Bangladesh", "Bulgaria", 
  "Bahrain", "Bahamas", "The, Bosnia and Herzegovina", "Belarus", "Belize", 
  "Bermuda", "Bolivia", "Brazil", "Barbados", "Brunei Darussalam", "Bhutan", 
  "Botswana", "Central African Republic", "Canada", "Switzerland", "Chile", 
  "China", "Cote d'Ivoire", "Cameroon", "Congo, Republic of", "Colombia", 
  "Comoros", "Cabo Verde", "Costa Rica", "Curacao", "Cayman Islands", 
  "Cyprus", "Czech Republic", "Germany", "Djibouti", "Dominica", "Denmark", "Dominican Republic", 
  "Algeria", "Ecuador", "Egypt", "Arab Republic of", "Eritrea", "Spain", 
  "Estonia", "Ethiopia", "Finland", "Fiji", "France", "Micronesia", 
  "Federated States of, Gabon", "United Kingdom", "Georgia", 
  "Ghana", "Guinea", "Gambia", "The, Guinea-Bissau", "Equatorial Guinea", 
  "Greece", "Grenada", "Guatemala", "Guyana", "Hong Kong SAR", "China", "Honduras", 
  "Croatia", "Haiti", "Hungary", "Indonesia", "India", "Ireland", "Iran", 
  "Islamic Republic of, Iraq", "Iceland", "Israel", "Italy", "Jamaica", "Jordan", 
  "Japan", "Kazakhstan", "Kenya", "Kyrgyz Republic", "Cambodia", "Kiribati", "St. Kitts and Nevis", 
  "Korea, Republic of, Kuwait", "Lao People's Democratic Republic, Lebanon", "Liberia", 
  "Libya", "St. Lucia", "Sri Lanka", "Lesotho", "Lithuania", "Luxembourg", "Latvia", 
  "Macao SAR", "China", "Morocco", "Moldova", "Madagascar", "Maldives", "Mexico", 
  "Marshall Islands", "North Macedonia", "Mali", "Malta", "Myanmar", "Montenegro", "Mongolia", 
  "Mozambique", "Mauritania", "Montserrat", "Mauritius", "Malawi", "Malaysia", "Namibia", 
  "Niger", "Nigeria", "Nicaragua", "Netherlands", "Norway", "Nepal", "Nauru", "New Zealand", 
  "Oman", "Pakistan", "Panama", "Peru", "Philippines", "Palau", "Papua New Guinea", 
  "Poland", "Puerto Rico", "Portugal", "Paraguay", "Qatar", "Romania", 
  "Russian Federation", "Rwanda", "Saudi Arabia", "Sudan", "Senegal", 
  "Singapore", "Solomon Islands", "Sierra Leone", "El Salvador", "San Marino", 
  "Somalia", "South Sudan", "Sao Tome and Principe", "Suriname", "Slovak Republic", 
  "Slovenia", "Sweden", "Eswatini", "Sint Maarten (Dutch part)", "Seychelles", 
  "Syrian Arab Republic", "Turks and Caicos Islands", "Chad", 
  "Togo", "Thailand", "Tajikistan", "Turkmenistan", "Tonga", 
  "Trinidad and Tobago", "Tunisia", "Türkiye", "Tuvalu", "Taiwan", 
  "China", "Tanzania", "Uganda", "Ukraine", "Uruguay", "United States", 
  "Uzbekistan", "St. Vincent and the Grenadines", "Venezuela", 
  "Republica Bolivariana de", "Vietnam", "Vanuatu", "Samoa", "Kosovo","South Africa",
  "Zambia", "Zimbabwe", "Americas", "Asia", "Europe", "MENA", "Oceania", "Sub-Saharan Africa")

#Drop pvalue in the indicator column

data_wwbi <- data_wwbi[!grepl("^P-Value:", data_wwbi$indicator_name), ]

# Extract available years and countries for select inputs

years <- as.character(2000:2022)  # Years are 2000 to 2022 based on column names in your data

countries <- unique(data_wwbi$country_name)  # Extract unique country names from the data set

indicator <- unique(data_wwbi$indicator_name)

# Filter the data using dplyr


data_wwbi_long <- data_wwbi %>%
  filter(indicator_name == indicator & country_name %in% countries) %>%
  select(country_name, indicator_name,wb_region,income_level,  starts_with("year_"))  # Select relevant columns


# Reshape the data using pivot_longer

data_wwbi_long <- data_wwbi_long %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value))  # Remove rows with NA values

data_wwbi_long <- data_wwbi_long %>%
  mutate(value_percentage = value * 100)

data_wwbi_long <- data_wwbi %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) #4096 obs



# Save the household data

write_dta(data_wwbi_long, file.path(data_path, "/Data/out/data_wwbi_long.dta"))

#Data Figure 1:Public sector employment per 1000 people


#Prepare data set 

public_sector_emp <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Public sector employment, as a share of formal employment", 
                                                                         "Public sector employment, as a share of paid employment", 
                                                                         "Public sector employment, as a share of total employment"), ]

public_sector_emp_total <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Public sector employment, as a share of total employment"), ]


public_sector_emp_temp <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Public sector employment, as a share of formal employment", 
                                                                              "Public sector employment, as a share of paid employment"), ]


# Filter for the indicator you want

public_sector_emp_total <- data_wwbi_long %>%
  filter(indicator_name == "Public sector employment, as a share of total employment") %>%
  filter(!is.na(wb_region)) %>%            # Remove NA regions
  filter(wb_region != "North America") %>% # Remove North America
  group_by(country_name) %>%               # Group by country
  slice_max(order_by = year, n = 1) %>%     # Keep only the last year per country
  ungroup()                                # Ungroup after slicing


public_sector_emp_total <- public_sector_emp_total %>%
  mutate(value_percentage = value * 100)

countries_per_region_total <- public_sector_emp_total %>%
  distinct(country_name, wb_region) %>%
  count(wb_region, name = "n_countries") %>%
  arrange(desc(n_countries))


# Save data set 

write_dta(public_sector_emp_total, file.path(data_path, "/Data/out/public_sector_emp_total.dta"))


# 2 as a percentage of paid employment


# Filter for the indicator you want

public_sector_emp_paid_f1 <- data_wwbi_long %>%
  filter(indicator_name == "Public sector employment, as a share of paid employment") %>%
  filter(!is.na(wb_region)) %>%            # Remove NA regions
  filter(wb_region != "North America") %>% # Remove North America
  group_by(country_name) %>%               # Group by country
  slice_max(order_by = year, n = 1) %>%     # Keep only the last year per country
  ungroup()                                # Ungroup after slicing


public_sector_emp_paid_f1 <- public_sector_emp_paid_f1 %>%
  mutate(value_percentage = value * 100)


countries_per_region_paid <- public_sector_emp_paid_f1 %>%
  distinct(country_name, wb_region) %>%
  count(wb_region, name = "n_countries") %>%
  arrange(desc(n_countries))

# Save the household data

write_rds(public_sector_emp_paid_f1, file.path(data_path, "/Data/out/public_sector_emp_paid_f1.rds"))

# 3 as percentage of formal employment 


# Filter for the indicator you want

public_sector_emp_formal <- data_wwbi_long %>%
  filter(indicator_name == "Public sector employment, as a share of formal employment") %>%
  filter(!is.na(wb_region)) %>%            # Remove NA regions
  filter(wb_region != "North America") %>% # Remove North America
  group_by(country_name) %>%               # Group by country
  slice_max(order_by = year, n = 1) %>%     # Keep only the last year per country
  ungroup()                                # Ungroup after slicing


public_sector_emp_formal <- public_sector_emp_formal %>%
  mutate(value_percentage = value * 100)


countries_per_region_formal <- public_sector_emp_formal %>%
  distinct(country_name, wb_region) %>%
  count(wb_region, name = "n_countries") %>%
  arrange(desc(n_countries))


# Save the data set 

write_dta(public_sector_emp_formal, file.path(data_path, "/Data/out/public_sector_emp_formal.dta"))

# number 1000 people 

data_people <- data_people %>%
  filter(!is.na(world_region)) %>%            # Remove NA regions
  group_by(country_name) %>%               # Group by country
  slice_max(order_by = year, n = 1) %>%     # Keep only the last year per country
  ungroup()                                # Ungroup after slicing



# --- simple normalizer that doesn't require stringi ---------------
norm <- function(x) {
  x |>
    stringr::str_squish() |>
    stringr::str_to_lower() |>
    iconv(to = "ASCII//TRANSLIT") |>
    stringr::str_replace_all("[^a-z0-9 ]", "")
}

#  Your target list 
targets_raw <- c(
  "Iceland","Luxembourg","Belgium","Ireland","Austria","Portugal","Lithuania",
  "Slovenia","Czech Republic","Panama","Uruguay","United States of America",
  "China","Albania","Venezuela","Bulgaria","Gabon","Mexico","Ecuador","Peru",
  "Paraguay","Colombia","Moldova","Timor-Leste","Vietnam","Cambodia",
  "Sri Lanka","Ghana","Zambia","Nicaragua","Honduras","India","Cameroon",
  "Bangladesh","Tajikistan","Afghanistan","Sierra Leone","Chad",
  "Rwanda","Madagascar","Burkina Faso"
)

targets <- tibble(label = targets_raw, norm = norm(targets_raw))

#  Alias map so names match your dataset ---
alias <- tribble(
  ~norm,                  ~norm_alias,
  norm("Czech Republic"),  norm("Czechia"),
  norm("Timor-Leste"),     norm("Timor Leste"),
  norm("Venezuela"),       norm("Venezuela, RB"),
  norm("Moldova"),         norm("Moldova, Republic of"),
  # optional: if your data had "United States" instead of "United States of America"
  norm("United States"),   norm("United States of America")
)


targets <- targets %>%
  left_join(alias, by = "norm") %>%
  mutate(norm = coalesce(norm_alias, norm)) %>%
  select(label, norm)

# Prep your plotting data (one row per country, latest year) ---
data_people2 <- data_people %>%
  group_by(country_name) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    country_norm = norm(country_name),
    pse_per_1k = coalesce(`Public sector employment per 1000 people...8`,
                          `Public sector employment per 1000 people...2`)
  ) %>%
  filter(!is.na(pse_per_1k))

# Save the data set 

write_rds(data_people2, file.path(data_path, "/Data/out/plot_df_f1.rds"))


#Figure 2: share of total public employees


#  Helpers (yours) -------------------------------------------------------
is_paid_share <- function(x) {
  str_detect(x, regex("workers,\\s*as a share of public paid employees", ignore_case = TRUE))
}

categorize <- function(x) {
  case_when(
    str_detect(x, regex("^Core Public Administration", ignore_case = TRUE)) & is_paid_share(x) ~ "Core Public Administration",
    str_detect(x, regex("^Public Safety",              ignore_case = TRUE)) & is_paid_share(x) ~ "Public Safety",
    str_detect(x, regex("^Education",                  ignore_case = TRUE)) & is_paid_share(x) ~ "Education",
    str_detect(x, regex("^Health",                     ignore_case = TRUE)) & is_paid_share(x) ~ "Health",
    str_detect(x, regex("^Social Security",            ignore_case = TRUE)) & is_paid_share(x) ~ "Social Security",
    TRUE ~ NA_character_
  )
}

# ---Build `picked` from your raw table ------------------------------------

picked <- data_wwbi_long %>%
  mutate(
    category = categorize(indicator_name),
    # create a stable country_code if missing (prefers iso3c, else any existing code/name)
    country_code = coalesce(.data$country_code, .data$iso3c, .data$country_name)
  ) %>%
  filter(!is.na(category)) %>%
  select(country_name, country_code, income_level, year, value, indicator_name, category)

# ---  Keep only the five categories (latest year per country/category) -------
picked_latest <- picked %>%
  filter(!is.na(value), !is.na(income_level)) %>%
  mutate(year = as.integer(year)) %>%
  group_by(country_code, category) %>%   # or: group_by(country_name, category)
  slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(value_pct = 100 * value)

# ---  Average across countries within income group ---------------------------
by_inc_cat <- picked_latest %>%
  group_by(income_level, category) %>%
  summarise(share = mean(value_pct, na.rm = TRUE), .groups = "drop")

# --- Compute residual "Other (SOE & Utilities)" -----------------------------
needed_cols <- c("Core Public Administration","Public Safety","Education","Health","Social Security")

wide_inc <- by_inc_cat %>%
  tidyr::pivot_wider(names_from = category, values_from = share) %>%
  mutate(`Other (SOE & Utilities)` =
           pmax(0, 100 - rowSums(dplyr::across(all_of(needed_cols)), na.rm = TRUE)))

stack_df <- wide_inc %>%
  tidyr::pivot_longer(cols = -income_level, names_to = "category", values_to = "share")


# Save the data set

write_rds(stack_df, file.path(data_path, "/Data/out/data_f2.rds"))

#Figure 3:Public sector employment


# Filter for the indicator you want

public_sector_emp_paid_f1 <- data_wwbi_long %>%
  filter(indicator_name == "Public sector employment, as a share of paid employment") %>%
  filter(!is.na(wb_region)) %>%            # Remove NA regions
  filter(wb_region != "North America") %>% # Remove North America
  group_by(country_name) %>%               # Group by country
  slice_max(order_by = year, n = 1) %>%     # Keep only the last year per country
  ungroup()                                # Ungroup after slicing


public_sector_emp_paid_f1 <- public_sector_emp_paid_f1 %>%
  mutate(value_percentage = value * 100)


countries_per_region_paid <- public_sector_emp_paid_f1 %>%
  distinct(country_name, wb_region) %>%
  count(wb_region, name = "n_countries") %>%
  arrange(desc(n_countries))

# Save the household data

write_rds(public_sector_emp_paid_f1, file.path(data_path, "/Data/out/public_sector_emp_paid.rds"))


# 2 as percentage of formal employment 

# Filter for the indicator you want

public_sector_emp_formal <- data_wwbi_long %>%
  filter(indicator_name == "Public sector employment, as a share of formal employment") %>%
  filter(!is.na(wb_region)) %>%            # Remove NA regions
  filter(wb_region != "North America") %>% # Remove North America
  group_by(country_name) %>%               # Group by country
  slice_max(order_by = year, n = 1) %>%     # Keep only the last year per country
  ungroup()                                # Ungroup after slicing


public_sector_emp_formal <- public_sector_emp_formal %>%
  mutate(value_percentage = value * 100)


countries_per_region_formal <- public_sector_emp_formal %>%
  distinct(country_name, wb_region) %>%
  count(wb_region, name = "n_countries") %>%
  arrange(desc(n_countries))


# Save the data set 

write_rds(public_sector_emp_formal, file.path(data_path, "/Data/out/public_sector_emp_formal.rds"))


#Figure 4: Public sector employment Non FCV and FCV countries


#Prepare data set 

public_sector_emp_fcv <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Public sector employment, as a share of formal employment", 
                                                                             "Public sector employment, as a share of paid employment", 
                                                                             "Public sector employment, as a share of total employment"), ]

# --- 0) FCV list ---------------------------------------

fcv_names <- c(
  # Conflict
  "Afghanistan","Burkina Faso","Cameroon","Central African Republic",
  "Congo, Democratic Republic of","Ethiopia","Haiti","Iraq","Lebanon","Mali",
  "Mozambique","Myanmar","Niger","Nigeria","Somalia","South Sudan","Sudan",
  "Syrian Arab Republic","Ukraine","West Bank and Gaza (territory)","Yemen, Republic of",
  # Institutional and social fragility
  "Burundi","Chad","Comoros","Congo, Republic of","Eritrea","Guinea-Bissau",
  "Kiribati","Libya", "Kosovo", "Marshall Islands","Micronesia, Federated States of",
  "Papua New Guinea","Sao Tome and Principe","Solomon Islands","Timor-Leste",
  "Tuvalu","Venezuela, RB","Zimbabwe"
)


# --- Categorize the three indicators ---------------------------------------
cat_for_indicator <- function(s) {
  case_when(
    str_detect(s, regex("share of total employment",  ignore_case = TRUE)) ~ "Public sector employment (% of total employment)",
    str_detect(s, regex("share of paid employment",   ignore_case = TRUE)) ~ "Public sector employment (% of paid employment)",
    str_detect(s, regex("share of formal employment", ignore_case = TRUE)) ~ "Public sector employment (% of formal employment)",
    TRUE ~ NA_character_
  )
}

df <- public_sector_emp_fcv %>%
  mutate(
    category = cat_for_indicator(indicator_name),
    fcv_group = if_else(country_name %in% fcv_names, "FCV", "Non-FCV", missing = "Non-FCV")
  ) %>%
  filter(!is.na(category), !is.na(value))

# --- Latest year per country × category ------------------------------------
latest <- df %>%
  group_by(iso3c, country_name, category) %>%
  slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(value_pct = 100 * value)  # convert 0.35 -> 35

# --- Average by FCV group ---------------------------------------------------
by_fcv <- latest %>%
  group_by(fcv_group, category) %>%
  summarise(mean_share = mean(value_pct, na.rm = TRUE), .groups = "drop")

# Order categories to match your example (top = formal)
cat_levels <- c(
  "Public sector employment (% of formal employment)",
  "Public sector employment (% of paid employment)",
  "Public sector employment (% of total employment)"
)
by_fcv <- by_fcv %>%
  mutate(category = factor(category, levels = cat_levels),
         fcv_group = factor(fcv_group, levels = c("FCV","Non-FCV")))
#Save data set 

write_rds(by_fcv, file.path(data_path, "/Data/out/data_fcv.rds"))


#Figure 5: Log GDP per capita vs Public sector employment

#data 

public_sector_emp_paid_educ <- data_wwbi_long %>%
  filter(indicator_name == "Individuals with primary education as a share of public paid employees")


public_sector_emp_paid_educ <- public_sector_emp_paid_educ %>%
  filter(!is.na(value)) %>%
  group_by(country_name, indicator_name) %>%
  slice_max(order_by = year, n = 1) %>%
  ungroup()

public_sector_emp_paid_educ <- public_sector_emp_paid_educ %>%
  mutate(value_percentage = value * 100)


#Rename columns 

data_gdp <- data_gdp %>%
  rename(
    country_name = `Country Name`,
    country_code = `Country Code`,
    gdp_2015     = `2015`
  )



data_gdp <- data_gdp %>%
  mutate(log_gdp = log10(gdp_2015))

# Merge by country name

merged_data <- public_sector_emp_paid_educ %>%
  left_join(data_gdp, by = "country_name")  # Adjust if your join key is different


keep_names <- c(
  "Afghanistan","Angola","Albania","Argentina","Armenia","Australia","Azerbaijan",
  "Burundi","Benin","Burkina Faso","Bangladesh","Bolivia","Brazil","Bhutan","Botswana",
  "Central African Republic","Chile","China","Cameroon",
  "Congo, Democratic Republic of",   # Dem. Rep. Congo
  "Congo, Republic of",              # Congo
  "Colombia","Comoros","Cabo Verde","Costa Rica","Djibouti",
  "Dominican Republic","Ecuador","Egypt, Arab Rep.","Ethiopia",
  "Micronesia, Federated States of", # Micronesia
  "Gabon","Georgia","Ghana","Guinea","Gambia, The",         # The Gambia
  "Guinea-Bissau","Guatemala","Honduras","Haiti","India","Iran, Islamic Rep.",
  "Jamaica","Jordan","Kazakhstan","Kenya","Cambodia","Liberia","Sri Lanka",
  "Lesotho","Morocco","Moldova","Madagascar","Maldives","Mexico","Mali","Myanmar",
  "Mongolia","Mozambique","Mauritania","Mauritius","Malawi","Namibia","Nigeria",
  "Nicaragua","Nepal","Pakistan","Panama","Peru","Philippines","Papua New Guinea",
  "Poland","Paraguay","Rwanda","Sierra Leone","El Salvador","Somalia","Eswatini",
  "Chad","Thailand","Tajikistan","Timor-Leste","Tunisia","Turkey","Tanzania",
  "Uganda","Uruguay","United States","Uzbekistan","Vietnam",
  "West Bank and Gaza","West Bank and Gaza (territory)",    # include either form
  "Kosovo","South Africa","Zambia","Zimbabwe"
)

merged_keep <- merged_data %>%
  filter(country_name %in% keep_names)

# Build the scatterplot

df_plot <- merged_keep %>%
  dplyr::filter(is.finite(log_gdp), is.finite(value_percentage))

#Save data set 

write_rds(df_plot, file.path(data_path, "/Data/out/data_f5.rds"))


#Figure B1: indicador es females, as a share of public paid and private paid employment

female_employment <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Females, as a share of public paid employees", 
                                                                         "Females, as a share of private paid employees"), ]
female_employment <- female_employment %>%
  mutate(value_percentage = value * 100)


#Rename Indicator 

female_employment <- female_employment %>%
  mutate(indicator_name = ifelse(indicator_name == "Females, as a share of public paid employees", "Public", indicator_name))

female_employment <- female_employment %>%
  mutate(indicator_name = ifelse(indicator_name == "Females, as a share of private paid employees", "Private", indicator_name))

#Keep last year available 

female_employment <- female_employment %>%
  filter(!is.na(value)) %>%  # Keep only rows with values
  group_by(country_name, indicator_name) %>%
  filter(year == max(year)) %>%  # Get latest year per country & indicator
  ungroup()    


# Keep Public/Private, pivot wide, and keep the *latest year* per country

df_scatter <- female_employment %>%
  filter(indicator_name %in% c("Public","Private")) %>%
  select(country_name, wb_region, year, indicator_name, value_percentage) %>%
  pivot_wider(names_from = indicator_name, values_from = value_percentage) %>%
  group_by(country_name) %>%
  arrange(desc(year), .by_group = TRUE) %>%
  # require both values present
  filter(!is.na(Public), !is.na(Private)) %>%
  slice(1) %>%
  ungroup()

#Save data set 

write_rds(df_scatter, file.path(data_path, "/Data/out/data_b1.rds"))

#Figure B2: Public sector workforce 

#Data set 

gender_wagep <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Gender wage premium in the public sector, by industry: Health (compared to male paid employees)", 
                                                                    "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)", 
                                                                    "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)"), ]
gender_wagep <- gender_wagep %>%
  mutate(value_percentage = value * 100)


#Rename Indicator 

gender_wagep <- gender_wagep %>%
  mutate(indicator_name = ifelse(indicator_name == "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)", "Health", indicator_name))

gender_wagep <- gender_wagep %>%
  mutate(indicator_name = ifelse(indicator_name == "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)", "Education", indicator_name))

gender_wagep <- gender_wagep %>%
  mutate(indicator_name = ifelse(indicator_name == "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)", "Public Administration", indicator_name))


gender_wagep <- gender_wagep %>%
  filter(!is.na(value)) %>%  # Keep only rows with values
  group_by(country_name, indicator_name) %>%
  filter(year == max(year)) %>%  # Get latest year per country & indicator
  ungroup()    


# Your keep list
keep_countries <- c(
  "Argentina","Armenia","Australia","Bolivia","Brazil","Botswana","Chile",
  "Dem. Rep. Congo","Colombia","Costa Rica","Dominican Republic","Ecuador",
  "Ethiopia","Georgia","The Gambia","Guatemala","Honduras","Jordan","Sri Lanka",
  "Lesotho","Moldova","Namibia","Pakistan","Peru","Philippines","Paraguay",
  "El Salvador","Uganda","Uruguay","West Bank and Gaza","Kosovo","Zambia"
)

# --- Harmonize a few country names to match your dataset --------------
name_map <- c(
  "Dem. Rep. Congo"    = "Congo, Dem. Rep.",
  "The Gambia"         = "Gambia, The",
  "Dominican Republic" = "Dominican Republic",
  "West Bank and Gaza" = "West Bank and Gaza"
)

keep_countries_std <- recode(keep_countries, !!!name_map, .default = keep_countries)

# --- Filter to the keep list -----------------------------------------
wf_keep <- gender_wagep %>%
  filter(country_name %in% keep_countries_std)

# --- Merge GDP, compute log10 ----------------------------------------
gdp_2015 <- data_gdp %>%
  mutate(gdp_value = as.numeric(gdp_2015))  # if already numeric, this is harmless

df_plot <- wf_keep %>%
  left_join(gdp_2015, by = "country_name") %>%
  mutate(log_gdp = log10(gdp_value)) %>%
  # keep only finite rows for plotting/regression
  filter(is.finite(log_gdp), is.finite(value_percentage))


#Save data set 

write_rds(df_plot, file.path(data_path, "/Data/out/df_plotb2.rds"))

#Figure 12: Public sector wage premium (compared to all private employees)


#Data set 

publicsector_wagep2 <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Public sector wage premium (compared to all private employees)"), ]

publicsector_wagep2 <- publicsector_wagep2 %>%
  mutate(value_percentage = value * 100)


#Rename Indicator 

publicsector_wagep2 <- publicsector_wagep2 %>%
  mutate(indicator_name = ifelse(indicator_name == "Public sector wage premium (compared to all private employees)", "Public sector wage premium", indicator_name))


publicsector_wagep2 <- publicsector_wagep2 %>%
  filter(!is.na(value)) %>%  # Keep only rows with values
  group_by(country_name, indicator_name) %>%
  filter(year == max(year)) %>%  # Get latest year per country & indicator
  ungroup()    

#Merge
# Keep the latest observation per country from your wage-premium data

wage_latest <- publicsector_wagep2 %>%
  group_by(country_name) %>%
  slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
  ungroup()

# Ensure GDP has log10 (your glimpse shows 'log_gdp' already; keep this as safety)

data_gdp <- data_gdp %>%
  mutate(log_gdp = if (!"log_gdp" %in% names(.)) log10(gdp_value) else log_gdp)

# Merge

df_plotf12 <- wage_latest %>%
  left_join(data_gdp %>% select(country_name, log_gdp), by = "country_name") %>%
  filter(!is.na(log_gdp), !is.na(value_percentage))

#Save data set 

write_rds(df_plotf12, file.path(data_path, "/Data/out/df_plotf12.rds"))


#Figure 15: Public sector wage premium, by education level: Tertiary Education (compared to formal wage employees)

#Data set 

educ <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Public sector wage premium, by education level: Tertiary Education (compared to formal wage employees)", 
                                                            "Public sector wage premium, by education level: Secondary Education (compared to formal wage employees)"), ]
educ <- educ %>%
  mutate(value_percentage = value * 100)


#Rename Indicator 

educ <- educ %>%
  mutate(indicator_name = ifelse(indicator_name == "Public sector wage premium, by education level: Tertiary Education (compared to formal wage employees)", "Tertiary", indicator_name))


educ <- educ %>%
  mutate(indicator_name = ifelse(indicator_name == "Public sector wage premium, by education level: Tertiary Education (compared to formal wage employees)", "Secondary", indicator_name))


#Keep last year available

educ <- educ %>%
  filter(!is.na(value)) %>%  # Keep only rows with values
  group_by(country_name, indicator_name) %>%
  filter(year == max(year)) %>%  # Get latest year per country & indicator
  ungroup() 

#Remove high income countries 

educ <- educ %>%
  filter(income_level != "High income")

plot_dff15 <- educ %>%
  # keep only secondary + tertiary wage premium indicators
  filter(indicator_code %in% c("BI.WAG.PREM.PB.SG", "BI.WAG.PREM.PB.TT")) %>%
  mutate(level = recode(indicator_code,
                        "BI.WAG.PREM.PB.SG" = "Secondary education",
                        "BI.WAG.PREM.PB.TT" = "Tertiary education")) %>%
  # latest year per country & level
  group_by(iso3c, level) %>%
  slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # average across countries (you can swap mean -> median if you prefer)
  group_by(level) %>%
  summarise(
    avg_pct = mean(value_percentage, na.rm = TRUE),
    n_countries = dplyr::n()
  ) %>%
  # order bars: secondary first
  mutate(level = factor(level, levels = c("Secondary education", "Tertiary education")))

#Save data set 

write_rds(plot_dff15, file.path(data_path, "/Data/out/df_plotf15.rds"))

#Figure 16: pay compression ratio (90/10th percentile) de private sector y de public sector

#Data set 

paycompression <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Pay compression ratio in public sector (ratio of 90th/10th percentile earners)", 
                                                                      "Pay compression ratio in private sector (ratio of 90th/10th percentile earners)"), ] 

paycompression <- paycompression %>%
  mutate(value_percentage = value * 100)


#Rename Indicator 

paycompression <- paycompression %>%
  mutate(indicator_name = ifelse(indicator_name == "Pay compression ratio in public sector (ratio of 90th/10th percentile earners)", "Public Sector", indicator_name))


paycompression <- paycompression %>%
  mutate(indicator_name = ifelse(indicator_name == "Pay compression ratio in private sector (ratio of 90th/10th percentile earners)", "Private Sector", indicator_name))


# Keep latest per country × sector

latest_pc <- paycompression %>%
  filter(!is.na(value), indicator_name %in% c("Public Sector","Private Sector")) %>%
  group_by(country_name, income_level, indicator_name) %>%
  slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
  ungroup()

#Mean by income level × sector (simple country mean)

by_inc <- latest_pc %>%
  group_by(income_level, indicator_name) %>%
  summarise(mean_ratio = mean(value, na.rm = TRUE), .groups = "drop")

#  Compute GLOBAL as the mean of the income-level means (unweighted)

inc_levels <- c("Low income","Lower middle income","Upper middle income","High income")

global_row <- by_inc %>%
  filter(income_level %in% inc_levels) %>%
  group_by(indicator_name) %>%
  summarise(mean_ratio = mean(mean_ratio, na.rm = TRUE), .groups = "drop") %>%
  mutate(income_level = "Global")

# Combine and order the categories

plot_dff16 <- bind_rows(global_row, by_inc) %>%
  filter(income_level %in% c("Global", inc_levels)) %>%
  mutate(
    income_level = factor(income_level, levels = c("Global", inc_levels)),
    indicator_name = factor(indicator_name, levels = c("Private Sector","Public Sector"))
  )
#Save data set

write_rds(plot_dff16, file.path(data_path, "/Data/out/df_plotf16.rds"))

