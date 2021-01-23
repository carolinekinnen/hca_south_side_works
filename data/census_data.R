library(tidycensus)
library(dplyr)

##################################
###     Reference Material     ###
##################################
# Map of Chicago with Census tracts overlaid:
# https://www2.census.gov/geo/maps/dc10map/tract/st17_il/c17031_cook/DC10CT_C17031_005.pdf
# Boundaries of South Shore (according to Google Maps):
# https://www.google.com/maps/place/South+Shore,+Chicago,+IL/@41.7627036,-87.584293,15z/data=!4m5!3m4!1s0x880e28451b90dff9:0xebe200bd6e0aa8a2!8m2!3d41.7589929!4d-87.5700257
# Census variable IDs:
# https://api.census.gov/data/2017/acs/acs5/profile/variables.html

##################################
###    Police District Data    ###
##################################
# There are two police districts in South Shore.
# The area north of 75th St is contained in District 3.
# The area south of 75th St is contained in District 4.
# Not sure where to look for data representing the boundaries....something to look into
# We could also just create a map and draw our own boundaries.

##################################
###        Census Data         ###
##################################

# Need to register API key to use
key = census_api_key("36f9784d961adc74d5268b0733587a8a3989d390",
                     install = TRUE, overwrite = TRUE)

# Set variable IDs of interest
var_ids = c("DP05_0001E", # total number of people (ppl)
            "DP02_0001E", # total number of households (HHs)
            "DP05_0005E", # number of ppl under age 5
            "DP05_0006E", # number of ppl age 5-9
            "DP05_0007E", # number of ppl age 10-14
            "DP05_0008E", # number of ppl age 15-19
            "DP05_0009E", # number of ppl age 20-24
            "DP05_0010E", # number of ppl age 25-34
            "DP05_0011E", # number of ppl age 35-44
            "DP05_0012E", # number of ppl age 45-54
            "DP05_0013E", # number of ppl age 55-59
            "DP05_0014E", # number of ppl age 60-64
            "DP05_0015E", # number of ppl age 65-74
            "DP05_0016E", # number of ppl age 75-84
            "DP05_0017E", # number of ppl over age 85
            "DP03_0052E", # number of HHs with annual income + benefits under $10,000
            "DP03_0053E", # number of HHs with annual income + benefits $10,000-$14,999
            "DP03_0054E", # number of HHs with annual income + benefits $15,000-$24,999
            "DP03_0055E", # number of HHs with annual income + benefits $25,000-$34,999
            "DP03_0056E", # number of HHs with annual income + benefits $35,000-$49,999
            "DP03_0057E", # number of HHs with annual income + benefits $50,000-$74,999
            "DP03_0058E", # number of HHs with annual income + benefits $75,000-$99,999
            "DP03_0059E", # number of HHs with annual income + benefits $100,000-$149,999
            "DP03_0060E", # number of HHs with annual income + benefits $150,000-$199,999
            "DP03_0061E", # number of HHs with annual income + benefits over $200,000
            "DP02_0059E", # number of ppl over age 25 with less than 9th grade education
            "DP02_0060E", # number of ppl over age 25 with some high school
            "DP02_0061E", # number of ppl over age 25 with high school diploma
            "DP02_0062E", # number of ppl over age 25 with some college
            "DP02_0063E", # number of ppl over age 25 with associate's degree
            "DP02_0064E", # number of ppl over age 25 with bachelor's degree
            "DP02_0065E", # number of ppl over age 25 with graduate degree
            "DP03_0119E", # percentage of HHs below poverty level
            "DP03_0129E", # percentage of ppl under 18 living in poverty
            "DP02_0152E", # number of HHs with an Internet subscription
            "DP04_0003PE",# percentage of housing units that are vacant
            "DP02_0071E", # number of ppl living with a disability
            "DP03_0096E", # number of ppl with health insurance
            "DP03_0097E", # number of ppl with private health insurance
            "DP03_0098E") # number of ppl with public health insurance
            # Still need data on public housing, public assistance, banks pc,
            # grocery stores pc, pharmacies pc, currency exchanges pc....something to look into
            
# User-defined function that pulls in Census data for South Shore
pull_data = function(year, zone){
  # Pull data
  data = get_acs(geography = zone, variables = var_ids,
                     state = "IL", year = year,
                     geometry = TRUE, # not really sure what this does
                     key = key)
  # Keep only South Shore data
  if (zone == "tract"){
    data = data %>% filter(GEOID %in% c("17031430101", "17031430102",
                                        paste(seq(17031430200, 17031430900, 100)),
                                        "17031431200", "17031431301", "17031431302",
                                        "17031431400", "17031834200")) # there should be 15 tracts
  }else if (zone == "zcta"){
    data = data %>% filter(GEOID == "60619" | GEOID == "60637" | GEOID == "60649") # there should be 3 zip codes
  }
  return(data)
}

census_data = pull_data(2019, # year of interest (as of 01/22/2021, the most recent data is from 2019)
                       "tract") # zone of interest (possible values are "tract" and "zcta";
                                # "tract" provides finer-grained data based on Census tracts;
                                # "zcta" provides coarser-grained data based on zip codes)
