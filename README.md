# DIPS3 Daily: Smartphone-Sensing Data Processing (SOEP-IS 2023)
 
R code I wrote as a student research assistant at **DIW Berlin, Socio-Economic Panel (SOEP)**, 2023–2024, for the DFG-funded project *Dynamic Interactions between Personality and Social Relationships* (DIPS).
 
This repository contains only the parts I coded myself. The full data release is documented in:
 
> Krämer, M., Brandes, V. et al. (2025). *SOEP-IS 2023 – DIPS3 DAILY: Daily Diaries and Smartphone Sensing on the Daily Level (DIPS Project).* SOEP Survey Papers 1430. DIW Berlin/SOEP.
 
## Background
 
DIPS studies how social relationships change from day to day. For the first time in the SOEP, participants of the SOEP Innovation Sample shared **smartphone-sensing data**: passively recorded app usage and sensor data from their phones, alongside daily diary questionnaires. This gives behavioural measures of social contact, such as how much time someone spends in messaging and calling apps, instead of relying only on self-reports.
 
## What my code does
 
Raw sensing data arrives as event logs with timestamps. My code turns ~1 million of these records into clean, documented variables that researchers can use directly.
 
1. **Cleaning and validation:** checks timestamps, duplicates, gaps and implausible values, and marks records that don't match across data sources. 
2. **Aggregation across time levels:** converts event logs into hourly and daily variables, e.g. time spent in communication apps per hour.
3. **Diary data preparation:** processes the smartphone-based daily diary questionnaires so they can be linked to the sensing data.
4. **Release-ready output:** produces user-friendly datasets and documentation for open data release through the SOEP.
 
## Tools
 
R (data.table / tidyverse <!-- keep what you used -->), Stata and Excel for documentation, Git for version control.
 
## Data access
 
The data is **not included** in this repository. SOEP-IS data is available to researchers through the [SOEP Research Data Center](https://www.diw.de/soep) after signing a data use agreement.
 
## Credits
 
Project leads: Dr. Theresa Entringer and Dr. Michael Kraemer, DIW Berlin. Code in this repository written by Vanessa Brandes.
 
