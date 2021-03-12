# A Review of School Shootings in the United States

## Purpose

The purpose of this project is to explore the phenomenon of school shootings in the United States, as well as the trends surrounding them. Data for this project was provided by the Naval Postgraduate School’s Center for Homeland Defense and Security (CHDS) and can be found at https://www.chds.us/ssdb/.

## Reading, Cleaning, and Transforming the Data

The following libraries were utilized for the purposes of data reading, cleaning, transformation, and visualization:

```
library(readxl)
library(tidyverse)
library(lubridate)
library(ggtext)
library(extrafont)
```

The data is kept in an Excel workbook (.xlsx) file and is spread across multiple sheets. Sheet one contained the data pertaining to the incident itself (e.g., location of school); sheet two contained shooter data (e.g., relationship to school); sheet three contained victim data (e.g., age); and sheet four contained weapon data (e.g., type of arms used).

```
incident_raw <- read_xlsx('SSDB_Raw_Data.xlsx', sheet = 2)
shooter_raw <- read_xlsx('SSDB_Raw_Data.xlsx', sheet = 3)
victim_raw <- read_xlsx('SSDB_Raw_Data.xlsx', sheet = 4)
weapon_raw <- read_xlsx('SSDB_Raw_Data.xlsx', sheet = 5)
```

While the data provided was more robust than other data sources, there were still serious data quality issues present. For example, the most common age provided in the `shooter` dataset is N/A (i.e., 16% of entries) and 89% of all racial characteristics provided in the `victims` dataset were N/A. Accordingly, data was filtered and sorted without undergoing imputation. The primary transformation tasks included pivoting, mutating and aggregating data, as well as renaming columns for standardization.

```
incident <- incident_raw %>%
  rename_all(., tolower) %>%
  mutate(
    year = year(as_date(date)),
    month = month(as_date(date)),
    day = day(as_date(date)),
    day_of_week = weekdays(as_date(date)),
    first_shot = format(strptime(first_shot, "%I:%M %p"), format="%H:%M:%S"),
    first_shot = if_else(first_shot == '', 'N/A', first_shot),
    ) %>%
  mutate(incident_time = if_else(is.na(first_shot), 'N/A', first_shot)) %>%
  select(-c(sources, number_news, date, quarter, media_attention, reliability))
  
shooter <- shooter_raw %>%
  select(-c(criminalhistory, minorchargedadult, verdict)) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(age_group = case_when(age < 13 ~ 'Child',
                               13 <= age & age <= 17 ~ 'Teen',
                               age >= 18 ~ 'Adult')) %>%
  rename(incident_id = 1) %>%
  count(incident_id, shooterdied) %>%
  pivot_wider(names_from = shooterdied, values_from = n) %>%
  rename(shooter_survived = 2,
         shooter_died = 3,
         shooter_na = 4) %>%
  mutate_all(~replace_na(., 0))
  
weapon <- weapon_raw %>%
  select(-weapondetails) %>%
  mutate(weapontype = tolower(str_replace_all(weapontype, ' ', '_'))) %>%
  rename(incident_id = 1) %>%
  count(incident_id, weapontype) %>%
  pivot_wider(names_from = weapontype, values_from = n) %>%
  mutate_all(., ~replace_na(., 0)) %>%
  mutate(handguns = handgun + mulitiple_handguns + multiple_handguns,
         rifles = rifle + multiple_rifles,
         other = no_data + multiple_unknown + unknown,
         total_weapons = handguns + rifles + other) %>%
  select(-c(handgun, mulitiple_handguns, multiple_handguns, rifle, multiple_rifles, no_data, multiple_unknown, unknown))
  
victim_count <- victim_raw %>%
  rename(incident_id = 1) %>%
  count(incident_id, injury) %>%
  pivot_wider(names_from = injury, values_from = n) %>%
  mutate_at(vars('Fatal', 'Wounded', 'None', 'Minor Injuries', 'NA'), ~replace_na(.,0)) %>%
  rename_all(., tolower)
```
  
The final dataframe that was created was **ss_df** as seen below.

```
ss_df <- left_join(x = incident, y = shooter, by = 'incident_id') %>%
  left_join(., victim_count, by = 'incident_id') %>%
  left_join(., weapon, by = 'incident_id')
```

## Visualizing the Data

The first visualization looked at the total number of shootings over time. As it seems, there is a constant upward trend since 1970, the first year present in the group. It is important to note that 2021 shootings were not included due to the COVID-19 pandemic and its potential impact on shooting frequency. A second graph looks at the total number of shootings by type, and bins them into five categories (including "Other").

```
ss_df %>%
  count(year) %>%
  filter(year != '2021') %>%
  ggplot(aes(x = year, y = n)) + 
  annotate(geom = 'text', x = 2002, y = 120, label = 'Max number of shootings\nsince data was collected', color = 'Black', family = 'Times', size = 4, hjust = 0) + 
  annotate(geom = 'segment', x = 2010, xend = 2017, y = 119, yend = 119, color = 'Black') +
  geom_line(size = 1, color = '#3A4869', alpha = 0.8, width = .2) + 
  geom_point(color = '#3A4869', size = 3, shape = 16) + 
  labs(x = 'YEAR',
       y = 'NUMBER OF SCHOOL SHOOTINGS',
       title = 'SCHOOL SHOOTINGS',
       subtitle = 'Since 1970, there have been 1,641 school shootings in the United States.') + 
  theme(panel.background = element_blank(),
        panel.grid = element_line(linetype = 3, color = '#dbdbdb'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(family = 'Andale Mono', size = '12', color = '#423E37'),
        axis.title.x = element_text(family = 'Andale Mono', size = '12', colour = '#423E37'),
        axis.title.y = element_text(family = 'Andale Mono', size = '12', colour = '#423E37'),
        plot.title = element_text(family = 'Andale Mono', size = '32', colour = 'Black'),
        plot.subtitle = element_text(family = 'Times', size = '14', colour = 'Black', face = 'italic'))
```
![image](https://user-images.githubusercontent.com/47543478/110873286-088c0a00-8297-11eb-89fe-6dabcdff00d7.png)

```
ss_df %>%
  filter(!is.na(situation)) %>%
  mutate(situation = fct_lump(situation, n = 4)) %>%
  count(year, situation, preplanned) %>%
  filter(year != '2021') %>%
  ggplot(aes(x = year, y = n)) + 
  facet_wrap(~situation, nrow = 1) +
  geom_col(fill = '#3A4869', size = 1, color = '#3A4869') +
  labs(x = 'YEAR',
       y = 'NUMBER OF SCHOOL SHOOTINGS',
       title = 'SCHOOL SHOOTINGS BY TYPE') +
  theme(panel.background = element_blank(),
        panel.grid = element_line(linetype = 3, color = '#dbdbdb'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(family = 'Andale Mono', size = '12', color = '#423E37'),
        axis.text.x = element_text(angle = 270),
        axis.title.x = element_text(family = 'Andale Mono', size = '12', colour = '#423E37'),
        axis.title.y = element_text(family = 'Andale Mono', size = '12', colour = '#423E37'),
        plot.title = element_text(family = 'Andale Mono', size = '20', colour = 'Black'),
        plot.subtitle = element_text(family = 'Times', size = '14', colour = 'Black', face = 'italic'),
        strip.background = element_blank(),
        strip.text = element_text(family = 'Andale Mono'),
        panel.spacing = unit(.5, 'cm'))
```

![image](https://user-images.githubusercontent.com/47543478/110873337-248fab80-8297-11eb-9c6c-f346ff946116.png)
