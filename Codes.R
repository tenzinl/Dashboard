
# clean memory ------------------------------------------------------------
rm(list = ls())


# read in data ------------------------------------------------------------
#set working directory

library(ggplot2)
library(scales)
library(rio)
library(dplyr)
library(tidyr)

location='https://github.com/DACSS-Visual/tabular_univar_cat/raw/main/data/'
file='eduwa.rda'
link=paste0(location,file)
load(file=url(link))


# see data ----------------------------------------------------------


townEduwa=eduwa[eduwa$LocaleType=='Town',]
table(townEduwa$LocaleSub)

townEduwa$LocaleSub=droplevels(townEduwa$LocaleSub)

absoluteT=table(townEduwa$LocaleSub)
absoluteT

propT=prop.table(absoluteT)*100
propT

(tableFreq=as.data.frame(absoluteT))

# renaming data frame columns
names(tableFreq)=c("Locale","Count")
# adding percents:
tableFreq$Percent=as.vector(propT)

tableFreq
tableFreq$Locale <- gsub("Town: ", "", tableFreq$Locale)

tableFreq$LABELS <- paste0(round(tableFreq$Percent, 2), '%')


# deliverable 1 ----------------------------------------------------------

base = ggplot(data = tableFreq, 
              aes(x = reorder(Locale, Percent), y = Percent)) +
  theme_minimal()

plot1 = base + geom_bar(fill = "lightblue", stat = 'identity')

plot2 = plot1 + labs(
  title = 'The Unequal Distribution of Public Schools Across Town Types',
  subtitle = 'Washington State - 2019',
  x = NULL, 
  y = NULL,
  caption = 'Source: US Department of Education'
)

plot4 = plot2 + scale_y_continuous(
  breaks = c(0, 25, 50),
  limits = c(0, 50),
  labels = scales::unit_format(suffix = '%')
)

plot5 = plot4 + theme(
  plot.caption = element_text(hjust = 0),
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5)
)

plot6 = plot5 + geom_text(aes(y = Percent, label = LABELS), 
                          hjust = -0.2, 
                          size = 4)

del1Draft = plot6 + coord_flip()
del1Draft


# save del1Draft ----------------------------------------------------------
saveRDS(del1Draft, file = "del1Draft.rds")


# deliverable 2 ----------------------------------------------------------


linkMass="https://github.com/DACSS-Visual/tabular_bivar_catcat/raw/refs/heads/main/data/MSP%20DFS%20Arrests%2019-20Q1.xlsx"
arrests=rio::import(linkMass,which = 1)

offenseUCRrace=table(arrests$`Arrest Offense by UCR Code`,arrests$Race)
offenseUCRrace_df <- as.data.frame(offenseUCRrace)

colnames(offenseUCRrace_df) <- c("UCR_Code", "Race", "Count")

offenseUCRrace_df$UCR_Code <- gsub(" or ", ",", offenseUCRrace_df$UCR_Code) 
offenseUCRrace_df$UCR_Code <- gsub("-", ",", offenseUCRrace_df$UCR_Code)    

# Separate rows for compound UCR codes
arrests_data_cleaned <- offenseUCRrace_df %>%
  separate_rows(UCR_Code, sep = ",")


# Recode Race
arrests_data_cleaned <- arrests_data_cleaned %>%
  mutate(
    Race = case_when(
      Race == "I" ~ "American Indian or Alaskan Native",
      Race == "O" ~ "Asian or Pacific Islander",
      Race == "B" ~ "Black / African American",
      Race == "H" ~ "Hispanic",
      Race == "J" ~ "Middle Eastern or East Indian (South Asia)",
      Race == "W" ~ "White",
      Race == "U" ~ "Unknown",
      Race == "N" ~ "Not Applicable",
      TRUE ~ "Other"  # Default for unexpected values
    )
  )

# Recode Arrest Offense by UCR Code

arrests_data_cleaned <- arrests_data_cleaned %>%
  mutate(
    Crime_Group = case_when(
      UCR_Code %in% c("09A", "100", "120", "11A", "11B", "11C", "11D", "13A", "13B", "13C") ~ "Violent Crimes",
      UCR_Code %in% c("200", "220", "240", "250", "270", "280", "MV", "290", "23A", "23B", "23C", "23D", "23F", "23G", "23H") ~ "Property Crimes",
      UCR_Code %in% c("35A", "35B") ~ "Drug-Related Crimes",
      UCR_Code %in% c("36A", "36B", "370") ~ "Sexual Offenses",
      UCR_Code %in% c("39A", "39B", "39C", "39D", "40A", "40B", "40C", "64A", "64B", 
                      "90B", "90C", "90D", "90E", "90F", "90G", "90H", "90J", "C90C") ~ "Public Order Crimes",
      UCR_Code %in% c("210", "230", "250", "270","26A","26C","90A") ~ "White-Collar/Financial Crimes",
      UCR_Code %in% c("520") ~ "Weapon-Related Crimes",
      UCR_Code %in% c("720", "90", "90Z") ~ "All Other Crimes",
      TRUE ~ "Unknown"
    )
  )


base_plot <- ggplot(data = arrests_data_cleaned, 
                    aes(x = Crime_Group, y = Count, fill = Race)) +
  theme_minimal()

  
bar_chart <- base_plot + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(
    title = "Arrests by Crime Group and Race in Massachusetts",
    subtitle = "Analyzing arrest offense by race",
    x = "Crime Group",
    y = "Count of Arrests",
    fill = "Race", 
    caption = "Source: Massachusetts State Police"
  ) +
  scale_fill_brewer(palette = 'Set2') +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),  # Larger title
    plot.subtitle = element_text(hjust = 0.5, size = 9),  # Subtitle size
    axis.text.y = element_text(size = 6),  # Increase y-axis label size
    axis.text.x = element_text(size = 6),  # Increase x-axis text size
    legend.position = "bottom",  # Move legend to the right
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 6)
  )
 
  
barStacked_counts = bar_chart + geom_bar(stat = "identity",
                                         position = 'stack')#defaul

del2Draft= barStacked_counts + coord_flip()
del2Draft


# save del2Draft ----------------------------------------------------------
saveRDS(del2Draft, file = "del2Draft.rds")


  