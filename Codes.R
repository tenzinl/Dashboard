
# clean memory ------------------------------------------------------------
rm(list = ls())


# read in data ------------------------------------------------------------
#set working directory

library(ggplot2)
library(scales)
library(rio)
library(dplyr)
library(tidyr)
library(sf)
library(dplyr)


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

# Recode Race
arrests <- arrests %>%
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

arrests <- arrests %>%
  mutate(
    UCR_Code = gsub(" or ", ",", `Arrest Offense by UCR Code`),  # Replace "or" with ","
    UCR_Code = gsub("-", ",", `Arrest Offense by UCR Code`)     # Replace "-" with ","
  ) %>%
  separate_rows(`Arrest Offense by UCR Code`, sep = ",") 

# Recode Arrest Offense by UCR Code

arrests<- arrests %>%
  mutate(
    Crime_Group = case_when(
      `Arrest Offense by UCR Code` %in% c("09A", "100", "120", "11A", "11B", "11C", "11D", "13A", "13B", "13C") ~ "Violent Crimes",
      `Arrest Offense by UCR Code` %in% c("200", "220", "240", "250", "270", "280", "MV", "290", "23A", "23B", "23C", "23D", "23F", "23G", "23H") ~ "Property Crimes",
      `Arrest Offense by UCR Code` %in% c("35A", "35B") ~ "Drug-Related Crimes",
      `Arrest Offense by UCR Code` %in% c("36A", "36B", "370") ~ "Sexual Offenses",
      `Arrest Offense by UCR Code` %in% c("39A", "39B", "39C", "39D", "40A", "40B", "40C", "64A", "64B", 
                      "90B", "90C", "90D", "90E", "90F", "90G", "90H", "90J", "C90C") ~ "Public Order Crimes",
      `Arrest Offense by UCR Code` %in% c("210", "230", "250", "270","26A","26C","90A") ~ "White-Collar/Financial Crimes",
      `Arrest Offense by UCR Code` %in% c("520") ~ "Weapon-Related Crimes",
      `Arrest Offense by UCR Code` %in% c("720", "90", "90Z") ~ "All Other Crimes",
      TRUE ~ "Unknown"
    )
  )

offenseUCRrace = table(arrests$Crime_Group, arrests$Race)


offenseUCRrace_df = as.data.frame(offenseUCRrace)


colnames(offenseUCRrace_df) = c("Crime_Group", "Race", "Count")

# Step 7: Calculate marginal proportions by Race (margin = 2)
offenseUCRrace_mgCol = prop.table(offenseUCRrace, margin = 2) * 100


offenseUCRrace_mgCol_df = as.data.frame(offenseUCRrace_mgCol)


colnames(offenseUCRrace_mgCol_df) = c("Crime_Group", "Race", "pctCol")


offenseUCRrace_df = merge(offenseUCRrace_df, offenseUCRrace_mgCol_df, by = c("Crime_Group", "Race"))

base1 = ggplot(offenseUCRrace_df, aes(x = Crime_Group, y = pctCol ) ) 

#the bars
bars1  = base1 + geom_bar( stat = "identity" ) + theme_minimal()

# bar per day time with 'facet'
bars1 = bars1 + facet_grid(~ Race) 

bars1

barsFacet = bars1 + facet_grid(~ Race)  # X

barsFacet + coord_flip()

baseRE  = ggplot(offenseUCRrace_df, 
                 aes(x = reorder(Crime_Group, pctCol), #here
                     y = pctCol ) ) + theme_minimal()

barsRE = baseRE + geom_bar( stat = "identity" ) 
barsREFacet = barsRE + facet_grid( ~ Race) 
barsREFacet= barsREFacet + coord_flip() 

final_bar <- barsREFacet + 
  theme(
    axis.text.y = element_text(size = 8, angle = 20),  # Adjust y-axis text size and angle
    strip.text = element_text(size = 6)               # Shrink facet label text size
  ) + 
  geom_text(aes(label = ifelse(pctCol > 4, round(pctCol,1), "")),  # Annotate only if pctCol > 4
            nudge_y = 2, size = 1.5, hjust = -0.05, ) +
  labs(
    title = "Arrests by Crime Group and Race in Massachusetts",
    subtitle = "Analyzing arrest offense by race",
    caption = "Source: Massachusetts State Police",
    x = "",
    y = "%"
  )


del2Draft= final_bar


# save del2Draft ----------------------------------------------------------
saveRDS(del2Draft, file = "del2Draft.rds")




# deliverable 3 ----------------------------------------------------------

linkBoston="https://github.com/DACSS-Visual/SpatialData/raw/refs/heads/main/data/BostonContrib.xlsx"

bostonCont=rio::import(linkBoston)

#see it
head(bostonCont)


linkZips='https://raw.githubusercontent.com/DACSS-Visual/SpatialData/refs/heads/main/data/zip_codes.json'
bostonZips=sf::read_sf(linkZips)
#see it
head(bostonZips)

plot(bostonZips[2])

summary(bostonCont$Amount)

tapply(bostonCont$Amount,bostonCont$`Tender Type Description`,summary)

str(bostonCont,width = 60, strict.width = 'cut')

cont_tender=bostonCont[bostonCont$`Tender Type Description`%in% c('Check','Credit Card'),]

cont_tenderagg <- cont_tender %>%
  group_by(Zip, `Tender Type Description`) %>%
  summarise(
    amountPerCap = mean(Amount, na.rm = TRUE),
    .groups = "drop"
  )


contrib_zipMap=merge(bostonZips,cont_tenderagg,
                     by.x='ZIP5', # 
                     by.y='Zip')

do.call(data.frame,aggregate(data=contrib_zipMap,
                             amountPerCap~`Tender Type Description`,fivenum))


customCuts <- c(0, 10, 100, 200, 300, 400, 500, 1000)

theLabelsForLevels=c("upTo_10",">10_to100", ">100_to200",">200_to300", ">300_to400","400_to500","MoreThan_500")

contrib_zipMap$amount_perCap_cat <- cut(
  contrib_zipMap$amountPerCap, 
  breaks = customCuts, 
  labels = theLabelsForLevels, 
  right = FALSE 
)



final_plot = ggplot() +
  geom_sf(data = contrib_zipMap, aes(fill = amount_perCap_cat), color = NA) +
  labs(
    fill = "Average US$ PerCapita",
    title = "Check Contributions Lead Over Credit Card Contributions",
    subtitle = "Boston ZIP boundaries, 2024 contributions",
    caption = "Source: Massachusetts Office of Campaign and Political Finance",
  ) +
  scale_fill_viridis_d(option = "magma", na.value = "grey90") +
  facet_grid(~ `Tender Type Description`, labeller = as_labeller(c(
    "Check" = "Check Contributions",
    "Credit Card" = "Credit Card Contributions"
  ))) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank()
  )



del3Draft= final_plot


# save del2Draft ----------------------------------------------------------
saveRDS(del3Draft, file = "del3Draft.rds")


