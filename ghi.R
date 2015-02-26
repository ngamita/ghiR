# Manipulating GHI datasets in R
# Author: ngamita@gmail.com

# read file into R, make sure to read using "xlsx" package
# all the sheets (Names, Weights & Health centers)
# Make sure check for files in getwd(). 

require("xlsx")

# read all sheets (1,2,3 --> names_df, weights_df & health centers_df)
names_df <- read.xlsx("GHI.xlsx", 1 , stringsAsFactors=F)
weights_df <- read.xlsx("GHI.xlsx", 2 , stringsAsFactors=F, endRow = 136)
health_centers_df <- read.xlsx("GHI.xlsx", 3 , stringsAsFactors=F)

# Remove duplicate records (df = weights_df)
# duplicated(weights_df)
# weights_df[duplicated(weights_df$Patient.GHI.ID), ]
# Make unique
# unique(weights_df[duplicated(weights_df$Patient.GHI.ID), ])
# weights_df_with_no_dups <- weights_df[!duplicated(weights_df$Patient.GHI.ID), ]
# Ignore old dates, so go with the latest dates. 
weights_df_ordered <- weights_df[order(weights_df$Date.of.Measurement, decreasing = TRUE),]
weights_df_with_no_dups <- weights_df_ordered[!duplicated(weights_df_ordered$Patient.GHI.ID), ]
nrow(weights_df_with_no_dups)

# Calculate the average weight (df = weights_df)
mean(weights_df_with_no_dups$Weight..KG., na.rm = TRUE)

# Calculate the number of mothers at each health center for all mothers on the “Health Centers” tab. 
# (df = health_centers_df)
# First clean the table. 
# health_centers_df[complete.cases(health_centers_df),]

mothers_at_hcs <- table(health_centers_df$Training.Health.Center)
mothers_at_hcs


# Create a “column” or vertical bar chart displaying this information
# barplot(table(health_centers_df$Training.Health.Center))
# TODO: Draw and dump it out. 
barplot(mothers_at_hcs, main="Mothers at Each Health Center", xlab="Health Centers",  
        ylab="Count Mothers", names.arg=c("Unknown","Bumbogo","Kayanga","Nyacyonga","Rubongo"), 
        border="blue", density=c(10,20,30,40,50))

# 4th Merge tab. 
# a. The tab should only include mothers on the “Names” tab
# b. The columns should be Mama GHI ID, Mama Name, Child GHI ID, Child Name, Date of
# Measurement, Weight (KG), and Mama Health Center

# TODO: a--> run the left outer join to Names table (keep everything in names)
        # b --> run left ourter on new (merge) to health_center(keep everything in names_weight)

# step 1. remove duplicates (both tables names and weights)
names_df_with_no_dups <- (names_df[!duplicated(names_df$Mama.GHI.ID),])

# Merge on names (left outer join)
names_weights_df <- merge(names_df_with_no_dups, weights_df_with_no_dups, by="Mama.GHI.ID", all.x = TRUE)

# No health center dups
# Run Merge 

names_weights_health_df <- merge(names_weights_df, health_centers_df, 
                                 by.x="Mama.GHI.ID", by.y="GHI.ID", all.x = TRUE)

# Clean the names --> ("Mama GHI ID", "Mama Name", "Child GHI ID", "Child Name", "Date of Measurement", "Weight (KG)", "Mama Health Center")
names_weights_health_df_clean <- names_weights_health_df

names(names_weights_health_df_clean) <- c("Mama GHI ID", "Mama Name", "Child GHI ID", "Child Name", 
                             "Date of Measurement", "Weight (KG)", "Mama Health Center")

# dump into Excel (the 4th tab, xlsx document.)
write.xlsx(names_weights_health_df_clean, file='4thtab.xlsx')

# c. Re-format the date so it appears in YYYY-MM-DD format
# current format is "15-Jan-15" --> ISO 8601 international standard format %Y-%m-%d. 
names_weights_health_df$Date.of.Measurement <- 
  as.Date(names_weights_health_df$Date.of.Measurement , format = "%B %d %Y")

#write.csv(names_weights_health_df, file='dates_check.csv')

# 5. Calculate the number of mothers with names that start with the letter “M” at each health center.
# use the names_weights_health_df
# First remove the duplicate mothers (mothers with > 1 children)

df <- names_weights_health_df
df <- names_weights_health_df[!duplicated(names_weights_health_df$Mama.GHI.ID), ]
mothers_with_m_name_df <- df[substring(tolower(df$Mama), 1,1) == 'm', ]

nrow(mothers_with_m_name_df)
table(mothers_with_m_name_df$Training.Health.Center)

# 6. Calculate the average weight of the children of mothers at each health center.
avg_weights <- aggregate(Weight..KG. ~ Training.Health.Center, data= names_weights_health_df, mean)

barplot(avg_weights$Weight..KG., main="Average Weights per Health Center", names.arg = c('Bumbogo', 'Nyacyuonga', 'Rubongo'),
        xlab="Health Centers", ylab="Avg Weights", col="blue")



