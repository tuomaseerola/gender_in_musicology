# load_scopus_datasets.R

df1 <- bib2df::bib2df("data/musicology_cleaned.bib", separate_names = TRUE)
stopifnot(nrow(df1) == 3558)

cat(paste("\nEntries in the database:", nrow(df1)))

#### Deal with authors, now nested into AUTHOR column
library(tidyr)
library(dplyr)

df <- df1 %>%
  unnest(AUTHOR)

head(df)
cat(paste("\nEntries in the author-expanded database:", nrow(df))) # 4138 authors

# check what type of articles have missing names
df$TYPE[is.na(df$first_name)]

# filter Erratum
df <- df %>%
  filter(
    !str_detect(TYPE, "Erratum|Corrigendum|Retraction|Announcement|Editorial")
  )
cat(paste(
  "\nEntries in the author-expanded database after filtering editorials etc:",
  nrow(df)
)) # 9662 authors

# Filter if full name is NA
df <- df %>%
  filter(!is.na(full_name))
cat(paste(
  "\nEntries in the author-expanded database after filtering empty:",
  nrow(df)
)) # 9651 authors

rm(df1)
#### Check categories
table(df$TYPE)

#### Delete empty columns
df <- df %>%
  select(where(~ !all(is.na(.))))
