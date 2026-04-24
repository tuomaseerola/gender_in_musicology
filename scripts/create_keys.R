# create_keys.R

#### Add Author ids--------

library(dplyr)
df <- df %>%
  group_by(BIBTEXKEY) %>%
  mutate(author_id = paste0("author", row_number()))

df$paper_id_author_id <- paste0(df$BIBTEXKEY, '_', df$author_id)

#### Add indices for the author orders -------

df$author_order <- as.numeric(stringr::str_replace(df$author_id, 'author', ''))
df$author_order
df <- mutate(group_by(df, BIBTEXKEY), Max = max(author_order))
df$author_last <- 0
df$author_last[df$author_order == df$Max] <- 1 # if the author rank equals max
df$author_last[df$author_order == 1] <- 0 # except when the rank is 1

#df <- dplyr::select(df,-Max) # delete this column, not needed

#### Add single authors
df$author_single <- 0
df$author_single[df$author_order==1 & df$Max==1] <- 1 # if the author is the only one in the paper

#### Unique author ids ----------
df$unique_name <- paste0(
  str_trim(str_to_lower(df$last_name, locale = "en")),
  "_",
  str_trim(str_to_lower(df$first_name, locale = "en"))
)

cat(paste("\n Unique authors count:", length(unique(df$unique_name))))
cat(paste("\n Unique countries count:", length(unique(df$Affiliation_country))))

table(df$author_order)
