# attribute_country.R

# Attempt attribute country from Affiliation using ISO 3166-1alpha-2 codes
library(countrycode)

# tactic 1; list all country names first
#names_of_countries <- countrycode::get_dictionary("exiobase3")
#save(names_of_countries, file = "data/names_of_countries.rda")
load(file = "data/names_of_countries.rda")

# fix some country names
names_of_countries$country.name[
  names_of_countries$country.name == "Hong Kong SAR China"
] <- "Hong Kong"
names_of_countries$country.name[
  names_of_countries$country.name == "Czechia"
] <- "Czech"

#### patch those countries that are not listed directly but mention city
source('scripts/missing_countries_in_affiliations.R') #

# read missing affiliations
#miss <- read.csv("data/missing_affiliations.txt", stringsAsFactors = FALSE,header = TRUE)
# For manual checking the papers via DOI
#tmp <- df[df$BIBTEXKEY==miss$BIBTEXKEY[126],]$DOI
#browseURL(paste0("https://doi.org/",tmp[1]))

#delete_studies<- unique(miss$BIBTEXKEY[miss$FIX=="DELETE_ME"])
#df <- dplyr::filter(df, !BIBTEXKEY %in% delete_studies)
#miss <- dplyr::filter(miss, !BIBTEXKEY %in% delete_studies)

# for (k in 1:nrow(miss)) {
#   df$AFFILIATIONS[df$BIBTEXKEY==miss$BIBTEXKEY[k]] <- miss$FIX[k]
# }

# attempt to estimate the countries from affiliations, but due to
# unattributed / multiple attributions, this is not accurate
uniq <- unique(df$BIBTEXKEY)
df$Affiliation_country <- NA

#k<-which(uniq=="Platte2024")

for (k in 1:length(uniq)) {
  #  print(paste0("Processing ", k, " of ", length(uniq), " BIBTEXKEYs: ", uniq[k]))
  index <- df$BIBTEXKEY == uniq[k]
  tmp_strings <- stringr::str_split(df$AFFILIATIONS[which(index)[1]], ";")
  tmp_expanded <- tmp_strings[[1]]
  tmp_expanded
  if (!is.na(tmp_expanded[1])) {
    tmp_names <- NULL
    for (i in 1:length(tmp_expanded)) {
      #      print(paste(i,"=>",tmp_expanded[i]))
      tmp_names_vector <- stringr::str_extract(
        tmp_expanded[i],
        names_of_countries$country.name
      )
      if (length(tmp_names_vector[!is.na(tmp_names_vector)]) > 0) {
        tmp_names[i] <- tmp_names_vector[!is.na(tmp_names_vector)][1] # get the first match!
      } else {
        tmp_names[i] <- NA # if no country found, put NA
      }
    }
    #    tmp_names
    df$Affiliation_country[index] <- tmp_names[1:length(which(index))]
    # if there are unattributed affiliation countries at this stage, use first authors country
    df$Affiliation_country[index][is.na(df$Affiliation_country[
      index
    ])] <- df$Affiliation_country[index][1]
  } else {
    df$Affiliation_country[index] <- NA
  }
}

## count missing values
sum(is.na(df$Affiliation_country)) # 1339 missing values
tmp <- data.frame(
  BIBTEXKEY = df$BIBTEXKEY[is.na(df$Affiliation_country)],
  Aff = df$Affiliation_country[is.na(df$Affiliation_country)],
  AFF = df$AFFILIATIONS[is.na(df$Affiliation_country)]
)

df$Affiliation_country <- str_replace_all(
  df$Affiliation_country,
  'Czech',
  'Czechia'
) # fix Czechia

# add country codes using ISO standard

df$Affiliation_country_code <- countrycode::countrycode(
  df$Affiliation_country,
  origin = "country.name",
  destination = "iso2c",
  warn = TRUE
)
#df$Affiliation_country_code

cat(paste(
  "\nEntries in the author-expanded database after author affiliations:",
  nrow(df)
)) # 9651 authors
rm(
  names_of_countries,
  tmp,
  tmp_strings,
  tmp_expanded,
  tmp_names,
  uniq,
  index,
  k,
  i,
  tmp_names_vector
)

df$Affiliation_country_exists<-'No'
df$Affiliation_country_exists[!is.na(df$Affiliation_country_code)]<-'Yes'
table(df$Affiliation_country_exists) # 1313 missing affiliations, 5 students 200 each!
table(df$JOURNAL,df$Affiliation_country_exists)

