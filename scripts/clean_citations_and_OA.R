# aggregate_citations_and_OA.R

df$OA <- "Undefined"
df$OA[stringr::str_detect(df$NOTE, ';.*Access')] <- df$NOTE[stringr::str_detect(
  df$NOTE,
  ';.*Access'
)]
df$OA <- stringr::str_replace_all(df$OA, 'Cite.*; ', '')
table(df$OA)
# Here consider all Open Access as OA
df$OA[df$OA != "Undefined"] <- 'Open Access'

table(df$OA)

#### Citations ------------------

df$Citations <- df$NOTE
df$Citations <- stringr::str_replace_all(df$Citations, ';.*$', '')
df$Citations <- stringr::str_replace_all(df$Citations, 'Cited by: ', '')
df$Citations <- as.numeric(df$Citations)
table(df$Citations)
#hist(df$Citations)
#median(df$Citations)
