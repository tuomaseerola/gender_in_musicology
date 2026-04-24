# Filename: summarise_gender.R
# Author: T. Eerola, 6/4/2024
# Project: gender in music psychology
# Status: in progress
#invisible(library(modelsummary))

#### 1. Gender distribution ------

# % of female authors in this field
#print(datasummary(Gender + 1 ~ N + Percent(), data = df,output='markdown'))

S <- summarise(group_by(df, Gender), N = n(), pct = N / nrow(df))
print(knitr::kable(S))

# How many coauthors
S <- summarise(
  group_by(df, Gender),
  M_coauthors = mean(Max),
  Md_coauthors = median(Max),
  SD_coauthors = sd(Max)
)
print(knitr::kable(S))

#### figure: How many coauthors per study --------------
S <- summarise(
  group_by(df, BIBTEXKEY),
  N_coauthors = max(Max),
  N = n(),
  N_mean = mean(Max)
)

g2 <- ggplot(S,aes(x=N_coauthors)) +
  geom_histogram(bins = 8, fill = "grey50", color = "black") +
  labs(x = "Number of authors", y = "Count") +
#  scale_y_continuous(breaks = c(1,4,16,64,256,256*4,256*16))+
  scale_x_continuous(breaks = seq(1,8))+
  theme_minimal()
print(g2)

#### figure: first authors and coauthors --------------
S <- summarise(
  group_by(dplyr::filter(df, author_id == "author1"), JOURNAL),
  n = n(),
  Sum_coauthors = sum(Max),
  prop_coauthors = (Sum_coauthors - n) / n,
  Coauthors = prop_coauthors * n,
  Single = n - Coauthors
)
#head(S)

S2<-pivot_longer(S, c(Single, Coauthors), names_to = "Authorship", values_to = "Count")
#head(S2)
S2$PROP <- paste0(round(S2$prop_coauthors,3)*100,"%")
#head(S2)

# Make every other NA
S2$prop_coauthors[seq(1,nrow(S2),by=2)] <- NA

g0 <- ggplot(S2,aes(y=Count,x = reorder(JOURNAL,n),fill=Authorship,label=PROP)) +
  geom_col(color='grey20') +
  labs(x = "Number of authors", y = "Count") +
  geom_text(aes(x=JOURNAL, y=n),size=2.4,nudge_y = 50)+
  scale_fill_manual(values = c("grey60","grey95")) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 1000, 100)) +
  theme_classic() +
  theme(legend.position = "bottom")
print(g0)

# just to check the counts
# sum(S$Sum_coauthors)
# nrow(df)
# sum(S$n)


# what names
tmp <- df %>%
  select(first_name, Gender) %>%
  filter(Gender == 'female') %>%
  drop_na() %>%
  group_by(first_name) %>%
  summarise(n = n())
tmp <- dplyr::arrange(tmp, -n)
#print(knitr::kable(head(tmp, 5), caption = 'Top 5 names.'))
#rm(tmp)

#### figure: How gender distribution per journal --------------
S <- summarise(
  group_by(df,JOURNAL),
  n = n(),
  female_prop = sum(Gender=='female') / n,
  female_n = n * female_prop,
  male_n = n - female_n
)
#head(S)
S2<-pivot_longer(S, c(female_n, male_n), names_to = "Gender_N", values_to = "Count")
#head(S2)

S2$PROP <- paste0(round(S2$female_prop,3)*100,"%")
#head(S2)

# Make every other NA
S2$PROP[seq(2,nrow(S2),by=2)] <- NA
S2$female_prop[seq(2,nrow(S2),by=2)] <- NA
#head(S2)
S2$Gender_N<-factor(S2$Gender_N,levels = c("female_n","male_n"),labels = c("Female","Male"))

g1 <- ggplot(S2,aes(y=Count,x = reorder(JOURNAL,n),fill=Gender_N,label=PROP)) +
  geom_col(color='grey20') +
  labs(x = "Journals", y = "Author Gender (count)") +
  geom_text(aes(x=JOURNAL, y=n),size=2.4,nudge_y = 60)+
  scale_fill_manual(name="Gender",values = c("grey60","grey95")) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 1000, 100),limits = c(0,890),expand = c(0.001,0.001)) +
  theme_classic() +
  theme(legend.position = "bottom")
print(g1)


#### figure: Gender distribution per sub-discipline --------------

journal <- NULL
journal[1] <- "Ethnomusicology"
journal[2] <- "Ethnomusicology Forum"
journal[3] <- "Journal of Musicology"
journal[4] <- "Journal of Popular Music Education"
journal[5] <- "Journal of Popular Music Studies"
journal[6] <- "Journal of the American Musicological Society"
journal[7] <- "Journal of World Popular Music"
journal[8] <- "Music Analysis"
journal[9] <- "Music and Letters"
journal[10] <-"Music Theory Spectrum"
journal[11] <-"Nineteenth Century Music"
journal[12] <-"Nineteenth-Century Music Review"
journal[13] <-"Popular Music"
journal[14] <-"Popular Music and Society"
journal[15] <-"Popular Music History"

# subdiscplines
discipline <- NULL
discipline[1] <- "Ethnomusicology"#"Ethnomusicology"
discipline[2] <- "Ethnomusicology"#"Ethnomusicology Forum"
discipline[3] <- "Musicology"#"Journal of Musicology"
discipline[4] <- "Popular Music"#"Journal of Popular Music Education"
discipline[5] <- "Popular Music"#"Journal of Popular Music Studies"
discipline[6] <- "Musicology"#"Journal of the American Musicological Society"
discipline[7] <- "Popular Music"#"Journal of World Popular Music"
discipline[8] <- "Music Analysis"#"Music Analysis"
discipline[9] <- "Musicology"#"Music and Letters"
discipline[10] <-"Music Analysis"#"Music Theory Spectrum"
discipline[11] <-"Musicology"#"Nineteenth Century Music"
discipline[12] <-"Musicology" #"Nineteenth-Century Music Review"
discipline[13] <- "Popular Music"#"Popular Music"
discipline[14] <- "Popular Music"#"Popular Music and Society"
discipline[15] <- "Popular Music"#"Popular Music History"

df$Discipline <- NA
for (i in 1:length(journal)) {
  df$Discipline[df$JOURNAL == journal[i]] <- discipline[i]
}

S <- summarise(
  group_by(df,Discipline),
  n = n(),
  female_prop = sum(Gender=='female') / n,
  female_n = n * female_prop,
  male_n = n - female_n
)
#head(S)
S2<-pivot_longer(S, c(female_n, male_n), names_to = "Gender_N", values_to = "Count")
#head(S2)

S2$PROP <- paste0(round(S2$female_prop,3)*100,"%")
#head(S2)

# Make every other NA
S2$PROP[seq(2,nrow(S2),by=2)] <- NA
S2$female_prop[seq(2,nrow(S2),by=2)] <- NA
#head(S2)
S2$Gender_N<-factor(S2$Gender_N,levels = c("female_n","male_n"),labels = c("Female","Male"))

g2 <- ggplot(S2,aes(y=Count,x = reorder(Discipline,n),fill=Gender_N,label=PROP)) +
  geom_col(color='grey20') +
  labs(x = "Discipline", y = "Author Gender (count)") +
  geom_text(aes(x=Discipline, y=n),size=2.4,nudge_y = 60)+
  scale_fill_manual(name="Gender",values = c("grey40","grey95")) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 2500, 500),limits = c(0,2525),expand = c(0.001,0.001)) +
  theme_classic() +
  theme(legend.position = "bottom")
print(g2)



#### 3. Number of co-authors -----------
#print(datasummary(factor(author_order) + 1 ~ Gender + N + Percent(), data = df))

##### NEW SUMMARIES ------
# How many author on average
cat("\n\n\n\n\n")

cat("\nNumber of coauthors:\n\n")
library(dplyr)
s <- summarise(group_by(df, BIBTEXKEY), n = n())
cat(paste("\n\n median:", median(s$n)))
cat(paste("\n\n mean:", round(mean(s$n), 3)))
cat(paste("\n\n sd:", round(sd(s$n), 3)))
cat(paste("\n\n max:", round(max(s$n), 3)))
cat("\n\n")
rm(s)

#### figure: Where are the authors from? --------------
#
print("How many authors can be attributed?")
sum(!is.na(df$Affiliation_country_code))
print(sum(!is.na(df$Affiliation_country_code))/ nrow(df))

table(df$Affiliation_country_code, useNA = "ifany")
df$Continent <- countrycode::countrycode(df$Affiliation_country_code, "iso2c", "continent")
df$Continent[is.na(df$Continent)] <- "Unknown"
table(df$Continent)
S <- summarise(group_by(df,Continent),
             n = n(),
             female = sum(Gender=="female"),
             male = sum(Gender=="male"),
             female_prop = female/n,
             male_prop = male/n
)
Sall <- summarise(group_by(df),
               n = n(),
               female = sum(Gender=="female"),
               male = sum(Gender=="male"),
               female_prop = female/n,
               male_prop = male/n
)

Sall$Continent<-"All"

Both <- rbind(S, Sall)
print(knitr::kable(Both))

