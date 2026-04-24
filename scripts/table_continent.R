# table_continent.R
#### 2. Continents ------------------
#cat("Continents \n")

unique_countries <- as.character(unique(df[, "Affiliation_country"]))
continents <- countrycode::countrycode(unique_countries,origin = "country.name",destination = "continent")
for (k in 1:length(unique_countries)) {
  df$continent[df$Affiliation_country == unique_countries[k]] <- continents[k]
}
sum(is.na(df$continent)) # should be 0)
#df$continent <- countrycode::countrycode(tmp,origin = "country.name",destination = "continent")
#table(df$continent)

U <- unique(df$continent)
print(U)
DATA <- NULL
for (k in 1:length(U)) {
  df$target_country <- 'Untarget'
  df$target_country[df$continent == U[k]] <- 'Target'
  t<-table(df$target_country,df$Gender)
  or <- effectsize::oddsratio(t) 
  or <- data.frame(or)
  or$Country<- U[k]
  or$N<- sum(t[1,])
  DATA <- rbind(DATA, or)  
}

DATA<-dplyr::select(DATA,Country,N,-CI,Odds_ratio,CI_low,CI_high)
DATA<-dplyr::arrange(DATA,-N)
names(DATA)[1]<-"Continent"
print(knitr::kable(DATA,digits = 2,caption='Female author odds ratios across continents.'))
