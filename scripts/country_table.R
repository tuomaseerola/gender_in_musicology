# Country_table.R

# analyse only countries with N observations or more
country_counts <- table(df$Affiliation_country)
index <- country_counts > 60
prominent <- df[df$Affiliation_country %in% names(index)[index==TRUE],]
#table(prominent$Affiliation_country)
#length(table(prominent$Affiliation_country))

U<-unique(prominent$Affiliation_country)

DATA <- NULL
for (k in 1:length(U)) {
  prominent$target_country <- 'Untarget'
  prominent$target_country[prominent$Affiliation_country == U[k]] <- 'Target'
  t<-table(prominent$target_country,prominent$Gender)
  or <- effectsize::oddsratio(t) 
  or <- data.frame(or)
  or$Country<- U[k]
  or$N<- sum(t[1,])
  DATA <- rbind(DATA, or)  
}
DATA<-dplyr::select(DATA,Country,N,-CI,Odds_ratio,CI_low,CI_high)
DATA<-dplyr::arrange(DATA,-N)
#print(knitr::kable(DATA,digits = 2,caption='Make this into two-column table in future.'))

#head(DATA)
g1<-ggplot(DATA,aes(x=reorder(Country,Odds_ratio),y=Odds_ratio,label=paste0("n=",N)))+
  geom_point(shape=15)+
#  geom_line()+
  geom_errorbar(aes(ymin=CI_low,ymax=CI_high),width=.2,linetype='solid',color='grey50')+
  geom_text(nudge_x = .30, size=2.25)+
  theme_classic(base_size = 14)+
  geom_hline(yintercept = 1,linetype='dashed',color='grey20')+
  xlab('Country') +
  ylab('Female Authorship Odds Ratio') +
  coord_flip()
print(g1)
