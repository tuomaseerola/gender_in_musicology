# citations.R
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'First'
df$authortype[df$author_last == 1] <- 'Other'
#table(df$authortype)

tmp<-dplyr::filter(df,authortype=='First')
citestats_first <- summarise(group_by(tmp, Gender), Md = median(Citations), M = mean(Citations),Q75=quantile(Citations,0.75))
stats_first <- broom::glance(kruskal.test(Citations ~ Gender, data=tmp))
citestats_first$CI_lower<-c(0,0)
citestats_first$CI_upper<-c(0,0)
citestats_first$CI_lower[1]<-DescTools::MedianCI(tmp$Citations[tmp$Gender=='female'], conf.level = 0.95)[2]
citestats_first$CI_upper[1]<-DescTools::MedianCI(tmp$Citations[tmp$Gender=='female'], conf.level = 0.95)[3]
citestats_first$CI_lower[2]<-DescTools::MedianCI(tmp$Citations[tmp$Gender=='male'], conf.level = 0.95)[2]
citestats_first$CI_upper[2]<-DescTools::MedianCI(tmp$Citations[tmp$Gender=='male'], conf.level = 0.95)[3]

# classify order: Coauthor
df$authortype <- 'Coauthor'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Other'

tmp <- dplyr::filter(df,authortype=='Coauthor')
citestats_coauthor <- summarise(group_by(tmp, Gender), Md = median(Citations), M = mean(Citations),Q75=quantile(Citations,0.75))
stats_coauthor <- broom::glance(kruskal.test(Citations ~ Gender, data=tmp))
citestats_coauthor$CI_lower<-c(0,0)
citestats_coauthor$CI_upper<-c(0,0)
citestats_coauthor$CI_lower[1]<-DescTools::MedianCI(tmp$Citations[tmp$Gender=='female'], conf.level = 0.95)[2]
citestats_coauthor$CI_upper[1]<-DescTools::MedianCI(tmp$Citations[tmp$Gender=='female'], conf.level = 0.95)[3]
citestats_coauthor$CI_lower[2]<-DescTools::MedianCI(tmp$Citations[tmp$Gender=='male'], conf.level = 0.95)[2]
citestats_coauthor$CI_upper[2]<-DescTools::MedianCI(tmp$Citations[tmp$Gender=='male'], conf.level = 0.95)[3]

# classify order: Last
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Last'

tmp <- dplyr::filter(df,authortype=='Last')
citestats_last <- summarise(group_by(tmp, Gender), Md = median(Citations), M = mean(Citations),Q75=quantile(Citations,0.75))
stats_last <- broom::glance(kruskal.test(Citations ~ Gender, data=tmp))
citestats_last$CI_lower<-c(0,0)
citestats_last$CI_upper<-c(0,0)
citestats_last$CI_lower[1]<-DescTools::MedianCI(tmp$Citations[tmp$Gender=='female'], conf.level = 0.95)[2]
citestats_last$CI_upper[1]<-DescTools::MedianCI(tmp$Citations[tmp$Gender=='female'], conf.level = 0.95)[3]
citestats_last$CI_lower[2]<-DescTools::MedianCI(tmp$Citations[tmp$Gender=='male'], conf.level = 0.95)[2]
citestats_last$CI_upper[2]<-DescTools::MedianCI(tmp$Citations[tmp$Gender=='male'], conf.level = 0.95)[3]

## Across all authors
citestats_all <- summarise(group_by(df, Gender), Md = median(Citations), M = mean(Citations),Q75=quantile(Citations,0.75))
stats_all <- broom::glance(kruskal.test(Citations ~ Gender, data=df))
citestats_all$CI_lower<-c(0,0)
citestats_all$CI_upper<-c(0,0)
citestats_all$CI_lower[1]<-DescTools::MedianCI(df$Citations[tmp$Gender=='female'], conf.level = 0.95)[2]
citestats_all$CI_upper[1]<-DescTools::MedianCI(df$Citations[tmp$Gender=='female'], conf.level = 0.95)[3]
citestats_all$CI_lower[2]<-DescTools::MedianCI(df$Citations[tmp$Gender=='male'], conf.level = 0.95)[2]
citestats_all$CI_upper[2]<-DescTools::MedianCI(df$Citations[tmp$Gender=='male'], conf.level = 0.95)[3]

