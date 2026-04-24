# open_access.R

#### classify order: First author -------
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'First'
df$authortype[df$author_last == 1] <- 'Other'

tmp<-dplyr::filter(df,authortype=='First')

output <- NULL
t <- table(tmp$OA,tmp$Gender)
output <- rbind(output,effectsize::oddsratio(t))
first <- data.frame(output)

#### classify order: Coauthor ----------
df$authortype <- 'Coauthor'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Other'
#table(df$authortype,df$Gender)

tmp<-dplyr::filter(df,authortype=='Coauthor')
output <- NULL
t <- table(tmp$OA,tmp$Gender)
output <- rbind(output,effectsize::oddsratio(t))
coauthor <- data.frame(output)

#### classify order: Last ----------
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Last'

tmp<-dplyr::filter(df,authortype=='Last')
output <- NULL
t <- table(tmp$OA,tmp$Gender)
output <- rbind(output,effectsize::oddsratio(t))
last <- data.frame(output)

first$Type<-'First'
coauthor$Type<-'Co'
last$Type<-'Last'

author_OA<-rbind(first,coauthor,last)
author_OA$Type<-factor(author_OA$Type,levels=c("First","Co","Last"))



