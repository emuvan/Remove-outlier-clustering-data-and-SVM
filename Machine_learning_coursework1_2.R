boxplot(Whitewine_2_$`fixed acidity`)
outlayers<-boxplot(Whitewine_2_$`fixed acidity`)$out
Whitewine_2_<-Whitewine_2_[-which(Whitewine_2_$`fixed acidity` %in% outlayers),]
