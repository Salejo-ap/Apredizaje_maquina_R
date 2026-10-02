install.packages("lsa")
library(lsa)
?cosine
movies<-read_excel("movies.xlsx")
distancias<-cosine(as.matrix(movies),as.vector(movies$`Toy Story (1995)`))
#distances<-sapply(names(movies),function(col{if (col !=ref_col)}))
distancias<-cosine(as.matrix(movies))
distancias<-as.data.frame(distancias)
str(distancias)
distancias$`Toy Story (1995)`
which.max(distancias$`Toy Story (1995)`)
VERLO<-sort(distancias$`Toy Story (1995)`, na.last=NA)
head(VERLO)
ver<-as.data.frame(distancias$`Toy Story (1995)`)
