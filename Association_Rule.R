install.packages("nutshell")
library(nutshell)   
install.packages("arulesViz")
library(arulesViz)
data("audioscrobbler")
summary(audioscrobbler)

itemFrequencyPlot(audioscrobbler, support = .20)

rules1 <- apriori (data=audioscrobbler, 
                   parameter=list (supp=0.1,conf = 0.08), 
                   appearance = list (default="rhs",lhs="Foo Fighters")) 
rules1_conf <- sort (rules1, by="confidence", decreasing=TRUE)# 'high-confidence' rules.
inspect(tail(rules1))

rules2 <- apriori (data=audioscrobbler, 
                  parameter=list (supp=0.1,conf = 0.08), 
                  appearance = list (default="lhs",rhs="Foo Fighters")) 
rules2_conf <- sort (rules2, by="confidence", decreasing=TRUE)# 'high-confidence' rules.
inspect(tail(rules2))

my_rules <- tail(rules1,3)
plot(my_rules)
plot(my_rules, method = "graph", engine = "htmlwidget")
inspect(my_rules)
