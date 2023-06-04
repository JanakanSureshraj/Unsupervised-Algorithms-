#This kind of analysis is called as frequent itemset analysis, association analysis or association rule learning. 

#Association Rule Mining is a two-step approach. 1. Frequent Itemset Generation. 2. Rule Generation. 

library(arules)
library(arulesViz)

#creating a list of baskets with a few products
market_basket<-
  list(
    c("apple", "beer", "rice", "meat"), 
    c("apple", "beer", "rice"), 
    c("apple", "beer"), 
    c("apple", "pear"), 
    c("milk", "beer", "rice", "meat"), 
    c("milk", "beer", "rice"),
    c("milk", "beer"),
    c("milk", "pear")
    )

#setting transaction names
names(market_basket)<-paste("T",c(1:8), sep="")
market_basket

#measures of support, lift and confidence need to be used to identify relevant rules from the set of all possible rules among items. 
#support: indication of how frequently a set of items appear in baskets (e.g.- support of {apple, beer, rice} is 2 out of 8, or 25%).
#confidence: indication of how often the support-rule has been found to be true (e.g.- confidence that beer is purchased given that apple is purchased ({apple -> beer}) is 3 out of 4, or 75%.). 
#lift: tells how likely item Y is purchased when item X is purchased, while controlling for how popular items Y and X are. E.g.- lift of {apple -> beer}= (3/8)/(4/8 * 6/8).
# lift = 1: implies no association between items.
# lift > 1: greater than 1 means that item Y is likely to be bought if item X is bought,
# lift < 1: less than 1 means that item Y is unlikely to be bought if item X is bought.
#The lift of {apple -> beer} is 1, which implies no association between the two items.

#back to code! 
#loading the transaction data into an object of the transaction class. 
trans<-as(market_basket, "transactions")
inspect(trans)
itemLabels(trans)
summary(trans)

#displaying relative item frequency 
itemFrequencyPlot(trans, topN=10, cex.names=1)
#{apple}, {milk} and {rice} all have a relative item frequency (i.e. support) of 50%.

#simple contingency table to explore data  
tbl<-crossTable(trans)
tbl[1:6, 1:6]

#add TRUE to sort items by frequency of purchase 
tbl<- crossTable(trans, sort=TRUE)
tbl

#Apriori function is a workhorse for arules package that has a lot of flexibility towards analysis.
#generating itemsets. 
itemsets<- apriori(trans, 
                   parameter=list(supp=.2, #support threshold
                                  minlen=1,
                                  target="frequent" #mining itemsets
                                  ))

summary(itemsets)
inspect(itemsets)
inspect(sort(itemsets, by="support", decreasing=TRUE))

quality(itemsets)$lift<-interestMeasure(itemsets, measure="lift", trans)
inspect(sort(itemsets, by="lift", decreasing=TRUE))
#above results are sorted by  lift and this helps supermarkets to arrange products (for example to be near) accordingly. 

#searching for rules
rules<-apriori(trans, 
               parameter=list(supp=0.2, conf=0.5, 
                              maxlen=10, 
                              target="rules"
                              ))
summary(rules)
#set of rules= 26 
#rule length distribution distribution (LHS + RHS): 4 rules with a length of 1 item; 13 rules with a length of 2 items and 9 rules with a length of 3 items.

#avoiding the rules 1 - 4 with empty RHS 
rules <- apriori(trans, 
                 parameter = list(supp=0.2, conf=0.5, 
                                  maxlen=10, 
                                  minlen=2,   # see this 2
                                  target= "rules"))
inspect(rules)
#e.g.- can observe that rule 13 states that {beer -> rice} has a support of 50% and a confidence of 67%. 
#this means this rule was found in 50% of all transactions. The confidence that rice (RHS) is purchased 
#given beer (LHS) is purchased (P(rice|beer)) is 67%. In other words, 67% of the times a customer buys beer, 
#rice is bought as well. 

#set RHS and LHS 

#setting a specific item for the RHS 
#e.g.- finding what items customers bought before buying beer
beer_rules_rhs <- apriori(trans, 
                          parameter = list(supp=0.2, conf=0.5, 
                                           maxlen=10, 
                                           minlen=2),
                          appearance = list(default="lhs", rhs="beer"))
inspect(beer_rules_rhs)

#setting a specific item for the LHS 
#e.g.- finding what items customers bought after buying beer
beer_rules_lhs <- apriori(trans, 
                          parameter = list(supp=0.2, conf=0.5, 
                                           maxlen=10, 
                                           minlen=2),
                          appearance = list(lhs="beer", default="rhs"))
inspect(beer_rules_lhs)

#another way to inspect
inspect(subset(rules,
               subset=lhs %in% c("milk", "beer") & rhs %in% 'rice' )) #%in% - matches any

#visualizing association rules 
plot(rules,method="graph", engine= 'interactive', shading=NULL)
#graph-based technique
subrules <- head(rules, n = 10, by = "confidence")#n=10 since graphs only work well with very few rules
plot(subrules, method = "graph",  engine = "htmlwidget")
