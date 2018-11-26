#Market basket analysis
#Lara Cobler Moncunill
#November 22nd 2018

library(arules) #analyze transactional data
library(arulesViz) #visual for arules package

#upload dataset:
transactions <- read.transactions("/Users/lara/Dropbox/Ubiqum/Lesson 2/Task2.4/ElectronidexTransactions2017.csv",format="basket",sep=",",rm.duplicates=TRUE) 
#duplicates in a transaction, not relevant for the analysis

#inspect the dataset
inspect(transactions) #shows transactions
inspect(transactions[9834]) #shows certain transaction
size(transactions) #number items per transaction
LIST(transactions[1:5]) #creates a list of vectors, each vector is the items in one row of the matrix
itemLabels(transactions) #labels of the items
summary(transactions)
#9835 transactions
#125 items bought
#most frequently bought: imac(2519)
# 2 with 0 items?
#2163 single items transaction
#biggest basket: 30 items
#median basket: 3 items
inspect(transactions[size(transactions)==22])


#remove empty transactions
#read as a dataframe
transactions_df <- read.csv("/Users/lara/Dropbox/Ubiqum/Lesson 2/Task2.4/ElectronidexTransactions2017.csv", header = FALSE, sep = ",")
#9835 rows
transactions_df <- transactions_df[!apply(transactions_df == "", 1, all),]
#9833 rows
#save the dataframe without column names
write.table(transactions_df, file = "transactions_noempty.csv", col.names = FALSE, row.names = FALSE, sep = ",")

#find duplicates
duplicated_row <- c() #vector that will contain all rows with duplicated values
duplicated_item <- factor() #vector that contains all duplicated items
for (i in 1:nrow(transactions_df)){
  vector <- unname(unlist(transactions_df[i,])) #vector without name
  vector <- vector[!vector %in% ""] #remove empty columns
  for (j in 1:length(vector)){
    if (duplicated(vector)[j]){
      duplicated_row <- append(duplicated_row,i) #save the row number
      duplicated_item <- append(duplicated_item,as.character(vector[j])) #save duplicated item with the name
    }
  }
}
table(duplicated_item) #most desktops and laptops!
barplot(table(duplicated_item), main="Duplicated Items",col=rainbow(19))
  #needs grooming!!, sort first
table(duplicated_row)

#change labels to product type
transactions_df_PT <- transactions_df
for (k in 1:ncol(transactions_df_PT)){
  #label Laptop
  levels(transactions_df_PT[,k]) <- gsub(".*Laptop.*","Laptop",levels(transactions_df_PT[,k]))
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Acer Aspire"] <- "Laptop"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="ASUS Chromebook"] <- "Laptop"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Apple MacBook Pro"] <- "Laptop"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Apple MacBook Air"] <- "Laptop"
  #label Accesories
  levels(transactions_df_PT[,k]) <- gsub(".*Mouse.*","Accessories",levels(transactions_df_PT[,k]))
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Generic Black 3-Button"] <- "Accessories"
  levels(transactions_df_PT[,k]) <- gsub(".*Keyboard.*","Accessories",levels(transactions_df_PT[,k]))
  levels(transactions_df_PT[,k]) <- gsub(".*Cable.*","Accessories",levels(transactions_df_PT[,k]))
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="HDMI Adapter"] <- "Accessories"
  levels(transactions_df_PT[,k]) <- gsub(".*Stand.*","Accessories",levels(transactions_df_PT[,k]))
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Full Motion Monitor Mount"] <- "Accessories"
  levels(transactions_df_PT[,k]) <- gsub(".*Drive.*","Accessories",levels(transactions_df_PT[,k]))
  #labels Audio
  levels(transactions_df_PT[,k]) <- gsub(".*Head.*","Audio",levels(transactions_df_PT[,k]))
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Apple Earpods"] <- "Audio"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Monster Beats By Dr Dre"] <- "Audio"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Monster Beats By Dr Dre "] <- "Audio"
  levels(transactions_df_PT[,k]) <- gsub(".*Speaker.*","Audio",levels(transactions_df_PT[,k]))
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="DOSS Touch Wireless Bluetooth"] <- "Audio"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Cyber Acoustics"] <- "Audio"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Sonos"] <- "Audio"  
  #labels Software
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Microsoft Office Home and Student 2016"] <- "Software"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Computer Game"] <- "Software"
  #labels Display
  levels(transactions_df_PT[,k]) <- gsub(".*Monitor.*","Display",levels(transactions_df_PT[,k]))
  #labels Printer Supplies
  levels(transactions_df_PT[,k]) <- gsub(".*Ink.*","Ink",levels(transactions_df_PT[,k]))
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Brother Printer Toner"] <- "Ink"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="DYMO Labeling Tape"] <- "Ink"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="DYMO Labeling Tape "] <- "Ink"
  #labels Printer
  levels(transactions_df_PT[,k]) <- gsub(".*Printer.*","Printer",levels(transactions_df_PT[,k]))
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="DYMO Label Manker"] <- "Printer"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="DYMO Label Manker "] <- "Printer"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="DYMO Label Manker  "] <- "Printer"
  #labels Tablet
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="iPad"] <- "Tablet"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="iPad "] <- "Tablet"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="iPad Pro"] <- "Tablet"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="iPad Pro "] <- "Tablet"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Fire HD Tablet"] <- "Tablet"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Samsung Galaxy Tablet"] <- "Tablet"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Kindle"] <- "Tablet"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Kindle "] <- "Tablet"
  #Smart Home devices
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Apple TV"] <- "Smart Home Devices"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Google Home"] <- "Smart Home Devices"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Smart Light Bulb"] <- "Smart Home Devices"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Fire TV Stick"] <- "Smart Home Devices"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="Roku Express"] <- "Smart Home Devices"
  #Desktop
  levels(transactions_df_PT[,k]) <- gsub(".*Desktop.*","Desktop",levels(transactions_df_PT[,k]))
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="iMac"] <- "Desktop"
  levels(transactions_df_PT[,k])[levels(transactions_df_PT[,k])=="iMac "] <- "Desktop"
}

#find frequency each category
table_PT <- table(as.matrix(transactions_df_PT))[3:11] #remove empty spaces
pct_PT <- round(table_PT/sum(table_PT)*100)
lbls<-names(table_PT)
lbls<-paste(lbls,pct_PT,"%")
pie(table_PT, labels = lbls, main="Sales by Product Type in Electronidex",col=rainbow(9))
#make barplot

# chart for blackwells
blackwell <- read.csv("/Users/lara/Dropbox/Ubiqum/Lesson 2/Task2.3/existingproductattributes2017.2.csv", header = T, sep = ",")
blackwell <- blackwell[,c("ProductType","Volume")]
blackwell_a <- aggregate(blackwell$Volume, by=list(blackwell$ProductType),FUN=sum)
pct_bl <- round(blackwell_a$x/sum(blackwell_a$x)*100)
lbls_bl<-blackwell_a$Group.1
lbls_bl<-paste(lbls_bl,pct_bl,"%")
pie(blackwell_a$x, labels = lbls_bl, main="Sales by Product Type in Blackwell",col=rainbow(9))

#see what are the product types repeated in the same transaction
duplicated_product <- factor() #vector that contains all duplicated items
for (i in 1:nrow(transactions_df_PT)){
  vector <- unname(unlist(transactions_df_PT[i,])) #vector without name
  vector <- vector[!vector %in% ""] #remove empty columns
  for (j in 1:length(vector)){
    if (duplicated(vector)[j]){
      duplicated_product <- append(duplicated_product,as.character(vector[j])) #save duplicated item with the name
    }
  }
}
table(duplicated_product) #most desktops and laptops!
table_duplicates <- sort(table(duplicated_product),decreasing = T)
barplot(table_duplicates, main="Duplicated Product types",col=rainbow(9))
#sort first!

#save the dataframe by product type
write.table(transactions_df_PT, file = "transactions_PT_all.csv", col.names = FALSE, row.names = FALSE, sep = ",")

#split Electronidex in two groups, companies and individuals
retail <- data.frame()
companies <- data.frame()
retail_row <- c() #store row index in case I need it later
companies_row <- c()
for (m in 1:nrow(transactions_df_PT)){
  vector <- unname(unlist(transactions_df_PT[m,])) #vector without name
  vector <- vector[!vector %in% ""] #remove empty columns
  if ((length(which(vector=="Desktop"|vector=="Laptop")) >=3) |
      (length(which(vector=="Printer")) >=2) |
      (length(which(vector=="Tablet")) >=3) |
      (length(which(vector=="Display")) >=3) |
      (length(vector) >=8)){ #if contains 3 or more Desktop/Laptop
    companies <- rbind(companies,transactions_df_PT[m,])      #store it in companies
    companies_row <- append(companies_row, m)
  }else{
    retail <- rbind(retail,transactions_df_PT[m,])
    retail_row <- append(retail_row, m)
  }
}
#pie charts 
table_retail <- table(as.matrix(retail))[3:11] #remove empty spaces
pct_retail <- round(table_retail/sum(table_retail)*100)
lbls_retail<-names(table_retail)
lbls_retail<-paste(lbls_retail,pct_retail,"%")
pie(table_retail, labels = lbls_retail, main="Sales by Product Type for Retail",col=rainbow(9))

table_companies <- table(as.matrix(companies))[3:11] #remove empty spaces
pct_companies <- round(table_companies/sum(table_companies)*100)
lbls_companies<-names(table_companies)
lbls_companies<-paste(lbls_companies,pct_companies,"%")
pie(table_companies, labels = lbls_companies, main="Sales by Product Type for Companies",col=rainbow(9))

#save the dataframes
write.table(companies, file = "companies_PT.csv", col.names = FALSE, row.names = FALSE, sep = ",")
write.table(retail, file = "retail_PT.csv", col.names = FALSE, row.names = FALSE, sep = ",")

#all transactions
transactions <- read.transactions("transactions_noempty.csv", sep =",", format("basket"),  rm.duplicates = TRUE)
summary(transactions)
#same as before but no empty transactions, 9833 transactions
#visualize dataset
#visualize items
itemFrequencyPlot(transactions) #too much items
itemFrequencyPlot(transactions,
                  support=0.1) #items with greater than or equal to 10% frequency 
#top 10 items
itemFrequencyPlot(transactions,
                  type="absolute", #maybe "relative"
                  topN=10,
                  #horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency, absolute')
#least frequenly bought
barplot(sort(table(unlist(LIST(transactions))))[1:10],
        horiz=TRUE,
        las=1,
        col='steelblue3',
        xlab='',
        main='Frequency, absolute')
#needs resize to see the labels
#visualize transactions
image(transactions[1:500]) #firsts 500 tranactions
image(sample(transactions, 100)) #100 random samples

#open transactions with product types
transactions_PT <- read.transactions("transactions_PT_all.csv", sep =",", format("basket"),  rm.duplicates = TRUE)
summary(transactions_PT)
itemFrequencyPlot(transactions_PT)

retail_PT <- read.transactions("retail_PT.csv", sep =",", format("basket"),  rm.duplicates = TRUE)
summary(retail_PT)
itemFrequencyPlot(retail_PT,topN=9,col="Steelblue3",main="Retail")

companies_PT <- read.transactions("companies_PT.csv", sep =",", format("basket"),  rm.duplicates = TRUE)
summary(companies_PT)
itemFrequencyPlot(companies_PT,topN=9,col="Steelblue3",main="Companies")

#New levels with product type
categories <- itemLabels(transactions)
for (k in 1:length(categories)){
  #label Laptop
  categories[k] <- gsub(".*Laptop.*","Laptop",categories[k])
  categories[k][categories[k]=="Acer Aspire"] <- "Laptop"
  categories[k][categories[k]=="ASUS Chromebook"] <- "Laptop"
  categories[k][categories[k]=="Apple MacBook Pro"] <- "Laptop"
  categories[k][categories[k]=="Apple MacBook Air"] <- "Laptop"
  #label Accesories
  categories[k] <- gsub(".*Mouse.*","Accessories",categories[k])
  categories[k][categories[k]=="Generic Black 3-Button"] <- "Accessories"
  categories[k] <- gsub(".*Keyboard.*","Accessories",categories[k])
  categories[k] <- gsub(".*Cable.*","Accessories",categories[k])
  categories[k][categories[k]=="HDMI Adapter"] <- "Accessories"
  categories[k] <- gsub(".*Stand.*","Accessories",categories[k])
  categories[k][categories[k]=="Full Motion Monitor Mount"] <- "Accessories"
  categories[k] <- gsub(".*Drive.*","Accessories",categories[k])
  #labels Audio
  categories[k] <- gsub(".*Head.*","Audio",categories[k])
  categories[k][categories[k]=="Apple Earpods"] <- "Audio"
  categories[k][categories[k]=="Monster Beats By Dr Dre"] <- "Audio"
  categories[k][categories[k]=="Monster Beats By Dr Dre "] <- "Audio"
  categories[k] <- gsub(".*Speaker.*","Audio",categories[k])
  categories[k][categories[k]=="DOSS Touch Wireless Bluetooth"] <- "Audio"
  categories[k][categories[k]=="Cyber Acoustics"] <- "Audio"
  categories[k][categories[k]=="Sonos"] <- "Audio"  
  #labels Software
  categories[k][categories[k]=="Microsoft Office Home and Student 2016"] <- "Software"
  categories[k][categories[k]=="Computer Game"] <- "Software"
  #labels Display
  categories[k] <- gsub(".*Monitor.*","Display",categories[k])
  #labels Printer Supplies
  categories[k] <- gsub(".*Ink.*","Ink",categories[k])
  categories[k][categories[k]=="Brother Printer Toner"] <- "Ink"
  categories[k][categories[k]=="DYMO Labeling Tape"] <- "Ink"
  categories[k][categories[k]=="DYMO Labeling Tape "] <- "Ink"
  #labels Printer
  categories[k] <- gsub(".*Printer.*","Printer",categories[k])
  categories[k][categories[k]=="DYMO Label Manker"] <- "Printer"
  categories[k][categories[k]=="DYMO Label Manker "] <- "Printer"
  categories[k][categories[k]=="DYMO Label Manker  "] <- "Printer"
  #labels Tablet
  categories[k][categories[k]=="iPad"] <- "Tablet"
  categories[k][categories[k]=="iPad "] <- "Tablet"
  categories[k][categories[k]=="iPad Pro"] <- "Tablet"
  categories[k][categories[k]=="iPad Pro "] <- "Tablet"
  categories[k][categories[k]=="Fire HD Tablet"] <- "Tablet"
  categories[k][categories[k]=="Samsung Galaxy Tablet"] <- "Tablet"
  categories[k][categories[k]=="Kindle"] <- "Tablet"
  categories[k][categories[k]=="Kindle "] <- "Tablet"
  #Smart Home devices
  categories[k][categories[k]=="Apple TV"] <- "Smart Home Devices"
  categories[k][categories[k]=="Google Home"] <- "Smart Home Devices"
  categories[k][categories[k]=="Smart Light Bulb"] <- "Smart Home Devices"
  categories[k][categories[k]=="Fire TV Stick"] <- "Smart Home Devices"
  categories[k][categories[k]=="Roku Express"] <- "Smart Home Devices"
  #Desktop
  categories[k] <- gsub(".*Desktop.*","Desktop",categories[k])
  categories[k][categories[k]=="iMac"] <- "Desktop"
  categories[k][categories[k]=="iMac "] <- "Desktop"
}

#make new level
transactions@itemInfo$categories <- categories

#New levels with product type according to electronidex
categories_electro <- itemLabels(transactions)
for (k in 1:length(categories_electro)){
  #label Laptop
  categories_electro[k] <- gsub(".*Laptop.*","Laptop",categories_electro[k])
  categories_electro[k][categories_electro[k]=="Acer Aspire"] <- "Laptop"
  categories_electro[k][categories_electro[k]=="ASUS Chromebook"] <- "Laptop"
  categories_electro[k][categories_electro[k]=="Apple MacBook Pro"] <- "Laptop"
  categories_electro[k][categories_electro[k]=="Apple MacBook Air"] <- "Laptop"
  #label Mouse and Keyboard
  categories_electro[k] <- gsub(".*Keyboard and Mouse.*","MK combo",categories_electro[k])
  categories_electro[k][categories_electro[k]=="Logitech Desktop MK120 Mouse and keyboard Combo"] <- "MK combo"
  #accessories
  categories_electro[k][categories_electro[k]=="Microsoft Office Home and Student 2016"] <- "Accessories"
  categories_electro[k][categories_electro[k]=="Computer Game"] <- "Accessories"
  categories_electro[k][categories_electro[k]=="Belkin Mouse Pad"] <- "Accessories"
  categories_electro[k][categories_electro[k]=="Large Mouse Pad"] <- "Accessories"
  #label mouse
  categories_electro[k] <- gsub(".*Mouse.*","Mouse",categories_electro[k])
  categories_electro[k][categories_electro[k]=="Generic Black 3-Button"] <- "Mouse"
  #label keyboard
  categories_electro[k] <- gsub(".*Keyboard.*","Keyboard",categories_electro[k])
  #computer cords
  categories_electro[k] <- gsub(".*Cable.*","Computer Cords",categories_electro[k])
  categories_electro[k][categories_electro[k]=="HDMI Adapter"] <- "Computer Cords"
  #computer satnds
  categories_electro[k] <- gsub(".*Stand.*","Accessories",categories_electro[k])
  categories_electro[k][categories_electro[k]=="Full Motion Monitor Mount"] <- "Computer Stand"
  #external hardrives
  categories_electro[k] <- gsub(".*Drive.*","External Hardrives",categories_electro[k])
  #Computer Headphones
  categories_electro[k] <- gsub(".*Headset.*","Computer Headset",categories_electro[k])
  categories_electro[k][categories_electro[k]=="Panasonic On-Ear Stereo Headphones RP-HT21"] <- "Computer Headset"
  categories_electro[k][categories_electro[k]=="Kensington Headphones"] <- "Computer Headset"
  categories_electro[k][categories_electro[k]=="Koss Home Headphones"] <- "Computer Headset"
  categories_electro[k][categories_electro[k]=="Ailihen Stereo Headphones"] <- "Computer Headset"
  #Active Headphones
  categories_electro[k] <- gsub(".*Headphone.*","Active Headphones",categories_electro[k])
  categories_electro[k][categories_electro[k]=="Apple Earpods"] <- "Active Headphones"
  categories_electro[k][categories_electro[k]=="Monster Beats By Dr Dre"] <- "Active Headphones"
  categories_electro[k][categories_electro[k]=="Monster Beats By Dr Dre "] <- "Active Headphones"
  #Speakers
  categories_electro[k] <- gsub(".*Speaker.*","Speakers",categories_electro[k])
  categories_electro[k][categories_electro[k]=="DOSS Touch Wireless Bluetooth"] <- "Speakers"
  categories_electro[k][categories_electro[k]=="Cyber Acoustics"] <- "Speakers"
  categories_electro[k][categories_electro[k]=="Sonos"] <- "Speakers"  
  #labels Display
  categories_electro[k] <- gsub(".*Monitor.*","Display",categories_electro[k])
  #labels Printer Supplies
  categories_electro[k] <- gsub(".*Ink.*","Ink",categories_electro[k])
  categories_electro[k][categories_electro[k]=="Brother Printer Toner"] <- "Ink"
  categories_electro[k][categories_electro[k]=="DYMO Labeling Tape"] <- "Ink"
  categories_electro[k][categories_electro[k]=="DYMO Labeling Tape "] <- "Ink"
  #labels Printer
  categories_electro[k] <- gsub(".*Printer.*","Printer",categories_electro[k])
  categories_electro[k][categories_electro[k]=="DYMO Label Manker"] <- "Printer"
  categories_electro[k][categories_electro[k]=="DYMO Label Manker "] <- "Printer"
  categories_electro[k][categories_electro[k]=="DYMO Label Manker  "] <- "Printer"
  #labels Tablet
  categories_electro[k][categories_electro[k]=="iPad"] <- "Tablet"
  categories_electro[k][categories_electro[k]=="iPad Pro"] <- "Tablet"
  categories_electro[k][categories_electro[k]=="Fire HD Tablet"] <- "Tablet"
  categories_electro[k][categories_electro[k]=="Samsung Galaxy Tablet"] <- "Tablet"
  categories_electro[k][categories_electro[k]=="Kindle"] <- "Tablet"
  #Smart Home devices
  categories_electro[k][categories_electro[k]=="Apple TV"] <- "Smart Home Devices"
  categories_electro[k][categories_electro[k]=="Google Home"] <- "Smart Home Devices"
  categories_electro[k][categories_electro[k]=="Smart Light Bulb"] <- "Smart Home Devices"
  categories_electro[k][categories_electro[k]=="Fire TV Stick"] <- "Smart Home Devices"
  categories_electro[k][categories_electro[k]=="Roku Express"] <- "Smart Home Devices"
  #Desktop
  categories_electro[k] <- gsub(".*Desktop.*","Desktop",categories_electro[k])
  categories_electro[k][categories_electro[k]=="iMac"] <- "Desktop"
  categories_electro[k][categories_electro[k]=="iMac "] <- "Desktop"
}

#make new level
transactions@itemInfo$categories_electro <- categories_electro

#new labels for brand?
#new labels for gamers?

#subset company and individual
transactions_companies <- transactions[companies_row]
transactions_individual <- transactions[retail_row]

#rules for companies, blackwell's categories
transactions_companies_cat<- aggregate(transactions_companies, transactions_companies@itemInfo[["categories"]])
summary(transactions_companies_cat)

rules_com_cat<- apriori(transactions_companies_cat,parameter = list(support=0.001,conf=0.2,minlen=2),
                        appearance = list(lhs=c("Desktop","Laptop"),
                                          rhs=c("Accessories","Audio","Printer","Smart Home Devices","Display","Software",
                                              "Ink","Tablet" ),default="none"))
rules_com_cat <- rules_com_cat[!is.redundant(rules_com_cat)]                         
# 11 rules :S

#rules for retail
transactions_indiv_cat<- aggregate(transactions_individual, transactions_individual@itemInfo[["categories"]])
summary(transactions_indiv_cat)

rules_indiv_cat<- apriori(transactions_indiv_cat,parameter = list(support=0.001,conf=0.2,minlen=2),
                          appearance = list(lhs=c("Desktop","Laptop")))
rules_indiv_cat <- rules_indiv_cat[!is.redundant(rules_indiv_cat)]                         
plot(rules_indiv_cat)
plot(rules_indiv_cat, method="grouped")

#rules for companies, electronidex cat
transactions_companies_electro<- aggregate(transactions_companies, transactions_companies@itemInfo[["categories_electro"]])
summary(transactions_companies_electro)

rules_com_electro<- apriori(transactions_companies_electro,parameter = list(support=0.001,conf=0.2,minlen=2),
                        appearance = list(lhs=c("Desktop","Laptop")))
rules_com_electro <- rules_com_electro[!is.redundant(rules_com_electro)]                         
# 17 rules :S

#rules for retail, electronidex cat
transactions_indiv_electro<- aggregate(transactions_individual, transactions_individual@itemInfo[["categories_electro"]])
summary(transactions_indiv_electro)

rules_indiv_electro<- apriori(transactions_indiv_electro,parameter = list(support=0.001,conf=0.2,minlen=2))
rules_indiv_electro <- rules_indiv_electro[!is.redundant(rules_indiv_electro)]                         
plot(rules_indiv_electro)


plot(rules_indiv_electro, method="grouped")



#Rules for retail
rules_retail <- apriori (retail_PT, parameter = list(supp = 0.001, conf = 0.2, minlen=2))
summary(rules_retail)
#find redundant rules
is.redundant(rules_retail)
#remove redundant
rules_retail <- rules_retail[!is.redundant(rules_retail)]
summary(rules_retail)
plot(rules_retail)
plot(rules_retail, method="grouped")



#ByCategoriesDL<- apriori(CategoriesTransaction,parameter = list(support=0.05,conf=0.5,minlen=2),
#                         appearance = list(rhs=c(“Desktops”),lhs=c(“Accessories”,“ActiveHeadphones”,“Cartridge”,“Computer Mice”,“ComputerCords”,“ComputerHeadphones”,“ExternalHD”,
#                                                                    “Keyboards”,“MKCombo”,“Monitors”,“Printers”,“SmartHomeDevices”,“Software”,“Speakers”,“Stands”,“Tablets” ),default=“none”))

#ByCategoriesDL<- apriori(CategoriesTransaction,parameter = list(support=0.001,conf=0.1,minlen=2),
#                         appearance = list(rhs=c(“Accessories”,“ActiveHeadphones”,“Cartridge”,“Computer Mice”,“ComputerCords”,“ComputerHeadphones”,“ExternalHD”,
#                                                  “Keyboards”,“MKCombo”,“Monitors”,“Printers”,“SmartHomeDevices”,“Software”,“Speakers”,“Stands”,“Tablets” ),lhs=c(c(“Desktops”) ),default=“none”))
#inspect(head(sort(ByCategoriesDL,by=“lift”),20))



#find redundant rules
is.redundant(rules_2)
#remove redundant
rules_2 <- rules_2[!is.redundant(rules_2)]
summary(rules_2)

#plot rules
rules_2_sorted <- sort(rules_2, by='support', decreasing = T)

plot(rules_2[1:3], method="graph", control=list(type="items")) 










#Apriori algorithm
rules_1 <- apriori (transactions, parameter = list(supp = 0.1, conf = 0.8))
#0 rules
#occurring in 1% transactions, with 80% correct
rules_2 <- apriori (transactions, parameter = list(supp = 0.001, conf = 0.8))
#635 rules
rules_2 <- apriori (transactions, parameter = list(supp = 0.001, conf = 0.9))
#197 rules
summary(rules_2)
inspect(sort(rules_2, by='support', decreasing = T)[1:5])
inspect(sort(rules_2, by='lift', decreasing = T)[1:5])

#find redundant rules
is.redundant(rules_2)
#remove redundant
rules_2 <- rules_2[!is.redundant(rules_2)]
summary(rules_2)

#plot rules
rules_2_sorted <- sort(rules_2, by='support', decreasing = T)
plot(rules_2[1:3], method="graph", control=list(type="items")) 




#see if products from Blackwells are in RHS

#what factors influenced the purchase of product x
#rules <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.08), appearance = list (default="lhs",rhs="whole milk"), control = list (verbose=F)) # get rules that lead to buying 'whole milk'
#rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
#inspect(head(rules_conf))



#ItemRules <- subset(RulesName, items %in% "item name")



#for i sdd:
#  for j dffits(
#    type[[i]][j] find element j in the list i from type
