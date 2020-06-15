# clear workspace
rm(list = ls())

# load in data
product_data<- read.csv("BOP_extract_Copy.csv",header = TRUE)

#####################################
# Identify HFSS products in scope under PHE calorie and sugar reduction or SDIL

# remove out of scope product categories
product_data <- subset(product_data, Category!= "Drinks-Alcoholic")
product_data <- subset(product_data, Category!= "Oils")
product_data <- subset(product_data, Category!= "Fruit and vegetables")

# remove drinks which don't fall under SDIL
# work out if product is food or drink
product_data$TYPE <- NA
product_data$TYPE <- ifelse(grepl("Drinks", product_data$Category), "Drink", "Food")

# subset food and drinks
drinks045 <- subset(product_data, TYPE == "Drink")
foods045 <- subset(product_data, TYPE == "Food")

# identify if drinks contain added sugars
drinks045$add_sug <- NA
drinks045$add_sug <- ifelse(grepl("honey|Syrup|syrup|nectar|Glucose|glucose|Fructose|fructose|Sucrose|sucrose",drinks045$Description, 
                               drinks045$Ingredients,
                               drinks045$ingredients.2017), "TRUE",
                         ifelse(grepl("sugar|Sugar", drinks045$Ingredients, 
                                      drinks045$ingredients.2017), "TRUE",
                                "FALSE"))

# identify if drink falls under SDIL
drinks045$SDIL <- NA
drinks045$SDIL <- ifelse(drinks045$add_sug == TRUE & 
                        ((drinks045$LACT + drinks045$GALACT)/drinks045$TOTSUG)*100 <=75 &
                        drinks045$TOTSUG >=5, "Yes", "No")

# subset only drinks in scope (under SDIL)
drinks045<- subset(drinks045, SDIL == "Yes")

# remove unnecessary columns
drinks045 <- within(drinks045, rm(add_sug, SDIL))

# join foods with drinks
product_data <- rbind (foods045, drinks045)

#####################################

# code fruit and veg columns NA as 0
product_data$Fruit <- replace(product_data$Fruit, is.na(product_data$Fruit),0)
product_data$Veg <- replace (product_data$Veg, is.na(product_data$Veg),0)

# convert kcal to KJ
product_data$KJ <- product_data$KCALS*4.184

#####################################
# Calculate A points

# allocate A points for calories (KJ)
# create new column
product_data$A_KJ <- NA
product_data$A_KJ <- ifelse(product_data$KJ >= 3350,10,
                          ifelse(product_data$KJ >=3015,9,
                                 ifelse(product_data$KJ >= 2680,8,
                                        ifelse(product_data$KJ >= 2345,7,
                                               ifelse(product_data$KJ >=2010,6,
                                                      ifelse(product_data$KJ >=1675,5,
                                                             ifelse(product_data$KJ >=1340,4,
                                                                    ifelse(product_data$KJ >=1005,3,
                                                                           ifelse(product_data$KJ >=670,2,
                                                                                  ifelse(product_data$KJ >=335,1,0)
                                                                           )
                                                                    )
                                                             )
                                                      )
                                               )
                                        )
                                 )
                          )
)
                                                                    

# allocate A points for saturated fat
# create new column
product_data$A_satF <- NA
product_data$A_satF <- ifelse(product_data$SATFOD >= 10,10,
                                  ifelse(product_data$SATFOD >=9,9,
                                         ifelse(product_data$SATFOD >= 8,8,
                                                ifelse(product_data$SATFOD >= 7,7,
                                                       ifelse(product_data$SATFOD >=6,6,
                                                              ifelse(product_data$SATFOD >=5,5,
                                                                     ifelse(product_data$SATFOD >=4,4,
                                                                            ifelse(product_data$SATFOD >=3,3,
                                                                                   ifelse(product_data$SATFOD >=2,2,
                                                                                          ifelse(product_data$SATFOD >=1,1,0)
                                                                                   )
                                                                            )
                                                                     )
                                                              )
                                                       )
                                                )
                                         )
                                  )
)


# allocate A points for total sugar
# create new column
product_data$A_sug <- NA
product_data$A_sug <- ifelse(product_data$TOTSUG >= 45,10,
                            ifelse(product_data$TOTSUG >=40,9,
                                   ifelse(product_data$TOTSUG >= 36,8,
                                          ifelse(product_data$TOTSUG >= 31,7,
                                                 ifelse(product_data$TOTSUG >=27,6,
                                                        ifelse(product_data$TOTSUG >=22.5,5,
                                                               ifelse(product_data$TOTSUG >=18,4,
                                                                      ifelse(product_data$TOTSUG >=13.5,3,
                                                                             ifelse(product_data$TOTSUG >=9,2,
                                                                                    ifelse(product_data$TOTSUG >=4.5,1,0)
                                                                             )
                                                                      )
                                                               )
                                                        )
                                                 )
                                          )
                                   )
                            )
)

# allocate A points for sodium (NA)
# create new column
product_data$A_NA. <- NA
product_data$A_NA. <- ifelse(product_data$NA. >= 900,10,
                           ifelse(product_data$NA. >= 810,9,
                                  ifelse(product_data$NA. >= 720,8,
                                         ifelse(product_data$NA. >= 630,7,
                                                ifelse(product_data$NA. >= 540,6,
                                                       ifelse(product_data$NA. >= 450,5,
                                                              ifelse(product_data$NA. >= 360,4,
                                                                     ifelse(product_data$NA. >= 270,3,
                                                                            ifelse(product_data$NA. >= 180,2,
                                                                                   ifelse(product_data$NA. >= 90,1,0)
                                                                            )
                                                                     )
                                                              )
                                                       )
                                                )
                                         )
                                  )
                           )
)


# calculate total A points
product_data$A_TOTAL <- product_data$A_KJ + product_data$A_satF +product_data$A_sug + product_data$A_NA.

##############################
# calculate C points (positive nutrients)

# calculate total fruit, veg and nut %

# assign value for nuts - excludes composite dishes containing nuts
product_data$Nut <- NA
product_data$Nut <- ifelse(product_data$Category=="Fruit and vegetables" & grepl("nut", product_data$Description), 100,0)

product_data$FVN <- product_data$Fruit + product_data$Veg + product_data$Nut
# ensure a maximum of 100%
product_data$FVN <- ifelse(product_data$FVN>100, 100, product_data$FVN)

# allocate C points for F&V
product_data$C_FVN <- NA
product_data$C_FVN <- ifelse(product_data$FVN >= 80,5,
                            ifelse(product_data$FVN >= 60,2,
                                   ifelse(product_data$FVN >= 40,1,0)
                            )
)
                                                                           
# allocate points for AOAC fibre
product_data$C_FIB <- NA
product_data$C_FIB <- ifelse(product_data$AOACFIB >= 4.7,5,
                             ifelse(product_data$AOACFIB >= 3.7,4,
                                    ifelse(product_data$AOACFIB >= 2.8,3,
                                           ifelse(product_data$AOACFIB >= 1.9,2,
                                                  ifelse(product_data$AOACFIB >- 0.9,1,0)
                                           )
                                    )
                             )
)

# allocate points for protein
product_data$C_PRO <- NA
product_data$C_PRO <- ifelse(product_data$PROT >= 8,5,
                             ifelse(product_data$PROT >= 6.4,4,
                                    ifelse(product_data$PROT >= 4.8,3,
                                           ifelse(product_data$PROT >= 3.2,2,
                                                  ifelse(product_data$PROT >- 1.6,1,0)
                                           )
                                    )
                             )
)

#########################
# calculate total score for 2004/5 NPM

# create new column
product_data$TOTAL_04_5 <- NA

# check if food scores less that 11 A points
product_data$A_less11 <- NA
product_data$A_less11 <- ifelse(product_data$A_TOTAL < 11, TRUE,
                                ifelse(product_data$A_TOTAL >= 11, FALSE, NA))

# calculate total score for foods which score less than 11 'A' points
# unless A > 11 but also scores 5 or more for fruit veg nuts
# unless A > 11 and scores less than 5 for fruit veg nuts
product_data$TOTAL_04_5 <- ifelse(product_data$A_less11 == TRUE, product_data$A_TOTAL - (product_data$C_FVN +
                                                                                           product_data$C_FIB +
                                                                                           product_data$C_PRO), 
                                  ifelse(product_data$A_less11 == FALSE & product_data$C_FVN ==5, 
                                         product_data$A_TOTAL - (product_data$C_FVN +
                                                                   product_data$C_FIB +
                                                                   product_data$C_PRO), 
                                         ifelse(product_data$A_less11 == FALSE & product_data$C_FVN <5,
                                                product_data$A_TOTAL - (product_data$C_FVN +
                                                                          product_data$C_FIB),NA)))
                                                                                                                    

#########################
# work out whether a product complies with NPM cut-offs or not
# assign if food or drink to work out which cut-off applies

product_data$TYPE <- NA
product_data$TYPE <- ifelse(grepl("Drinks", product_data$Category), "Drink", "Food")

product_data$PASS <- NA
product_data$PASS <- ifelse(product_data$TYPE == "Drink" & product_data$TOTAL_04_5 <1,"Yes",
                          ifelse(product_data$TYPE == "Drink" & product_data$TOTAL_04_5>=1, "No",
                                 ifelse(product_data$TYPE == "Food" & product_data$TOTAL_04_5 <4, "Yes",
                                        ifelse(product_data$TYPE == "Food" & product_data$TOTAL_04_5 >=4, "No", "Unclassified"))))


# subset data to select those that pass or fail under the 2004/5 NPM
pass045 <- subset (product_data, product_data$PASS == "Yes")
fail045 <- subset (product_data, product_data$PASS == "No")
unclassified045 <- subset (product_data, product_data$PASS == "Unclassified")

# write out data
write.csv(product_data, file = "20045NPM.csv")

##############################
