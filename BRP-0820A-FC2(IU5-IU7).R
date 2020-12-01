no_of_cases_confirmed_imp<- c(21,12,13,24,25,26,35)
no_of_cases_confirmed_loc<- c(11,22,33,44,55,66,77)
#vector and factor nationality
nat_vec<- c('Singapore', 'India', 'Malaysia', 'India', 'Singapore', 'India', 'India')
nat_fac<- factor(nat_vec)
nat_fac

#vector and factor month
mon_vec<- c('March', 'April', "June", 'Apr', 'Sep', 'September', 'Jul')
mon_fac<- factor(mon_vec)
mon_fac
mon_fac<- as.factor(mon_vec)
mon_fac
#above way of factoring yield the same result
#but above data not clean since data of diff spelling

#TAKE NOTE FOR THE PROJECT PURPOSE
#trim data to only 3 characters and store in same factor during the conversion of vector to factor
#use function 'substr' i.e. substring
?substr
mon_fac<- as.factor(substr(mon_vec,1,3))
#above 1,3 means start first and then end 3rd characters
mon_fac
#however the result doesnt reflect in correct month order
#to do the ordering correctly
source_vec<- c('Imported', 'Local', 'Unlinked', 'Local', 'Imported', 'Unlinked')
source_fac<- factor(source_vec, levels = c('Unlinked', 'Local', 'Imported'))
#note that above order is manually done by specifying so otherwise it will be in alphabetical order
source_fac
str(source_fac)
#note that the result from str will show how the data is positioned so 3,2,1,2,3,1 is coincide to unlinked (1), local(2), imported (3)

size_vec<- c('XL', "L", "XL", "L", "M", "S")
size_fac<- factor(size_vec)
size_fac
str(size_fac)
#if never specify then will be alphabetical order
size_fac<- factor(size_vec, levels = c('S', 'M', 'L', 'XL'))
size_fac
str(size_fac)

blood<- c('B', 'AB', 'O', 'A', 'O', 'O', 'A','B')
blood_factor<- factor(blood)
blood_factor
str(blood_factor)
blood_factor<- factor(blood, levels = c('O', 'A', 'B', 'AB'), labels = c('BT_O', 'BT_A', 'BT_B', 'BT_AB'))
str(blood_factor)

#Lists
#combination of character, vector, numeric
#below is data entry
patient_list<- list('ABC', 45, '3 Jul', 'India')
#below is to give column names so that instead of seeing [[]] you see the column names instead
names(patient_list)<- c('Name', 'Age', 'DOB', 'Nationality')
patient_list
#str is to find the data type
str(patient_list)
#accessing both label ('Age') of data ('ABC') via single []
patient_list[2]
#but if state double [[]] then only the data (of 'ABC') w/o label (i.e. 'Age')
patient_list[[2]]

#Data frames
#combination of character, vector, numeric in table format
clust_vec<- c('ABC Church', 'ABC Dorm', 'XYZ Dorm', '13 Lodge', '13 Lodge', 'XYZ Dorm')
class(clust_vec)
hospitalised_vec<- c(11,22,33,44,24,35)
class(hospitalised_vec)
isolated_vec<- c(5,12,13,14,34,15)
class(isolated_vec)
source_vec<- c('Imported', 'Imported', 'Unlinked', 'Unlinked', 'Local', 'Local')
class(source_vec)
mon_vec<- c('March', 'Apr', "June", 'April', 'September', 'July')
class(mon_vec)
covid_df<- data.frame(clust_vec, source_vec, mon_vec, hospitalised_vec, isolated_vec)
covid_df
str(covid_df)

rm(covid_df)
#adding names right at the beginning in 1 coding
covid_df<- data.frame('Cluster_Name' = clust_vec, 'Source_Link' = source_vec, 'Month' = mon_vec, "Hospitalised_Num" = hospitalised_vec, 'Isolated_Num' = isolated_vec, stringsAsFactors = F)
covid_df
str(covid_df)

rm(covid_df)
#changing column names after df created 
covid_df<- data.frame(clust_vec, source_vec, mon_vec, hospitalised_vec, isolated_vec)
names(covid_df)<- c('Cluster Name', 'Source Link', 'Month', "Hospitalised Num", 'Isolated Num')
covid_df
str(covid_df)

#when creating df, by default, system converts character vec to factor hence to avoid this, need to include stringsAsFactors = F
#reason for this is due to sometimes data still not cleaned up

mon_fac<- as.factor(substr(mon_vec,1,3))
mon_fac

covid_df
str(covid_df)
#note that even if change mon to factor the str still not reflect as need to specify to the df
covid_df$mon_vec<- as.factor(substr(mon_vec,1,3))
str(covid_df)
#note that the column name has been changed from mon_vec to Month hence the reference to the df has to be updated also otherwise an extra column shows up as per result
rm(covid_df)
covid_df<- data.frame('Cluster_Name' = clust_vec, 'Source_Link' = source_vec, 'Month' = mon_vec, "Hospitalised_Num" = hospitalised_vec, 'Isolated_Num' = isolated_vec, stringsAsFactors = F)
covid_df$Month<- as.factor(substr(mon_vec,1,3))
covid_df
str(covid_df)

#accessing df via $
#if nothing specified then all details of the column will be out
covid_df$`Cluster Name`
#to specify the exact location within the column
covid_df$`Cluster Name`[1]
#to specify the exact data
covid_df[1,2]
#for the entire column
covid_df[,3]
#for the entire row
covid_df[3,]

rm(covid_df)
covid_df<- data.frame('Cluster_Name' = clust_vec, 'Source_Link' = source_vec, 'Month' = mon_vec, "Hospitalised_Num" = hospitalised_vec, 'Isolated_Num' = isolated_vec, stringsAsFactors = F)
covid_df$Month<- as.factor(substr(mon_vec,1,3))
str(covid_df)
#visualisation
#category (cluster, source, month) + KPI (hosp number, isolated number) wrt covid df
plot(covid_df$Cluster_Name, covid_df$Hospitalised_Num)
plot(covid_df$Source_Link, covid_df$Isolated_Num)
plot(covid_df$Month, covid_df$Hospitalised_Num, covid_df$Isolated_Num)
plot(covid_df$Hospitalised_Num, covid_df$Isolated_Num)
#can only plot if factors
covid_df$Cluster_Name<- as.factor(clust_vec)
covid_df$Source_Link<- as.factor(source_vec)
str(covid_df)
plot(covid_df$Cluster_Name, covid_df$Hospitalised_Num)
plot(covid_df$Source_Link, covid_df$Isolated_Num)
plot(covid_df$Cluster_Name)
plot(covid_df$Source_Link)
plot(covid_df$Month)
#by default, category will show bar charts and number will show scatter 
plot(covid_df$Hospitalised_Num)
plot(covid_df$Isolated_Num)
?plot

#par() function is for standardisation
par(col = 'blue')
plot(covid_df$Hospitalised_Num, covid_df$Isolated_Num)
plot(covid_df$Isolated_Num, covid_df$Hospitalised_Num)
par()$col
par(col.main = 'red')
plot(covid_df$Hospitalised_Num, covid_df$Isolated_Num, main = 'Numbers(1)', pch = 10, lty = 0, cex.axis = 2)
plot(covid_df$Isolated_Num, covid_df$Hospitalised_Num, main = 'Numbers(2)', pch = 25, lty = 0, cex.axis = 0.5)
par()$col.main
par()

hist(covid_df$Hospitalised_Num, breaks = 5)
hist(covid_df$Isolated_Num, breaks = 20)


pairs(covid_df)
#whatever fields are being plotted using 'pairs'