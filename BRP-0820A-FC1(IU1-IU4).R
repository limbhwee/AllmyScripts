#clear all values
rm(list=ls())

#using '<-' or '=' is the same but recommended to use <- throughout
a<- 2
a= 1
aa=2
aa<- 3
aa<-4

#case sensitive
A<- 1

#clear selected values
rm(a, A)
rm(aa)

#clear console by 'ctrl' + L

#R can do transformation, analysis, visualisation and prediction

#Vector
##1 dimensional; same data type; sequence of data elements
##3 types vector: numerical, character, logical
##dataset in a column and placed horizontally will become vector in R
##db reading data row by row, column by column, before reaching certain column data (designated record(s)) hence the reason for changing data set to vector without having to go through unnecessary records

##atomic vectors?##

#####numeric vector
no_of_cases_confirmed_local<- c(11,22,33,44,55,66,77)
no_of_cases_confirmed_local

no_of_cases_confirmed_imported<- c(21,12,13,24,25,26,35)
no_of_cases_confirmed_imported

#####per day
no_of_cases_confirmed_local<- c(11,22,33,44,55,66,77)
#####names() function straight away label the days to the number of cases
names(no_of_cases_confirmed_local)<- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
no_of_cases_confirmed_local

no_of_cases_confirmed_imported<- c(21,12,13,24,25,26,35)
#####same way can be done to also label the days to the number of cases by changing the name of variable to 'imported'
names(no_of_cases_confirmed_imported)<- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
no_of_cases_confirmed_imported

#####create a char vector (reusable) so that instead of copy and paste and change, days can be used instead
days<- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
names(no_of_cases_confirmed_local)<- days
names(no_of_cases_confirmed_imported)<- days
no_of_cases_confirmed_local
no_of_cases_confirmed_imported

#####arithmetic in vector
?sum()

total_imported<- sum(no_of_cases_confirmed_imported)
total_imported
total_local<- sum(no_of_cases_confirmed_local)
total_local

#####to see overall total regardless of days
overall_total<- total_imported + total_local
overall_total

#####to see by the days total
total_per_day<- no_of_cases_confirmed_local + no_of_cases_confirmed_imported
total_per_day

#####if vector length (number of records) not the same then recycling will occur hence it is important to fill up empty fields either by '0' or mean (will learn by in machine learning)

#####days above threshold values
imported_gt20<- no_of_cases_confirmed_imported > 20
imported_gt20
#####above will show by T or F
imported_gt20<- no_of_cases_confirmed_imported[no_of_cases_confirmed_imported > 20]
imported_gt20
#####above will show which days are > 20

#####days above threshold values
local_gt40<- no_of_cases_confirmed_local > 40
local_gt40
#####above will show by T or F
local_gt40<- no_of_cases_confirmed_local[no_of_cases_confirmed_local > 40]
local_gt40
#####above will show which days are > 40

#Matrix
##2dimensional, same data types, combination of 2 columns/ vectors, rows and columns
no_of_cases_confirmed_imported<- c(21,12,13,24,25,26,35)
no_of_cases_confirmed_imported
no_of_cases_confirmed_local<- c(11,22,33,44,55,66,77)
no_of_cases_confirmed_local
days<- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
names(no_of_cases_confirmed_local)<- days
names(no_of_cases_confirmed_imported)<- days
no_of_cases_confirmed_imported
no_of_cases_confirmed_local
#####above already named the days for local and imported

#####rbind combines 2 vectors into matrix or matrix and vector
no_of_cases_mat<- rbind(no_of_cases_confirmed_local, no_of_cases_confirmed_imported)
no_of_cases_mat
#####note that columns (days) already named

#####vectors always rem and case sensitive
no_of_cases_confirmed_imp<- c(21,12,13,24,25,26,35)
no_of_cases_confirmed_loc<- c(11,22,33,44,55,66,77)
#####days defined not included in the new data set
no_of_cases_mat2<- rbind(no_of_cases_confirmed_loc, no_of_cases_confirmed_imp)
no_of_cases_mat2

#####colnames() == column names
#####reassigning days defined
days<- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
colnames(no_of_cases_mat)<- days
no_of_cases_mat
#####note that the name is for matrix not the vectors which is why when the same set of data used for cbind, the rows are not named also

#####cbind combines 2 vectors into matrix or matrix and vector
no_of_cases_mat3<- cbind(no_of_cases_confirmed_loc, no_of_cases_confirmed_imp)
no_of_cases_mat3
#####rownames() == row names
rownames(no_of_cases_mat3)<- days
no_of_cases_mat3

#####cbind -> rownames() -> colSums() -> rbind (as seen between rows 131 to 137)
#####rbind -> colnames() -> rowSums() -> cbind

#####arithmetic in matrix is use colSums or rowSums instead of just sum() since this is matrix

weekly_sum<- colSums(no_of_cases_mat3)
weekly_sum
#####above result is a vector, new row

#####to combine weekly result (vector) to matrix
final_matrix1<- rbind(no_of_cases_mat3, weekly_sum)
final_matrix1

#####position of which comes first determines where it appears
final_matrix<- rbind(weekly_sum, no_of_cases_mat3)
final_matrix

#####using cbind
casetype_sum<- rowSums(no_of_cases_mat)
casetype_sum
#####above result is a vector, new column

#####to combine weekly result (vector) to matrix
final_matrix2<- cbind(no_of_cases_mat, casetype_sum)
final_matrix2

#####position of which comes first determines where it appears
final_matrix3<- cbind(casetype_sum, no_of_cases_mat)
final_matrix3

#####subsetting is reading the data/ value
final_matrix2[11]
final_matrix2[1,6]

#####[rownumber:rownumber, colnumber]
#####[rownumber, colnumber:colnumber]
#####weekdays local cases
final_matrix2[1,1:5]
#####weekdays imported cases
final_matrix2[2,1:5]

#####weekdays average of local cases
weekday_avg_loc<- mean(final_matrix2[1,1:5])
weekday_avg_loc

#####weekend average of local cases
weekend_avg_loc<- mean(final_matrix2[1,6:7])
weekend_avg_loc

#####weekdays average of imported cases
weekday_avg_imp<- mean(final_matrix2[2,1:5])
weekday_avg_imp

#####weekend average of imported cases
weekend_avg_imp<- mean(final_matrix2[2,6:7])
weekend_avg_imp

#####Sun average of cases
sun_avg_cases<- mean(final_matrix2[,7])
sun_avg_cases
