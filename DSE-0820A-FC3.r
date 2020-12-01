rm(list=ls())

df1 = data.frame(CustomerId = c(1:6), Product = c("Oven","Television","Mobile","WashingMachine","Lightings","Ipad"))
df2 = data.frame(CustomerId = c(2, 4, 6, 7, 8), State = c("California","Newyork","Santiago","Texas","Indiana")) 
df1
df2

#left (inner) join
df_left = merge(x=df1,y=df2,by="CustomerId")
df_left
df_inner=df1 %>% inner_join(df2,by="CustomerId")
df_inner


#outer join
df_outer = merge(x=df1,y=df2,by="CustomerId",all=TRUE)
df_outer
df_full= df1 %>% full_join(df2,by="CustomerId")
df_full

#left join
df_left2 = merge(x=df1,y=df2,by="CustomerId",all.x=TRUE)
df_left2
df_left3= df1 %>% left_join(df2,by="CustomerId")
df_left3

#right join
df_right = merge(x=df1,y=df2,by="CustomerId",all.y=TRUE)
df_right
df_right2= df1 %>% right_join(df2,by="CustomerId")
df_right2

#cross join
#cross join (also sometimes known as a Cartesian Join) 
#results in every row of one table being joined to every row of another table
df_cross = merge(x = df1, y = df2, by = NULL)
df_cross

#semi-join
#This is like inner join, 
#with only the left dataframe columns and values are selected
df_semi= df1 %>% semi_join(df2,by="CustomerId")
df_semi

#anti-join
#this join is like df1-df2, 
#as it selects all rows from df1 that are not present in df2
df_anti= df1 %>% anti_join(df2,by="CustomerId")
df_anti
