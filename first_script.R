df_test <- data.frame(var_1 = rnorm(100,0,1), var_2 = rnorm(100,1,2), var_3 = rnorm(100,4,5), var_4 = rnorm(100,5,6))


df_test2 <- list(a = c(1,2,3), b = c(4,5,6))

df_test3 <- list(df_test, df_test2)

df_test4 <- unlist(df_test3)



# testing
# testing12
# testing123
