df_test <- data.frame(var_1 = rnorm(100,0,1), var_2 = rnorm(100,1,2))


openxlsx:: write.xlsx(df_test, file = "df_test.xlsx", rowNames = TRUE)
