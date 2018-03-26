install.packages("sparklyr")
library(sparklyr)
hr<-read.csv( "D://Project/HR.csv")
spark_install(version = "2.1.0")
sc <- spark_connect(master = "local")
library(dplyr)
hr_tbl <- copy_to(sc, hr)


partitions <- hr_tbl %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 1099)
fit <- partitions$training %>%
  ml_random_forest(left ~ ., type = "classification")
  
  
  # predict from the model for the test data
  pred<-
  sdf_predict(fit,hr_tbl$test)
  
  c<-collect(pred)
  confusionMatrix(c$left,c[["prediction"]])

  


                                  
                                  
    spark_disconnect(sc)                              
