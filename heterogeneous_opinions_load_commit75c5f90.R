risultati <- matrix(nrow = 40, ncol=49)

db <- read.csv("deffuant_bench_distribution experiment distribution-table.csv",sep = ",",dec = ".",skip = 6)
db$parameters <- paste(db$mu, db$theta)



rindex <- 1

for (param in c("0.25 0.25","0.25 0.75","0.75 0.25","0.75 0.75")){
   
 print(param)  
 for (modal in c("static mu","change_mu")){
    
  
    
  for (step in c(1:5)){
    
  
   db_time <- db[db$X.step.== step,]
   db_time <- db_time[db_time$modality == modal,]
   db_time_parameters <- db_time[db_time$parameters == param,8]    

   m <- matrix(nrow = length(db_time_parameters), ncol = 49)

   for (i in c(1:length(db_time_parameters))) {

    db_time_parameters <- gsub("\\[", "", db_time_parameters)
    db_time_parameters <- gsub("\\]", "", db_time_parameters)
    a <- as.data.frame(strsplit(db_time_parameters[i], split = " "), )
    colnames(a)[1] <- "opinions"
    a$opinions <- as.double(a$opinions)

    a$opinions[a$opinions < 0] <- 0
    a$opinions[a$opinions > 1] <- 1
    b <- hist(a$opinions, breaks=seq(0,1,l=50), plot = FALSE)
    c <- b[[2]]

    m[i,] <- c

    }
  r <- colMeans(m)
  risultati[rindex,] <- r
  rindex <- rindex + 1
 }
}
}




