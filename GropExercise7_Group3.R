library(data.table)
leons <-fread("LoanStats3a.csv", sep = ",", data.table = F, drop = 1, stringsAsFactors= T)

library(PivotalR)
library(DBI)
library(RPostgreSQL)

pgConn = dbConnect("PostgreSQL", dbname = "n054020006", host="127.0.0.1" ,user="n054020006", password="ndx82625")

ifelse(dbExistsTable(pgConn, "loans"), dbRemoveTable(pgConn, "loans"),
       dbWriteTable(pgConn, "loans", as.data.frame(leons)))
dbListTables(pgConn)
dbSendQuery(pgConn, "delete from loans where loan_status not in ('Fully Paid','Charged Off')")

loans = dbSendQuery(pgConn, "select * from loans")
numOfRow = nrow(loans)
#numOfRow=dbSendQuery(pgConn,"select count(*) from loans")

set.seed(1)

train_idx = sample(1:numOfRow, size = numOfRow * 0.7)
train_df = loans[train_idx, ]
test_df = loans[-train_idx, ]

LOOCV_lm_rmse = function(f, d){
  numOfRec = nrow(d);
  rmse_vec = rep(0,numOfRec); # n RMSEs
  reponse_var = all.vars(f)[1]; # Get the name of reponse variable
  
  for(i in 1:numOfRec){
    m = glm(formula=f, data=d[- i,],na.action = na.omit);
    rmse_vec[i] = (d[[reponse_var]][i]) - predict(m,newdata=d[i,]);
  }
  return( paste("LOOCV RMSE for lm(", format(f),") =", 
                round(sqrt(mean(rmse_vec ^ 2)),4)) );
}
LOOCV_lm_rmse(loan_status ~ loan_amt, train_df)