library(ISLR); data("Hitters") ;data("Default");

ifelse(dbExistsTable(pgConn, "default"), dbRemoveTable(pgConn, "default"),
       dbWriteTable(pgConn, "default", as.data.frame(diamonds)))

dbc = db.connect(dbname = "n054020016", user="n054020016", host="127.0.0.1",
                 password="xnmyq1ft", default.schema = "public, madlib", verbose = T)

db_Default = db.data.frame("Default", dbc, verbose = T)

db_Counter = by(db_Default,db_Default$student, count)
lk(db_Counter)