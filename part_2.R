
Practicum Group:
Name: Harsha Teja Gorijavolu 
NUID: 002160768
Email: Gorijavolu.h@northeastern.edu

Name: Saloni Patil
NUID: 001559860
patil.sal@northeastern.edu

Practicum 2: Part 2


```{r}
library(RMySQL)
library(tidyverse)
library(lubridate)
```

#Establish the connection to the MySQL Server       

```{r}
db_user <- 'admin'
db_password <- 'admin123'
db_name <- 'sandbox'
db_host <- 'dbs.crvfkwye7vwt.us-east-1.rds.amazonaws.com'
db_port <- 3306

aws <-dbConnect(MySQL(),dbname = db_name,user = db_user,
                 password = db_password,
                 host = db_host,
                 port=db_port)
```


Drop the dimension and fact tables if they exist

```{sql connection=aws}
DROP TABLE IF EXISTS author_fact
```
```{sql connection=aws}
DROP TABLE IF EXISTS authordim
```

Create the dimension and fact tables.

```{sql connection=aws}

CREATE TABLE IF NOT  EXISTS authordim(
  author_id INT NOT NULL PRIMARY KEY,
  lastName VARCHAR(50) NOT NULL,
  firstName VARCHAR(50) NOT NULL,
  initials VARCHAR(50) NOT NULL,
  affiliationType VARCHAR(100)
);

```


```{sql connection=aws}
CREATE Table IF NOT  EXISTS   author_fact(
author_id INT,
author_name varchar(200),
num_articles int NOT NULL,
avg_articles_per_year INT NOT NULL,

foreign key(author_id) references authordim(author_id));
```



Establish connection between part-1 database
```{r}
conn <- dbConnect(SQLite(), dbname="D://PubmedDB.db")
```

Extract data from the database from part 1 

```{r}
results<-dbGetQuery(conn, "select author_id,name,sum(cnt) as num_articles, avg(cnt) as avg_articles_per_year from (select alt.author_id, (lastName || ', ' || firstName) as name,year_created,count(*) as cnt from article inner join AuthorLookupTable alt on alt.article_id=article.article_id inner join author on author.author_id=alt.author_id group by alt.author_id,year_created) as temp group by author_id")
```
Verify the data
```{r}
results
```
Write the data into the fact table with necessary measures and facts
```{r}
dbWriteTable(aws, 'author_fact', results,overwrite = T,row.names=F)
```

Verify the data in the fact table
```{sql connection=aws}
select * from author_fact

```

Testing part 3 from the output
#### Extracting top 10 authors with most published articles, extracted from fact table.

###Part 3
```{sql connection=aws}
select * from author_fact order by num_articles desc limit 10; 

```

Disconnect the databases
  
  
```{r}
dbDisconnect(conn)
```

```{r}
dbDisconnect(aws)
```


