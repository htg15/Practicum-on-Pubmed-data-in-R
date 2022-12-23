
Practicum Group:
Name: Harsha Teja Gorijavolu 
NUID: 002160768
Email: Gorijavolu.h@northeastern.edu

Name: Saloni Patil
NUID: 001559860
patil.sal@northeastern.edu

Practicum 2: Part 1



### Loading the required packages
```{r}
library(XML)
library(RSQLite)
library(methods)
library(dplyr)
library(dbplyr)
```






### 1
 
Part 1 (40 pts) Load XML:  (3 pts / 1 hr) Create a normalized relational schema that contains the following entities/tables: Articles, Journals, Authors. Use the XML document to determine the appropriate attributes (fields/columns) for the entities (tables). While there may be other types of publications in the XML, you only need to deal with articles in journals. Create appropriate primary and foreign keys. Where necessary, add synthetic surrogate keys. For articles you should minimally store the article title (<ArticleTitle>) and month and year created (<DateCreated>); for journals store the journal name/title, volume, issue, and publication date. For authors you should store last name, first name, initial,

### Extract XML Data
```{r }
# parsing the xml file
xmlDoc  <- xmlParse("pubmed_sample.xml")
```

```{r}
# getting the root node of the tree
r <- xmlRoot(xmlDoc)
numArticles <- xmlSize(r)
numArticles
```
ERD:

https://lucid.app/lucidchart/8f09f768-e59e-4fc2-9325-9b7372bd2932/edit?invitationId=inv_fb6f8733-2335-40fd-8a17-4a684e7548f5&page=Nn6csn~ealVN#


#Connecting to Database 
```{r}
dbcon <- dbConnect(SQLite(), dbname="PubmedDB.db")

```

# Drop the existing tables before creating
```{sql connection=dbcon}
DROP TABLE IF EXISTS ISSNTable;
DROP TABLE IF EXISTS CitationTable;
DROP TABLE IF EXISTS AffiliationTable;
DROP TABLE IF EXISTS ArticlePubModelTable;

```
```{sql connection=dbcon}
DROP TABLE IF EXISTS JournalTable
```
```{sql connection=dbcon}
DROP TABLE IF EXISTS AuthorTable
```
```{sql connection=dbcon}
DROP TABLE IF EXISTS  ArticleTable
```
```{sql connection=dbcon}
DROP TABLE IF EXISTS  AuthorLookupTable
```

Create tables as per the normalized schema as depicted in the ERD.
```{sql connection=dbcon}
CREATE TABLE IF NOT  EXISTS ISSNTable(
  issn_id INT NOT NULL PRIMARY KEY,
  issn_type VARCHAR(20) NOT NULL
  );
```

```{sql connection=dbcon}
CREATE TABLE IF NOT  EXISTS CitationTable(
  cite_id INT NOT NULL PRIMARY KEY,
  cited_medium VARCHAR(20) NOT NULL
);
```

```{sql connection=dbcon}
CREATE TABLE IF NOT  EXISTS ArticlePubModelTable(
  articlepubmodel_id INT NOT  NULL PRIMARY KEY,
  pubType VARCHAR(20) NOT NULL
);
```

```{sql connection=dbcon}
--creating PublicationDateTable
CREATE TABLE IF NOT  EXISTS PublicationDateTable(
  pdt_id INT NOT NULL PRIMARY KEY,
  pdt_year INT NOT NULL,
  pdt_month INT NOT NULL
  );
```

```{sql connection=dbcon}
--creating journal table
CREATE TABLE IF NOT  EXISTS journal(
  journal_id INT NOT NULL PRIMARY KEY,
  title VARCHAR(100) NOT NULL,
  volume VARCHAR(30) NOT NULL,
  issue VARCHAR(30) NOT NULL,
  issn_id INT NOT NULL,
  cite_id INT NOT NULL,
  pdt_id INT NOT NULL,
  FOREIGN KEY (cite_id) REFERENCES CitationTableTable(cite_id),
  FOREIGN KEY (issn_id) REFERENCES ISSNTable(issn_id),
  FOREIGN KEY (pdt_id) REFERENCES PublicationDateTable(pdt_id)
);
```

```{sql connection=dbcon}
--creating author table
CREATE TABLE IF NOT  EXISTS author(
  author_id INT NOT NULL PRIMARY KEY,
  lastName VARCHAR(50) NOT NULL,
  firstName VARCHAR(50) NOT NULL,
  initials VARCHAR(50) NOT NULL,
  affiliationType VARCHAR(100)
);

```

```{sql connection=dbcon}
--creating article table
CREATE TABLE IF NOT  EXISTS article(
  article_id INT NOT NULL PRIMARY KEY,
  title VARCHAR(100) NOT NULL,
  month_created INT,
  year_created INT,
  pubtype VARCHAR(30),
  articlepubmodel_id INT NOT NULL,
  journal_id INT NOT NULL,
  author_id INT NOT NULL,
  FOREIGN KEY (journal_id) REFERENCES journal(journal_id),
  FOREIGN KEY (author_id) REFERENCES author(author_id)
);
```

```{sql connection=dbcon}
--creating AuthorLookupTable
CREATE TABLE IF NOT  EXISTS AuthorLookupTable(
  article_id INT NOT NULL,
  author_id INT NOT NULL,
  FOREIGN KEY (article_id) REFERENCES article(article_id),
  FOREIGN KEY (author_id) REFERENCES author(author_id),
  PRIMARY KEY(article_id,author_id)
);
```



### Extract the inputs  by traversing node by node using functions.

```{r}

Fileinput <- r[[1]]
numArticles <- xmlSize(r)
b<- xpathSApply(Fileinput, "//PubmedArticleSet/PubmedArticle/MedlineCitation/Article", xmlValue)
numArticles
num <- xmlSize(r)
`%notin%` = Negate(`%in%`)
num
```

Extraction functions for the remainder of the code.
```{r getterfunctions}
Fileinput <- r[[1]]


```

```{r}
extract_ISSNTable_issntype <- function(Fileinput){return 
(xpathSApply(Fileinput, ".//MedlineCitation/Article/Journal/ISSN/@IssnType"
             ))}
```

```{r}
extract_CitationTable_citemedium <- function(Fileinput){return 
(xpathSApply(Fileinput, ".//MedlineCitation/Article/Journal/JournalIssue/@CitedMedium"
             ))}
```

```{r}
extract_AffiliationTableaffln <- function(Fileinput){return 
(xpathSApply(Fileinput, ".//MedlineCitation/Article/AuthorList/Author/Affiliation", 
               xmlValue))}

```

```{r}
extract_articlepubmodel <- function(Fileinput){return 
  (xpathSApply(Fileinput, ".//MedlineCitation/Article/@PubModel"
               ))}
```

```{r}
extract_PublicationDateTable_pubdate_month <- function(Fileinput){return
(xpathSApply(Fileinput, ".//MedlineCitation/Article/Journal/JournalIssue/PubDate/Month",
               xmlValue))}

extract_PublicationDateTable_pubdate_year <- function(Fileinput){return 
(xpathSApply(Fileinput, ".//MedlineCitation/Article/Journal/JournalIssue/PubDate/Year",
               xmlValue))}

extract_Publicationmd <- function(Fileinput){return 
(xpathSApply(Fileinput, ".//MedlineCitation/Article/Journal/JournalIssue/PubDate/MedlineDate",
               xmlValue))}

```

```{r}
extract_journal_title <- function(Fileinput){return 
(xpathSApply(Fileinput, ".//MedlineCitation/Article/Journal/Title",
               xmlValue))}

extract_journal_issue_vol <- function(Fileinput){return 
(xpathSApply(Fileinput, ".//MedlineCitation/Article/Journal/JournalIssue/Volume",
               xmlValue))}


extract_journal_issue_issue <- function(Fileinput){return 
(xpathSApply(Fileinput, ".//MedlineCitation/Article/Journal/JournalIssue/Issue",
             xmlValue))}
```

```{r}
extract_author_completeyn <- function(Fileinput){return
(xpathSApply(Fileinput, ".//MedlineCitation/Article/AuthorList/@CompleteYN" ))}

extract_author_validyn <- function(Fileinput){return
(xpathSApply(Fileinput, ".//MedlineCitation/Article/AuthorList/Author/@ValidYN" 
               ))}

extract_author_lastname <- function(Fileinput){return 
(xpathSApply(Fileinput, ".//MedlineCitation/Article/AuthorList/Author/LastName", 
             xmlValue))}
extract_author_firstname <- function(Fileinput){return 
(xpathSApply(Fileinput, ".//MedlineCitation/Article/AuthorList/Author/ForeName", 
               xmlValue))}
extract_author_initials <- function(Fileinput){return 
(xpathSApply(Fileinput, ".//MedlineCitation/Article/AuthorList/Author/Initials", 
                xmlValue))}
```

```{r}
extract_article_title <- function(Fileinput){return
(xpathSApply(Fileinput, ".//MedlineCitation/Article/ArticleTitle", 
               xmlValue))}

extract_article_month <- function(Fileinput){return
(xpathSApply(Fileinput, ".//MedlineCitation/DateCreated/Month", 
               xmlValue))}

extract_article_year <- function(Fileinput){return
(xpathSApply(Fileinput, ".//MedlineCitation/DateCreated/Year", 
               xmlValue))}

```

### Double check the extractions to perform ETL

```{r testingfuncions}
Fileinput <- r[[2]]
extract_Publicationmd(Fileinput)

extract_articlepubmodel(Fileinput)

```

```{r}

extract_ISSNTable_issntype(Fileinput)


extract_CitationTable_citemedium(Fileinput)


extract_Publicationmd(Fileinput)
extract_AffiliationTableaffln(Fileinput) 
```

```{r}
extract_articlepubmodel(Fileinput) 
```

```{r}
extract_journal_title(Fileinput)

extract_journal_issue_vol(Fileinput)

extract_journal_issue_issue(Fileinput) 

```

```{r}
  extract_author_completeyn(Fileinput)
  extract_author_validyn(Fileinput)
  extract_author_lastname(Fileinput) 
  extract_author_firstname(Fileinput) 
  extract_author_initials(Fileinput)


```

```{r}
extract_article_title(Fileinput) 

extract_article_month(Fileinput) 

extract_article_year(Fileinput) 

```

```{r datafilling}
 numArticles <- xmlSize(r)

```

```{r}

ArticlePubModel.df <- data.frame(articlepubmodel_id = integer(),
                         pubType = character(),
                         stringsAsFactors = F)

```

```{r}
Citation.df<-data.frame (cite_id=integer(),
                        cited_medium=character(),
                        stringsAsFactors = F)
```

```{r}
ISSN.df<-data.frame (issn_id=integer(),
                     issn_type=character(),
                     stringsAsFactors = F)
```

```{r}
PublicationDateTable.df <- data.frame(pdt_id = integer(),
                         pdt_year = integer(),
                         pdt_month = character(),
                         stringsAsFactors = F)
```

```{r}

Journal.df <- data.frame (journal_id = integer(),
                          title = character(),
                          volume = character(),
                          issue = character(),
                          cite_medium = character(),
                          issn_type = character(),
                          issn_id = integer(),
                          cite_id = integer(),
                          pdt_id = integer(),
                          stringsAsFactors = F)
```
 
```{r}

Author.df <- data.frame (author_id = integer(),
                         lastName = character(),
                         firstName = character(),
                         initials = character(),
                         affiliationType= character(),
                         stringsAsFactors = F)

```
 
```{r}

Article.df <- data.frame (article_id = integer(),
                          title = character(),
                          month_created = integer(),
                          year_created = integer(),
                          pubtype = character(),
                          articlepubmodel_id = integer(),
                          journal_id = integer(),
                          stringsAsFactors = F)

```

````{r}

AuthorLookupTable.df <- data.frame (
                          #lookup_id = integer(),
                          article_id = integer(),
                          author_id = integer(),
                          stringsAsFactors = F)

```

ETL begins. Performs the necessary operations on the XML data to format it into a normalized database.
```{r}
issn_id <- 1
cite_id <- 1
articlepubmodel_id <-1
pdt_id <- 1
journal_id <- 1
author_id <- 1
article_id<- 1
```



```{r}
numArticles <- xmlSize(r)
numArticles
for (ii in 1:numArticles)
{
  Fileinput <- r[[ii]]
  d=extract_articlepubmodel(Fileinput)
  newarticlepubmodel_id <- c(articlepubmodel_id,d)
  ArticlePubModel.df[nrow(ArticlePubModel.df) + 1, ] <- newarticlepubmodel_id 
  h=extract_PublicationDateTable_pubdate_month(Fileinput) 
  i=extract_PublicationDateTable_pubdate_year(Fileinput)
  medlined = extract_Publicationmd(Fileinput)
  if(length(h) == 0 & length(i) == 0)
    {
      h <- substring(medlined,10,12)
      i <- substring(medlined,1,4)
    }
  newpdt_id <- c(pdt_id,i,h)                           
  PublicationDateTable.df[nrow(PublicationDateTable.df) + 1, ] <- newpdt_id 
  e=extract_journal_title(Fileinput)
  f=extract_journal_issue_vol(Fileinput)
  g=extract_journal_issue_issue(Fileinput)
  b=extract_CitationTable_citemedium(Fileinput)
  a=extract_ISSNTable_issntype(Fileinput)
  newjournal_id <- c(journal_id,e,f,g,b,a,0,0,pdt_id)      
  Journal.df[nrow(Journal.df) + 1, ] <- newjournal_id
  l=extract_author_lastname(Fileinput) 
  m=extract_author_firstname(Fileinput) 
  n=extract_author_initials(Fileinput)
  c=extract_AffiliationTableaffln(Fileinput)
  range_of_author=length(l)
  for(limit in 1:range_of_author)
    {
      lastn <- l[limit]
      firstn <- m[limit]
      init <- n[limit]
      if(length(Author.df$author_id[which(Author.df$lastName == lastn & Author.df$firstName == firstn & Author.df$initials == init )])>0)
      {
        tempauthorid <- Author.df$author_id[which(Author.df$lastName == lastn & Author.df$firstName == firstn & Author.df$initials == init )]
        tempauthorid <- as.integer(tempauthorid)
        newrowlookup <- c(article_id,tempauthorid)
        AuthorLookupTable.df [nrow(AuthorLookupTable.df) + 1, ] <- newrowlookup
      }
      else
      { 
        if(limit==1)
        {
           newrow <- c(author_id,lastn,firstn,init,c)      
           Author.df[nrow(Author.df) + 1, ] <- newrow
           newrowlookup <- c(article_id,author_id)
           AuthorLookupTable.df [nrow(AuthorLookupTable.df) + 1, ] <- newrowlookup
           author_id <- author_id + 1
        }
        else{
          newrow <- c(author_id,lastn,firstn,init,"")      
          Author.df[nrow(Author.df) + 1, ] <- newrow
          newrowlookup <- c(article_id,author_id)
          AuthorLookupTable.df [nrow(AuthorLookupTable.df) + 1, ] <- newrowlookup
          author_id <- author_id + 1
          
        }
        }
    }
  o=extract_article_title(Fileinput) 
  p=extract_article_month(Fileinput) 
  q=extract_article_year(Fileinput)
  d=extract_articlepubmodel(Fileinput)
  newarticle_id <- c(article_id,o,p,q,d,0,journal_id)
  Article.df[nrow(Article.df) + 1, ] <- newarticle_id
  issn_id <- issn_id + 1
  cite_id <- cite_id + 1
  articlepubmodel_id <- articlepubmodel_id + 1 
  pdt_id <- pdt_id+1
  journal_id <- journal_id + 1
  article_id <- article_id + 1
}
```
Once the data is in data frame, we have to perform data cleaning.

```{r}
PublicationDateTable.df
```


```{r}
table(PublicationDateTable.df$pdt_month)
```
.
```{r}
PublicationDateTable.df$pdt_month[ PublicationDateTable.df$pdt_month == "12" ] <- "Sep"
```

```{r}
table(PublicationDateTable.df$pdt_month)
```


```{r}
table(PublicationDateTable.df$pdt_year)
```

```{r}
PublicationDateTable.df
```

```{r}
Author.df
```

```{r}
Article.df
```

```{r}
AuthorLookupTable.df
```

```{r}
Journal.df
```

```{r}
Journal.df$issn_id[Journal.df$issn_type=='Print']<-1
Journal.df$issn_id[Journal.df$issn_type=='Electronic']<-2
Journal.df$cite_id[Journal.df$cite_medium=='Print']<-1
Journal.df$cite_id[Journal.df$cite_medium=='Internet']<-2
Journal.df
UpdatedJournal.df <- Journal.df[,c('journal_id','title','volume','issue','issn_id','cite_id','pdt_id')]
UpdatedJournal.df

```

```{r}

 cite_id <- 1
 newcite_id <- c(cite_id,"Print")
 Citation.df[nrow(Citation.df) + 1, ] <- newcite_id 
 cite_id <- 2
 newcite_id <- c(cite_id,"Internet")
 Citation.df[nrow(Citation.df) + 1, ] <- newcite_id

```

```{r}
Citation.df
```

```{r}

 issn_id <- 1
 newissn_id <- c(issn_id,"Print")
 ISSN.df[nrow(ISSN.df) + 1, ] <- newissn_id
 issn_id <- 2
 newissn_id <- c(issn_id,"Electronic")
 ISSN.df[nrow(ISSN.df) + 1, ] <- newissn_id 

```

```{r}
ISSN.df
```

Load the normalized data into the tables

```{r}
dbWriteTable(dbcon, 'PublicationDateTable', PublicationDateTable.df,overwrite = T)
dbWriteTable(dbcon, 'author', Author.df,overwrite = T)
dbWriteTable(dbcon, 'CitationTable', Citation.df,overwrite = T)
dbWriteTable(dbcon, 'journal', UpdatedJournal.df,overwrite = T)
dbWriteTable(dbcon, 'article', Article.df,overwrite = T)
dbWriteTable(dbcon, 'ISSNTable', ISSN.df,overwrite = T)
dbWriteTable(dbcon, 'AuthorLookupTable', AuthorLookupTable.df,overwrite = T)
```
```{sql connection=dbcon}
SELECT b.article_id,a.author_id,a.lastName, a.firstName
FROM author a 
JOIN AuthorLookupTable b
ON a.author_id = b.author_id;
```  
```{sql connection=dbcon}
select author_id,name, avg(cnt),sum(cnt) 
from 
(select alt.author_id, (lastName || ", " || firstName) as name,year_created,count(*) as cnt from article 
inner join AuthorLookupTable alt 
on alt.article_id=article.article_id 
inner join author
on author.author_id=alt.author_id
group by alt.author_id,year_created) as temp
group by author_id
```
```{sql connection=dbcon}
select * from author

```
  
  Disconnect the databases
```{r}
dbDisconnect(dbcon)
```

