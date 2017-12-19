
dat07 <- read.csv('../data/hiv xwas Zambia07 male v7.csv')
dsn <- svydesign(ids=~mv021, probs=~mv005, nested=T, data=dat07)
svymean(~hiv03, dsn)

dat13 <- read.csv('../data/hiv xwas Zambia13 male v7.csv')
dsn <- svydesign(ids=~mv021, probs=~mv005, nested=T, data=dat13)
svymean(~hiv03, dsn)
