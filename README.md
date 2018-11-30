# R client for [Deribit API](https://www.deribit.com/docs/api/)

### Description

R API client for Deribit cryptocurrency exchange. Loosely based on [deribit-api-python](https://github.com/deribit/deribit-api-python) 

### Installation

This package is not on CRAN, so you'll have to use `devtools` package to install it from Git:

```
devtools::install_git("https://github.com/paltsev-p/rderibit")
```



### Public API

```
rderibit::getorderbook("BTC-28DEC18-3500-P", simplify = TRUE)
```

Public API methods:

```
getorderbook
getinstruments
getcurrencies
getsummary
index
stats
getlasttrades
```

### Private API

Obtain your `ACCESS_KEY` and `ACCESS_SECRET` keys from Deribit and construct the following list
```
keys <- list(key = ACCESS_KEY, secret = ACCESS_SECRET) 
```
```
rderibit::getopenorders(keys, simplify = F)
```

Private API methods:

```
account
buy
sell
cancel
cancelall
edit
getopenorders
positions
orderhistory
tradehistory
```
