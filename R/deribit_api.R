# Porting of deribit-api-python (https://github.com/deribit/deribit-api-python/blob/master/deribit_api.py) with some additions

#### HELPER FUNCTIONS ####

#' Helper function to send requests to the API. If private methods are used, provide corresponding API keys
#' @param keys list containing access and  secret keys in the form: keys <- list(key = ACCESS_KEY, secret = ACCESS_SECRET)
#' @param action Action URI
#' @param data additional data(options) provided as list of vectors (of length one, typically)
#' use httr::with_verbose(rderibit::request(action = "/api/v1/private/cancel", keys = keys, data = list(orderId = "6306623820") ) ) to troubleshoot the connection to API

request <- function(keys = NULL, action, data = NULL, simplify = TRUE) {

  # to set logger to DEBUG:
  # futile.logger::flog.threshold(futile.logger::DEBUG, name = "rderibit")
  futile.logger::flog.debug(keys, name = "rderibit")
  # response = None
  if (startsWith(action, "/api/v1/private/")) {
      assertthat::assert_that(!is.null(keys$key), !is.null(keys$secret), msg = "Key or secret empty" )

    apiSignature <- generate_signature(keys, action, data)
    response <- httr::GET( httr::add_headers(.headers = c('x-deribit-sig' = apiSignature)),
                            url = paste0("https://www.deribit.com/", action),
                           query = data)
 } else  response <- httr::GET( url = paste0("https://www.deribit.com/", action), query = data )

 if (simplify) {jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)
  } else  return(httr::content(response))

  # TODO Add error handling for responses as in this Python example
  # if response.status_code != 200:
  #   raise Exception("Wrong response code: {0}".format(response.status_code))
  #
  # json = response.json()
  #
  # if json["success"] == False:
  #   raise Exception("Failed: " + json["message"])
  #
  # if "result" in json:
  #   return json["result"]
  # elif "message" in json:
  #   return json["message"]
  # else:
  #   return "Ok"

  }

generate_signature <- function(keys, action, data, nonceoverride = F) {

  #  added for debugging using API request generator
  if (nonceoverride == FALSE) { nonce <- as.character( round(as.numeric(Sys.time())*1000) )
  } else {
    assertthat::assert_that(length(nonceoverride) == 1)
    assertthat::is.count(nonceoverride)
    nonce <- as.numeric(nonceoverride)
  }

  concat_param <- ""

  if (!is.null(data)) {
    assertthat::assert_that(typeof(data) == "list")

    for (i in sort(names(data))) {
      # the parameters (param1, param2, …) must be sorted alphabetically according to the parameters’ name

      assertthat::assert_that(length(data[[i]]) > 0)

      # if the X-th value (corresponding to the valueX) is of array type, for instance [’a’, ’b’, ’c’], the entries should be concatenated in a single string, i.e., the resulting valueX = ’abc’

      concat_param <- paste0(concat_param, "&", i, "=", paste(data[[i]], collapse = '') )
    }
  }

  string <- paste0("_=", nonce, "&_ackey=", keys$key, "&_acsec=", keys$secret, "&_action=", action, concat_param)

  # set futile.logger::flog.threshold(futile.logger::DEBUG, name='rderibit') to see the debug messages
  futile.logger::flog.debug(string, name = "rderibit")

  # digest::digest() didnt' work, using openssl::sha256() instead
  apiSignature <- paste0(apiKey, ".", nonce, ".", RCurl::base64(openssl::sha256(charToRaw(string))))

  futile.logger::flog.debug(apiSignature, name = "rderibit")

  return(apiSignature)
}


#### PUBLIC API ####

#' @export
getorderbook <- function(instrument, simplify = TRUE) {
  return(request(action = "/api/v1/public/getorderbook", data = list(instrument = instrument), simplify = simplify))
}

#' @export
getinstruments <- function(simplify = TRUE) { return(request(action = "/api/v1/public/getinstruments", simplify = simplify))
  }

#' @export
getcurrencies <- function(simplify = TRUE) { return(request(action = "/api/v1/public/getcurrencies", simplify = simplify))
  }

#' @export
getsummary <- function(instrument) { return(request(action = "/api/v1/public/getsummary", data = list(instrument = instrument)))
}

#' @export
index <- function() { return(request(action = "/api/v1/public/index"))
}

#' @export
stats <- function() { return(request(action = "/api/v1/public/stats"))
}

#' Retrieve the latest trades that have occured for a specific instrument.
#' @param instrument  string [optional], instrument name, example "BTC-28SEP18-3500-P" (or other items in rderibit::getinstruments()). Can as well be "all", "options", "futures", "BTC-THISWEEK", "BTC-NEXTWEEK"
#' @param startTimestamp timestamp of starting trade in milliseconds
#' @param endTimestamp timestamp of ending trade in milliseconds
#' @param startId – integer [optional], “since” tradeId, the server returns trades newer than that “since”.
#' @param endId - integer [optional], the server returns trades with tradeId smaller than the provided number.
#' @param startSeq relates to tradeSeq parameter
#' @param endSeq relates to tradeSeq parameter
#' @param count – integer [optional], count of trades returned (limitation: max. count is 100000, default 100)
#' @examples getlasttrades(instrument = "BTC-28SEP18-3500-P", startTimestamp = as.numeric(as.POSIXlt("2018-08-05 18:00:00 UTC"))*1000)
#'
#' @export
#'
getlasttrades <- function(instrument, count = NULL, startTimestamp = NULL, endTimestamp = NULL, startId = NULL,
                          endId = NULL, startSeq = NULL, endSeq = NULL, simplify = TRUE) {
  options <- list(instrument = instrument)

  if (!is.null(count))
      options[['count']] <- count

  if (!is.null(startTimestamp))
    options[['startTimestamp']] <- startTimestamp

  if (!is.null(endTimestamp))
    options[['endTimestamp']] <- endTimestamp

  if (!is.null(startId))
    options[['startId']] <- startId

  if (!is.null(endId))
    options[['endId']] <- endId

  if (!is.null(startSeq))
    options[['startSeq']] <- startSeq

  if (!is.null(endSeq))
    options[['endSeq']] <- endSeq


  return(request(action = "/api/v1/public/getlasttrades", data = options, simplify = simplify))
}


#### PRIVATE API ####

#' @export
account <- function(keys) { return (request(action = "/api/v1/private/account", keys = keys) ) }

#' Place a buy order in an instrument (authorization is required).
#' @param keys list containing access and  secret keys in the form: keys <- list(key = ACCESS_KEY, secret = ACCESS_SECRET)
#' @examples rderibit::buy(keys, "BTC-28DEC18-8750-P", postOnly = T, quantity = 0.1, price = .005)
#' @export
buy <- function(keys, instrument, quantity, price, postOnly = FALSE, label= NULL) {
  options <- list(
    instrument = instrument,
    quantity = quantity,
    price = price )

  if (!is.null(label))
    options[['label']] <- label

  if (postOnly == TRUE)
    options[["postOnly"]] <- postOnly

  return(request(keys = keys, action = "/api/v1/private/buy", data = options))

}



#' Places a sell order in an instrument (authorization is required).
#' @param keys list containing access and  secret keys in the form: keys <- list(key = ACCESS_KEY, secret = ACCESS_SECRET)
#' @examples rderibit::sell(keys, "BTC-28DEC18-8750-P", postOnly = T, quantity = 0.1, price = .005)
#' @export
sell <- function(keys, instrument, quantity, price, postOnly = FALSE, label= NULL) {
  options <- list(
    instrument = instrument,
    quantity = quantity,
    price = price )

  if (!is.null(label))
    options[['label']] <- label

  if (postOnly == TRUE)
    options[["postOnly"]] <- postOnly

  return(request(keys = keys, action = "/api/v1/private/sell", data = options))
}

#' @export
cancel <- function(keys, orderId){ return(request(keys = keys, action = "/api/v1/private/cancel", data = list(orderId = orderId) ) )
}

#' Bulk cancel orders based on type or instrument
#' @param typeDef Cancel instructions. Can be "all", "options", "futures" or NULL. Will be ignored if instrument is not NULL
#' @param instrument  string [optional], instrument name, example "BTC-28SEP18-3500-P" (or other items in rderibit::getinstruments()).
#' @examples cancelall(keys, "BTC-28DEC18-8750-P")
#' @export

cancelall <- function(keys, instrument = NULL, typeDef= NULL) {
  if (is.null(typeDef) & is.null(instrument) ) {
    warning("rderibit::cancelall(): Either typeDef or instrument should be defined. No request is sent, returning NULL")
    return(NULL)
  }

  if ( !is.null(typeDef) & !is.null(instrument) ) {
    warning("rderibit::cancelall(): As instrument is defined, input for typeDef is ignored")
    options <- list(instrument = instrument)
  } else if (!is.null(typeDef)) { options <- list(type = typeDef)
    } else if (!is.null(instrument)) options <- list(instrument = instrument)

  return(request(keys, "/api/v1/private/cancelall", data = options ) )
}



#' @export

edit <- function(keys, orderId, quantity, price) {

  options < list(
    orderId = orderId,
    quantity = quantity,
    price = price )

  return(request(keys, "/api/v1/private/edit", data = options) )
}



#' @export

getopenorders <- function(keys, instrument = NULL, orderId = NULL, simplify = TRUE) {

  options <- list()
  if (!is.null(instrument))  options[["instrument"]] <- instrument
  if (!is.null(orderId))  options[["orderId"]] <- orderId

  return(request(action = "/api/v1/private/getopenorders", keys = keys, data = options, simplify = simplify))
}


#' @export
positions <- function(keys) { return (request(action = "/api/v1/private/positions", keys = keys) ) }

#' @export
orderhistory <- function(keys, count = NULL, simplify = TRUE) {

  options <- list()
  if (!is.null(count))  options[["count"]] <- count

  return(request(action = "/api/v1/private/orderhistory", keys = keys, data = options, simplify = simplify))
}


#' @export

tradehistory <- function(keys, instrument = "all", count = NULL, startTradeId = NULL, simplify = TRUE) {

  options <- list(instrument = instrument)

  if (!is.null(count))  options[["count"]] <- count
  if (!is.null(startTradeId))  options[["startTradeId"]] <- startTradeId

  return(request(action = "/api/v1/private/tradehistory", keys = keys, data = options, simplify = simplify))
}



