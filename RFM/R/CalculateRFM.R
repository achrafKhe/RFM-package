
#' Measuring the value of a customer within the RFM framework
#' @details
#' \code{data} - Measure the recency, frequency, and monetary value of customer purchases.
#'             -Set a score to rank customers according to their purchase recency, frequency, and monetary value.
#'             -Calculate the overall RFM score
#'
#' @param arg1 Customer data set.
#' @param arg2 A number.
#' @param arg3 A number.
#' @param arg4 A number.
#' @return A data set.
#' @examples
#' add(1, 1)
#' add(10, 1)

RFMfunction <- function(d, weight_recency=1, weight_frequency=1, weight_monetary=1){

  # adjusting values to ensure that the weights add up to one
  weight_recency2 <- weight_recency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_frequency2 <- weight_frequency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_monetary2 <- weight_monetary/sum(weight_recency, weight_frequency, weight_monetary)

  print("weights are calculated")

  # RFM measures
  max.Date <- max(d$TransDate)

  temp <- d[,list(recency = as.numeric(max.Date - max(TransDate)),
    frequency = .N,
    monetary = mean(PurchAmount)),
    by=Customer
  ]

  print("RFM Measure done")

  # RFM scores
  temp <- temp[,list(Customer,
                     recency = as.numeric(cut2(-recency, g=3)),
                     frequency = as.numeric(cut2(frequency, g=3)),
                     monetary = as.numeric(cut2(monetary, g=3)))]

  # Overall RFM score
  temp[,finalscore:=weight_recency2*recency+weight_frequency2*frequency+weight_monetary2*monetary]

  print("Overall RFM Measure done")

  # RFM group
  temp[,group:=round(finalscore)]

  # Return final table
  return(temp)
}



