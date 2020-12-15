#' Data from the European Social Survey, Round 7 for Spain
#'
#' A dataset containing selected variables from the European Social Survey,
#' Round 7 for Spain.
#'
#' @format A data frame with 1925 rows and 19 variables:
#' \describe{
#' \item{idno}{The unique ID of each respondent}
#' \item{trstprl}{How much political trust do you have in institutions/parliament}
#' \item{trstplt}{How much political trust do you have in authorities/politicians}
#' \item{trstprt}{How much political trust do you have in institutions/political parties}
#' \item{stfedu}{How much political satisfaction do you have in education}
#' \item{stfhlth}{How much political satisfaction do you have in health services}
#' \item{psppsgv}{Political system allows people to have a say in what government does}
#' \item{psppipl}{Political system allows people to have influence on politics}
#' \item{ptcpplt}{Politicians care what people think}
#' \item{ppltrst}{Social trust}
#' \item{polintr}{Political interest}
#' \item{stflife}{Subjective well being , life satisfaction}
#' \item{stfeco}{Political satisfaction , country's economy}
#' \item{agea}{Age}
#' \item{edulvlb}{Education level}
#' \item{eisced}{ISCED education classification}
#' \item{psu}{Primary sampling unit. This is useful for doing complex weighted analysis.}
#' \item{stratum}{The stratum of each respondent. This is useful for doing complex weighted analysis}
#' \item{prob}{Probability weights. This is useful for doing complex weighted analysis}
#' }
#' @source Downloaded from the ESS website at https://www.europeansocialsurvey.org/download.html?file=ESS7ES&c=ES&y=2014 and merged with SDDF data for the weight variables.
"ess7es"
