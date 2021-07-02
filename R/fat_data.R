#' Randomly generated data to simulate child fat percentage regressed on time-use compositional data
#'
#' A dataset containing fat percentage (outcome), time-use compositions (sl,sb,lpa,mvpa), and covariates
#'  (sibs,parents,ed). Note sl+sb+lpa+mvpa=1440 minutes for each subject. The variables are as follows:
#'
#' \itemize{
#'   \item fat. child fat percentage (11.29--29.99)
#'   \item sl. daily sleep in minutes (283--765)
#'   \item sb. sedentary behaviour in minutes (354--789)
#'   \item lpa. low-intensity physical activity in minutes (157--507)
#'   \item mvpa. moderate- to vigorous-intensity physical activity in minutes (35--155)
#'   \item sibs. number of siblings (0,1,2,3,4)
#'   \item parents. number of parents/caregivers at home (1,2)
#'   \item ed. education level of parent(s) (0=high school, 1=diploma, 2=degree)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name fat_data
#' @usage data(fat_data)
#' @format A data frame with 100 rows and 8 variables
NULL
