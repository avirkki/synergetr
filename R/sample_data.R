#
# Description of example data sets
#

# Author(s)  : Arho Virkki
# Copyright  : TUH Centre for Clinical Informatics
# Date       : 2017-08-04


#' Imaginary diagnosis results and medical treatments
#'
#' A synthetic dataset that introduces an imaginary population with different
#' medical diagnoses (given by ICD-10 coding system) and the corresponding
#' drug treatments. The data does not make sense in medical terms and is 
#' only useful for illustrating the usage of the \code{synergetr} R package.
#'
#' @format  A data ftame with 10000 rows and 21 variables:
#' \itemize{
#'   \item id: Primary key (surrogate key)
#'   \item business_code: Iten business key (natural key)
#'   \item person_id: Personal identity code
#'   \item death_date: Death date, if applicable or known
#'   \item home_town: Home town (some random Finnish town names)
#'   \item home_town_code: Home town code id
#'   \item diag_code: ICD-10 diagnosis code
#'   \item is_main_diag: Wheter this is main dianosis
#'   \item diag_group: ICD-10 group
#'   \item diag_hierarchy: ICD-10 hierachy
#'   \item desc_latin: Diagnosis description in Latin
#'   \item desc_english: Diagnosis description in English
#'   \item diag_date: Date of diagnosis
#'   \item drug_trade_name: Drug treatment
#'   \item drug_atc_code: Drug ATC code
#'   \item drug_used_at_home: Is the drug given in hospital or at home
#'   \item drug_is_antibiotics: Is the drug antibiotic
#'   \item visit_time_min: How long the doctor visit took
#'   \item valid_event: Event validity in the database
#'   \item doctor_code: Doctor abbreviation code
#'   \item doctor_name: Doctor name
#' }
#' @usage data(diags)
#' @source Synthetic random data generated with \code{synergetr} R
#' package at Turku University Hospital on 2017-08-04 for the 
#' 'Models4Health' research project (T152_2017) by Arho Virkki.
"diags"
