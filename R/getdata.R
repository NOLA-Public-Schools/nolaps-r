


#' @importFrom magrittr %>%



# Database ----------------------------------------------------------------

#' @export
getdata_governance <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema('db_owner', 'governance')
  )

}



#' @export
getdata_derivation_discipline <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema('db_owner', 'derivation_discipline')
  )

}



#' @export
getdata_derivation_enrollment <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema('db_owner', 'derivation_enrollment')
  )

}



#' @export
getdata_disadvantage <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema('db_owner', 'disadvantage')
  )

}



#' @export
getdata_roster_assessment <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema('db_owner', 'roster_assessment')
  )

}



#' @export
getdata_roster_graduation <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema('db_owner', 'roster_graduation')
  )

}

# Local -------------------------------------------------------------------

#' @export
getdata_performancescores_schools <- function() {

  readr::read_csv(
    file = '~/Projects/Performance Scores/Data/performancescores_schools_clean.csv',
    col_types = 'dclccccdcccccddddddddddd'
  )

}



#' @export
getdata_assessment_roster <- function() {

  readr::read_csv(
    file = '~/Projects/Assessment/Data/assessment_roster_postcert_clean.csv',
    col_types = 'cccccccccccccccccccccccddcccccccccc'
  )

}



#' @export
getdata_enrollmentderivation <- function() {

  readr::read_csv(
    file = '~/Data/Enrollment Derivation/data/clean/enrollmentderivation_allyears.csv',
    col_types = 'dccccTccccTccccccTccccccTcccccccccccddccTTcccccccccccccccccccTccTcccccccccccccccccc'
  )

}



#' @export
getdata_multistats_mfp_districts <- function() {

  readr::read_csv(
    file = '~/Data/Multi Stats/clean/multistats_mfp_allyears_10_districts.csv',
    col_types = 'dccdddddddddddddddddddddddddddddddd'
  )

}



#' @export
getdata_multistats_mfp_schools <- function() {

  readr::read_csv(
    file = '~/Data/Multi Stats/clean/multistats_mfp_allyears_10_schools.csv',
    col_types = 'dcccccdddddddddddddddddddddddddddddddcccc'
  )

}



#' @export
getdata_sped_districts <- function() {

  readr::read_csv(
    file = '~/Data/Special Education/clean/sped_allyears_02_districts.csv',
    col_types = 'dccdd'
  )

}



#' @export
getdata_sped_schools <- function() {

  readr::read_csv(
    file = '~/Data/Special Education/clean/sped_allyears_02_schools.csv',
    col_types = 'dccccdd'
  )

}


