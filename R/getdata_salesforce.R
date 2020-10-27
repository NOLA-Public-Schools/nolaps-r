


#' @export
getdata_account <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id
      from Account
      "
    )
  )

}



#' @export
getdata_applicationschool <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        School_Code__c
        School__c
      from Application_School__c
      "
    )
  )

}


