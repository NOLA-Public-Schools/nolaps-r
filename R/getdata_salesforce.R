


#' @importFrom magrittr %>%



#' @export
getdata_coordinates_application <- function() {

  salesforcequery$salesforcequery$execute_query(
    glue::glue(
      "
      select
        OneApp_ID__c,
        Address_Longitude__c,
        Address_Latitude__c
      from Application__c
      where
        CreatedDate >= 2019-11-01T00:00:00Z and
        RecordTypeId in (
          select Id
          from RecordType
          where Name in ('Round 1', 'Round 2')
        )
      "
    )
  )

}



#' @export
getdata_recordtype <- function() {

  salesforcequery$salesforcequery$execute_query(
    glue::glue(
      "
      select Id, Name
      from RecordType
      "
    )
  )

}


