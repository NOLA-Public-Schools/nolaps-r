


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
getdata_application <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        OneApp_ID__c
      from Application__c
      where
        RecordTypeId in (
          '012d0000000tDWHAA2',
          '0120W000001tdvtQAA',
          '012d0000000tDWIAA2'
        ) and
        CreatedDate >= 2019-11-01T00:00:00Z
      "
    ),
    api_type = "Bulk 2.0"
  )

}



#' @export
getdata_applicationschool <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        School_Code__c,
        School__c
      from Application_School__c
      "
    )
  )

}



#' @export
getdata_applicationschoolranking <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        Application__c,
        Application_School__c
      from Application_School_Ranking__c
      where
        Application_School__c != null and
        CreatedDate >= 2019-11-01T00:00:00Z
      "
    ),
    api_type = "Bulk 2.0"
  )

}



#' @export
getdata_sibling <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        Student_OneApp_ID__c,
        PG_Sibling_OneApp_ID__c
      from Family_Relationship__c
      where Relationship_to_Student__c = 'Sibling'
      "
    ),
    api_type = "Bulk 2.0"
  )

}



#' @export
getdata_recordtype <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        Name
      from RecordType
      "
    )
  )

}



#' @export
getdata_student <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        OneApp_ID__c,
        SchoolForce__School__c,
        Future_School__c
      from Schoolforce__Student__c
      where School_Year__c = '2020-2021'
      "
    ),
    api_type = "Bulk 2.0"
  )

}


