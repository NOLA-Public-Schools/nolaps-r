


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
          select Id
          from RecordType
          where Name in ('Round 1', 'Round 2')
        ) and
        CreatedDate >= 2019-11-01T00:00:00Z
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
        Application__c in (
          select Id
          from Application__c
          where CreatedDate >= 2019-11-01T00:00:00Z
        ) and
        CreatedDate >= 2019-11-01T00:00:00Z and
        Application_School__c != null
      "
    )
  )

}



#' @export
getdata_family <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        Student_OneApp_ID__c,
        PG_Sibling_OneApp_ID__c
      from Family_Relationship__c
      where
        Student_Reference__c in (
          select Id
          from Schoolforce__Student__c
          where School_Year__c = '2020-2021'
        ) and
        Relationship_to_Student__c = 'Sibling'
      "
    )
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
    )
  )

}


