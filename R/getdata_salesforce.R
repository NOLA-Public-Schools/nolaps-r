#' @importFrom magrittr %>%



#' @export
getdata_account <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        School_Code_String__c,
        Name,
        Grade_Span_Current_SY__c,
        Grade_Span__c,
        Terminal_Grade__c
      from Account
      "
    )
  )

}



#' @export
getdata_account_address <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        School_Code_String__c,
        Name,
        BillingStreet,
        BillingCity,
        BillingState,
        BillingPostalCode
      from Account
      "
    )
  ) %>%
    dplyr::select(
      id_account = Id,
      code_site = School_Code_String__c,
      name_account = Name,
      street = BillingStreet,
      city = BillingCity,
      state = BillingState,
      zip = BillingPostalCode
    )

}



#' @export
getdata_application <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        OneApp_ID__c,
        Grade_Applying_For__c,
        Student_First_Name__c,
        Student_Last_Name__c,
        Parent_Guardian_First_Name__c,
        Parent_Guardian_Last_Name__c,
        Parent_Guardian_Email_Address__c,
        Primary_Contact_Number__c,
        Secondary_Contact_Number__c
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
getdata_student_active <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        OneApp_ID__c,
        SchoolForce__Student_First_Name__c,
        SchoolForce__Student_Last_Name__c,
        Current_Grade__c,
        SchoolForce__School__c
      from Schoolforce__Student__c
      where
        SchoolForce__Active__c = TRUE
      "
    ),
    api_type = "Bulk 2.0"
  )

}



#' @export
getdata_student_futureschool <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        OneApp_ID__c,
        Future_School__c
      from Schoolforce__Student__c
      where
        Recent_Record__c = 'true' and
        Future_School__c != null and
        School_Year__c = '2020-2021'
      "
    ),
    api_type = "Bulk 2.0"
  )

}


