#' @importFrom magrittr %>%



#' @export
einstein <- function() {

  tibble::tribble(
    ~code_site, ~code_site_group,
    "WBA001", "Einstein",
    "WBM001", "Einstein",
    "WBN001", "Einstein",
    "WBO001", "Einstein",
  )

}



#' @export
mutate_code_site_group <- function(x) {

  x %>%
    dplyr::mutate(code_site_group = dplyr::case_when(
      code_site %in% einstein()$code_site ~ "Einstein",
      stringr::str_detect(code_site, "^[:alnum:]{5,6}") ~ stringr::str_pad(
        stringr::str_extract(code_site, "^[:alnum:]{5,6}"),
        width = 6,
        side = "left",
        pad = "0"
        ),
      TRUE ~ code_site
      )
    )

}




#' @export
getdata_account <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        Name,
        School_Code_String__c,
        Grade_Span__c,
        School_Status__c
      from Account
      "
    )
  ) %>%
    dplyr::select(
      id_account = Id,
      name_account = Name,
      code_site = School_Code_String__c,
      gradespan_nextyear = Grade_Span__c,
      status = School_Status__c
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
      name_account = Name,
      code_site = School_Code_String__c,
      street = BillingStreet,
      city = BillingCity,
      state = BillingState,
      zip = BillingPostalCode
    )

}



#' @export
getdata_account_contacts <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        Name,
        School_Code_String__c,
        Enrollment_POC_Email__c
      from Account
      "
    )
  ) %>%
    dplyr::select(
      id_account = Id,
      name_account = Name,
      code_site = School_Code_String__c,
      email_enrollment = Enrollment_POC_Email__c
    )

}



#' @export
getdata_account_open <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        Name
      from Account
      where
        School_Status__c = 'Open'
      "
    )
  ) %>%
    dplyr::select(
      id_account = Id,
      name_account = Name
    )

}



#' @export
getdata_app <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        CreatedDate,
        OneApp_ID__c,
        Student__c,
        Id,
        Grade_Applying_For__c,
        Student_First_Name__c,
        Student_Last_Name__c,
        Parent_Guardian_First_Name__c,
        Parent_Guardian_Last_Name__c,
        Parent_Guardian_Email_Address__c,
        Primary_Contact_Number__c,
        Secondary_Contact_Number__c,
        RecordTypeId
      from Application__c
      where
        CreatedDate >= 2018-11-01T00:00:00Z
      "
    ),
    api_type = "Bulk 2.0"
  ) %>%
    dplyr::select(
      date_created = CreatedDate,
      oneappid = OneApp_ID__c,
      id_student = Student__c,
      id_app = Id,
      grade_applying = Grade_Applying_For__c,
      applicant_firstname = Student_First_Name__c,
      applicant_lastname = Student_Last_Name__c,
      pg_firstname = Parent_Guardian_First_Name__c,
      pg_lastname = Parent_Guardian_Last_Name__c,
      email = Parent_Guardian_Email_Address__c,
      phone_1 = Primary_Contact_Number__c,
      phone_2 = Secondary_Contact_Number__c,
      id_recordtype = RecordTypeId
    ) %>%
    dplyr::left_join(getdata_recordtype(), by = "id_recordtype") %>%
    dplyr::select(-id_recordtype) %>%
    dplyr::relocate(recordtype)

}


#' @export
getdata_app_1year <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        CreatedDate,
        OneApp_ID__c,
        Student__c,
        Id,
        Grade_Applying_For__c,
        Student_First_Name__c,
        Student_Last_Name__c,
        Parent_Guardian_First_Name__c,
        Parent_Guardian_Last_Name__c,
        Parent_Guardian_Email_Address__c,
        Primary_Contact_Number__c,
        Secondary_Contact_Number__c,
        RecordTypeId
      from Application__c
      where
        CreatedDate >= 2020-11-01T00:00:00Z
      "
    ),
    api_type = "Bulk 2.0"
  ) %>%
    dplyr::select(
      date_created = CreatedDate,
      oneappid = OneApp_ID__c,
      id_student = Student__c,
      id_app = Id,
      grade_applying = Grade_Applying_For__c,
      applicant_firstname = Student_First_Name__c,
      applicant_lastname = Student_Last_Name__c,
      pg_firstname = Parent_Guardian_First_Name__c,
      pg_lastname = Parent_Guardian_Last_Name__c,
      email = Parent_Guardian_Email_Address__c,
      phone_1 = Primary_Contact_Number__c,
      phone_2 = Secondary_Contact_Number__c,
      id_recordtype = RecordTypeId
    ) %>%
    dplyr::left_join(getdata_recordtype(), by = "id_recordtype") %>%
    dplyr::select(-id_recordtype) %>%
    dplyr::relocate(recordtype)

}



#' @export
getdata_appschool <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        School_Code__c,
        School__c,
        Is_Valid__c,
        Is_District_School__c
      from Application_School__c
      "
    )
  ) %>%
    dplyr::select(
      id_appschool = Id,
      code_appschool = School_Code__c,
      id_account = School__c,
      is_valid = Is_Valid__c,
      is_districtschool = Is_District_School__c
    )

}



#' @export
getdata_appschoolranking <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        CreatedDate,
        Application__c,
        Application_School__c,
        Rank__c
      from Application_School_Ranking__c
      where
        Application_School__c != null and
        CreatedDate >= 2018-11-01T00:00:00Z
      "
    ),
    api_type = "Bulk 2.0"
  ) %>%
    dplyr::select(
      id_appschoolranking = Id,
      date_created = CreatedDate,
      id_app = Application__c,
      id_appschool = Application_School__c,
      rank = Rank__c
    )

}



#' @export
getdata_appschoolranking_1year <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        CreatedDate,
        Application__c,
        Application_School__c,
        Rank__c
      from Application_School_Ranking__c
      where
        Application_School__c != null and
        CreatedDate >= 2020-11-01T00:00:00Z
      "
    ),
    api_type = "Bulk 2.0"
  ) %>%
    dplyr::select(
      id_appschoolranking = Id,
      date_created = CreatedDate,
      id_app = Application__c,
      id_appschool = Application_School__c,
      rank = Rank__c
    )

}



#' @export
getdata_sibling <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Student_OneApp_ID__c,
        PG_Sibling_OneApp_ID__c
      from Family_Relationship__c
      where Relationship_to_Student__c = 'Sibling'
      "
    ),
    api_type = "Bulk 2.0"
  ) %>%
    dplyr::select(
      student_oneappid = Student_OneApp_ID__c,
      sibling_oneappid = PG_Sibling_OneApp_ID__c
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
  ) %>%
    dplyr::select(
      id_recordtype = Id,
      recordtype = Name
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
  ) %>%
    dplyr::select(
      id_student = Id,
      oneappid = OneApp_ID__c,
      student_firstname = SchoolForce__Student_First_Name__c,
      student_lastname = SchoolForce__Student_Last_Name__c,
      grade_current = Current_Grade__c,
      id_account_current = SchoolForce__School__c
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



#' @export
getdata_student_3years <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        OneApp_ID__c,
        SchoolForce__School__c,
        School_Year__c
      from Schoolforce__Student__c
      where
        SchoolForce__School__c != null and
        School_Year__c in ('2018-2019', '2019-2020', '2020-2021')
      "
    ),
    api_type = "Bulk 2.0"
  ) %>%
    dplyr::select(
      id_student = Id,
      oneappid = OneApp_ID__c,
      id_account_current = SchoolForce__School__c,
      year_student = School_Year__c
    )

}


