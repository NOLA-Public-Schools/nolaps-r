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
        School_Status__c,
        Governance__c
      from Account
      "
    )
  ) %>%
    dplyr::select(
      id_account = Id,
      name_account = Name,
      code_site = School_Code_String__c,
      gradespan_nextyear = Grade_Span__c,
      status = School_Status__c,
      governance = Governance__c
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
        Grade_Span__c,
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
      gradespan_next = Grade_Span__c,
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
        Enrollment_POC_Email__c,
        Analytics_Reports_Email_s__c
      from Account
      "
    )
  ) %>%
    dplyr::select(
      id_account = Id,
      name_account = Name,
      code_site = School_Code_String__c,
      email_enrollment = Enrollment_POC_Email__c,
      email_analytics = Analytics_Reports_Email_s__c
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
getdata_account_highdemand <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        Name,
        School_Code_String__c,
        High_Demand_School__c
      from Account
      where
        High_Demand_School__c != null
      "
    )
  ) %>%
    dplyr::select(
      id_account = Id,
      name_account = Name,
      code_site = School_Code_String__c,
      is_highdemand = High_Demand_School__c
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
        Address_Longitude__c,
        Address_Latitude__c,
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
      lon = Address_Longitude__c,
      lat = Address_Latitude__c,
      id_recordtype = RecordTypeId
    ) %>%
    dplyr::left_join(getdata_recordtype(), by = "id_recordtype") %>%
    dplyr::select(-id_recordtype) %>%
    dplyr::relocate(recordtype)

}



#' @export
getdata_app_claimedschool <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        Student__c,
        OneApp_ID__c,
        Student_First_Name__c,
        Student_Last_Name__c,
        Grade_Applying_For__c,
        Current_School__c,
        RecordTypeId
      from Application__c
      where
        CreatedDate >= 2020-11-01T00:00:00Z and
        Current_School__c != null
      "
    ),
    api_type = "Bulk 2.0"
  ) %>%
    dplyr::select(
      id_app = Id,
      id_student = Student__c,
      oneappid = OneApp_ID__c,
      applicant_firstname = Student_First_Name__c,
      applicant_lastname = Student_Last_Name__c,
      grade_applying = Grade_Applying_For__c,
      id_appschool_claimed = Current_School__c,
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
        Name,
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
      name_appschool = Name,
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
        Rank__c,
        EC_Ranking__c,
        EC_Program_Type__c,
        EC_Eligibility__c
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
      rank = Rank__c,
      is_ec = EC_Ranking__c,
      programtype = EC_Program_Type__c,
      eligibility = EC_Eligibility__c
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
getdata_appschoolranking_priorities <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        CreatedDate,
        Application__c,
        Application_School__c,
        Rank__c,
        Verified_Sibling__c
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
      rank = Rank__c,
      is_verifiedsibling = Verified_Sibling__c
    )

}



#' @export
getdata_contact <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        Email,
        LastModifiedDate
      from Contact
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    dplyr::select(
      id_contact = Id,
      email = Email,
      date_modified = LastModifiedDate
    )

}



#' @export
getdata_ec_responses <- function(date_start = "2020-11-01", date_end = lubridate::today()) {

  salesforcer::sf_query(
    glue::glue_safe(
      "
      select
        Student__c,
        Application__c,
        Id,
        Student_Has_IEP__c,
        Student_Has_IFSP__c,
        Foster_Care_Kinship_Subsidy__c,
        Residency_Status__c,
        Student_Parent_Current_Location_Living_A__c
      from EC_Question_Responses__c
      where
        CreatedDate >= {date_start}T00:00:00Z
        and CreatedDate < {date_end}T00:00:00Z
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    dplyr::select(
      id_student = Student__c,
      id_application = Application__c,
      id_ec_response = Id,
      has_iep = Student_Has_IEP__c,
      has_ifsp = Student_Has_IFSP__c,
      is_foster = Foster_Care_Kinship_Subsidy__c,
      residency_status = Residency_Status__c,
      living_arrangement = Student_Parent_Current_Location_Living_A__c
    )

}



#' @export
getdata_gradecapacity <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        School_Name__c,
        Grade__c,
        Available_Seats__c,
        Current_Active_Register__c,
        Current_Live_Register__c
      from Grade_Capacity__c
      "
    )
  ) %>%
    dplyr::select(
      id_gradecapacity = Id,
      id_account = School_Name__c,
      grade = Grade__c,
      seats_available = Available_Seats__c,
      currentregister_active = Current_Active_Register__c,
      currentregister_live = Current_Live_Register__c
    )

}



#' @export
getdata_guardian <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Student_OneApp_ID__c,
        Reference_Id__c
      from Family_Relationship__c
      where Relationship_to_Student__c in ('Guardian', 'Parent')
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    dplyr::select(
      student_oneappid = Student_OneApp_ID__c,
      id_relationship = Reference_Id__c
    ) %>%
    tidyr::separate(
      col = id_relationship,
      into = c("id_contact_student", "id_contact_guardian"),
      sep = "_"
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
        SchoolForce__Contact_Id__c,
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
      oneappid = OneApp_ID__c,
      id_contact = SchoolForce__Contact_Id__c,
      id_student = Id,
      student_firstname = SchoolForce__Student_First_Name__c,
      student_lastname = SchoolForce__Student_Last_Name__c,
      grade_current = Current_Grade__c,
      id_account_current = SchoolForce__School__c
    )

}



#' @export
getdata_student_active_address <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        OneApp_ID__c,
        SchoolForce__Student_First_Name__c,
        SchoolForce__Student_Last_Name__c,
        Current_Grade__c,
        SchoolForce__School__c,
        SchoolForce__Address__c,
        SchoolForce__City__c,
        SchoolForce__State__c,
        SchoolForce__Zip_Code__c
      from Schoolforce__Student__c
      where
        SchoolForce__Active__c = TRUE and
        SchoolForce__School__c != null
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    dplyr::select(
      id_student = Id,
      oneappid = OneApp_ID__c,
      student_firstname = SchoolForce__Student_First_Name__c,
      student_lastname = SchoolForce__Student_Last_Name__c,
      grade_current = Current_Grade__c,
      id_account_current = SchoolForce__School__c,
      student_street = SchoolForce__Address__c,
      student_city = SchoolForce__City__c,
      student_state = SchoolForce__State__c,
      student_zip = SchoolForce__Zip_Code__c
    )

}



#' @export
getdata_student_contact <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        SchoolForce__Contact_Id__c,
        OneApp_ID__c,
        SchoolForce__Address__c,
        SchoolForce__City__c,
        SchoolForce__State__c,
        SchoolForce__Zip_Code__c,
        SchoolForce__Email__c,
        SchoolForce__Active__c
      from Schoolforce__Student__c
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    dplyr::select(
      id_student = Id,
      id_contact = SchoolForce__Contact_Id__c,
      oneappid = OneApp_ID__c,
      student_street = SchoolForce__Address__c,
      student_city = SchoolForce__City__c,
      student_state = SchoolForce__State__c,
      student_zip = SchoolForce__Zip_Code__c,
      student_email = SchoolForce__Email__c,
      is_active = SchoolForce__Active__c
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



#' @export
getdata_student_year <- function(years = c("2020-2021")) {

  years <-
    stringr::str_flatten(years, "', '") %>%
    stringr::str_c("('", ., "')")

  salesforcer::sf_query(
    glue::glue_safe(
      "
      select
        OneApp_ID__c,
        SchoolForce__Contact_Id__c,
        Id
      from Schoolforce__Student__c
      where
        School_Year__c in {years}
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    dplyr::select(
      oneappid = OneApp_ID__c,
      id_contact = SchoolForce__Contact_Id__c,
      id_student = Id
    )

}






