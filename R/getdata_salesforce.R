#' @import dplyr
#' @import glue
#' @import lubridate
#' @import salesforcer
#' @import stringr
#' @import tidyr

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
getdata_soql <- function(args = commandArgs(trailingOnly = TRUE)) {

  soql <- args[1]
  file_out <- args[2]

  salesforcer::sf_query(soql, guess_types = FALSE) %>%
    readr::write_excel_csv(file_out, na = "") %>%
    print()

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
        Application_Code__c,
        Governance__c,
        EC_Funding_Source__c,
        School_Status__c,
        High_Demand_School__c,
        Grade_Span_Current_SY__c,
        Grade_Span__c,
        Terminal_Grade__c,
        Guarantee_School__c,
        CMO_Group__c,
        Enrollment_POC_Email__c,
        Designated_Content_Approver__c,
        Analytics_Reports_Email_s__c,
        BillingStreet,
        BillingCity,
        BillingState,
        BillingPostalCode,
        BillingLongitude,
        BillingLatitude,
        Current_Year_Address__c,
        Neighborhood__c,
        Phone,
        Public_Email__c,
        Website,
        Registration_Details__c,
        School_Welcome_Message__c,
        Uniforms_Required__c,
        Letter_Grade__c
      from Account
      "
    ),
    guess_types = FALSE
  ) %>%
    dplyr::select(
      id_account = Id,
      name_account = Name,
      code_site = School_Code_String__c,
      code_appschool_account = Application_Code__c,
      governance = Governance__c,
      fundingsource = EC_Funding_Source__c,
      status = School_Status__c,
      is_highdemand = High_Demand_School__c,
      gradespan_currentyear = Grade_Span_Current_SY__c,
      gradespan_nextyear = Grade_Span__c,
      grade_terminal = Terminal_Grade__c,
      id_account_guarantee = Guarantee_School__c,
      cmo = CMO_Group__c,
      email_enrollment = Enrollment_POC_Email__c,
      email_approver = Designated_Content_Approver__c,
      email_analytics = Analytics_Reports_Email_s__c,
      street = BillingStreet,
      city = BillingCity,
      state = BillingState,
      zip = BillingPostalCode,
      lon = BillingLongitude,
      lat = BillingLatitude,
      address_currentyear = Current_Year_Address__c,
      neighborhood = Neighborhood__c,
      phone = Phone,
      email_public = Public_Email__c,
      website = Website,
      registration = Registration_Details__c,
      welcome = School_Welcome_Message__c,
      uniforms = Uniforms_Required__c,
      lettergrade = Letter_Grade__c
    ) %>%
    dplyr::mutate(dplyr::across(c(
      is_highdemand
      ),
      as.logical
      )
    ) %>%
    dplyr::mutate(dplyr::across(c(
      lon, lat
      ),
      as.numeric
      )
    )

}



#' @export
getdata_account_gradespan <- function() {

  getdata_account() %>%
    dplyr::select(id_account, gradespan_nextyear) %>%
    tidyr::separate_rows(gradespan_nextyear, sep = ";") %>%
    fix_grades(gradespan_nextyear) %>%
    dplyr::arrange(gradespan_nextyear) %>%
    dplyr::group_by(id_account) %>%
    dplyr::summarize(
      grade_max = max(gradespan_nextyear),
      gradespan_nextyear_vector = list(gradespan_nextyear)
    ) %>%
    dplyr::ungroup()

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
getdata_accountability <- function() {

  sf_query(
    glue(
      "
      select
        Id,
        Name,
        SIS_Site_Code__c,
        Federal_Site_Code__c,
        Closed__c,
        Single_Site__c,
        Contract_End_Date__c,
        Total_Enrollment__c,
        Letter_Grade__c,
        School_Performance_Score__c,
        Rate_Economically_Disadvantaged__c,
        Rate_Minority__c,
        Rate_Limited_English_Proficiency__c
      from Accountability_Site__c
      "
    ),
    guess_types = FALSE
  ) %>%
    select(
      id_accountability = Id,
      name_accountability = Name,
      code_site = SIS_Site_Code__c,
      code_site_federal = Federal_Site_Code__c,
      is_closed = Closed__c,
      is_singlesite = Single_Site__c,
      date_contract_end = Contract_End_Date__c,
      n_total = Total_Enrollment__c,
      grade_sps_cb = Letter_Grade__c,
      index_sps_cb = School_Performance_Score__c,
      rate_disadvantage = Rate_Economically_Disadvantaged__c,
      rate_minority = Rate_Minority__c,
      rate_limeng = Rate_Limited_English_Proficiency__c
    ) %>%
    mutate(across(
      c(
        n_total,
        starts_with("rate_")
      ),
      as.numeric
      )
    ) %>%
    mutate(across(
      c(
        starts_with("date_")
      ),
      as_date
      )
    ) %>%
    mutate(across(
      c(
        starts_with("is_")
      ),
      as.logical
      )
    )

}



#' @export
getdata_app <- function(round = "Round 1", start = date_appstart()) {

  salesforcer::sf_query(
    glue::glue_safe(
      "
      select
        CreatedDate,
        LastModifiedDate,
        Archived__c,
        OneApp_ID__c,
        Student__r.SchoolForce__Contact_Id__c,
        Student__c,
        Id,
        Student__r.Current_Grade__c,
        Grade_Applying_For__c,
        Student_First_Name__c,
        Student_Last_Name__c,
        Date_of_Birth__c,
        Parent_Guardian_First_Name__c,
        Parent_Guardian_Last_Name__c,
        Parent_Guardian_Email_Address__c,
        Creator_Email__c,
        Application_Email_Update__c,
        Primary_Contact_Number__c,
        Secondary_Contact_Number__c,
        Application_Language__c,
        Preferred_Language__c,
        Address_Longitude__c,
        Address_Latitude__c,
        Street_Street_Name__c,
        City__c,
        State__c,
        Zip_Code__c,
        Address_Validated__c,
        Student__r.SchoolForce__School__c,
        Student__r.Future_School__c,
        Current_School__c,
        Current_School__r.School__c,
        Has_Completed_Verification__c,
        EC_Program_Eligibility_Verified__c,
        Student__r.School_Year__c,
        Student__r.Recent_Record__c,
        Student__r.SchoolForce__Active__c,
        Eligibility_B3__c,
        Eligibility_EC_Special_Needs__c,
        Eligibility_EHS__c,
        Eligibility_Head_Start__c,
        Eligibility_LA4__c,
        Eligibility_NOEEN__c,
        Eligibility_NSECD__c,
        Eligibility_PK4_Type_II__c,
        Eligibility_PK_GT__c,
        Eligibility_Tuition_LA__c,
        Eligibility_Tuition_OR__c
      from Application__c
      where
        RecordType.Name = '{round}' and
        CreatedDate >= {start}
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    dplyr::select(
      date_created = CreatedDate,
      date_modified = LastModifiedDate,
      is_archived = Archived__c,
      oneappid = OneApp_ID__c,
      id_contact = Student__r.SchoolForce__Contact_Id__c,
      id_student = Student__c,
      id_app = Id,
      grade_current = Student__r.Current_Grade__c,
      grade_applying = Grade_Applying_For__c,
      applicant_firstname = Student_First_Name__c,
      applicant_lastname = Student_Last_Name__c,
      dob_app = Date_of_Birth__c,
      pg_firstname = Parent_Guardian_First_Name__c,
      pg_lastname = Parent_Guardian_Last_Name__c,
      pg_email = Parent_Guardian_Email_Address__c,
      email_update = Application_Email_Update__c,
      email = Creator_Email__c,
      phone_1 = Primary_Contact_Number__c,
      phone_2 = Secondary_Contact_Number__c,
      language_app = Application_Language__c,
      language_pref = Preferred_Language__c,
      lon = Address_Longitude__c,
      lat = Address_Latitude__c,
      street = Street_Street_Name__c,
      city = City__c,
      state = State__c,
      zip = Zip_Code__c,
      is_addressvalidated = Address_Validated__c,
      id_account_current = Student__r.SchoolForce__School__c,
      id_account_future = Student__r.Future_School__c,
      id_appschool_claimed = Current_School__c,
      id_account_claimed = Current_School__r.School__c,
      has_verified = Has_Completed_Verification__c,
      strings_verified = EC_Program_Eligibility_Verified__c,
      year_student = Student__r.School_Year__c,
      is_recent = Student__r.Recent_Record__c,
      is_active = Student__r.SchoolForce__Active__c,
      is_eligible_b3 = Eligibility_B3__c,
      is_eligible_ecsped = Eligibility_EC_Special_Needs__c,
      is_eligible_ehs = Eligibility_EHS__c,
      is_eligible_hs = Eligibility_Head_Start__c,
      is_eligible_la4 = Eligibility_LA4__c,
      is_eligible_noeen = Eligibility_NOEEN__c,
      is_eligible_nsecd = Eligibility_NSECD__c,
      is_eligible_pk4type2 = Eligibility_PK4_Type_II__c,
      is_eligible_ecgt = Eligibility_PK_GT__c,
      is_eligible_tuitionla = Eligibility_Tuition_LA__c,
      is_eligible_tuitionor = Eligibility_Tuition_OR__c
    ) %>%
    fix_grades(var = grade_current) %>%
    fix_grades(var = grade_applying) %>%
    dplyr::mutate(dplyr::across(c(
      is_archived,
      is_addressvalidated,
      has_verified,
      is_recent,
      is_active,
      starts_with("is_eligible_")
      ),
      as.logical
      )
    ) %>%
    dplyr::mutate(across(c(
      date_created,
      date_modified
      ),
      lubridate::ymd_hms
      )
    ) %>%
    dplyr::mutate(across(c(
      dob_app
      ),
      lubridate::as_date
      )
    )

}



#' @export
getdata_app_3year <- function(round = "Round 1", start = date_appstart_3year()) {

  getdata_app(round = round, start = start)

}



#' @export
getdata_appinput <- function(round = "Round 1", start = date_appstart()) {

  sf_query(
    glue_safe(
      "
      select
        OneApp_ID__c,
        Input_Table_Family_Link_Opt_Out__c,
        Input_Table_100_FPL__c,
        Input_Table_At_Risk__c,
        Input_Table_French__c,
        Input_Table_Gifted_and_Talented__c,
        Input_Table_IEP__c,
        Input_Table_Military_Child__c,
        Input_Table_Montessori__c,
        Input_Table_UNO__c,
        Input_Table_Address_Verified__c
      from Match_Input_Tables__c
      where
        Application__r.RecordType.Name = '{round}' and
        Application__r.CreatedDate >= {start}
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    select(
      oneappid = OneApp_ID__c,
      optout_family = Input_Table_Family_Link_Opt_Out__c,
      has_100fpl = Input_Table_100_FPL__c,
      has_disadvantage = Input_Table_At_Risk__c,
      has_french = Input_Table_French__c,
      has_gt = Input_Table_Gifted_and_Talented__c,
      has_iep = Input_Table_IEP__c,
      has_military = Input_Table_Military_Child__c,
      has_montessori = Input_Table_Montessori__c,
      has_uno = Input_Table_UNO__c,
      has_verifiedaddress = Input_Table_Address_Verified__c
    ) %>%
    mutate(across(c(
      optout_family,
      has_100fpl,
      has_disadvantage,
      has_french,
      has_gt,
      has_iep,
      has_military,
      has_montessori,
      has_uno,
      has_verifiedaddress
      ),
      ~ as.logical(as.numeric(.))
      )
    )

}



#' @export
getdata_appschool <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        Name,
        Site_Code__c,
        School_Code__c,
        School__c,
        School__r.Name,
        School__r.School_Code_String__c,
        School__r.Governance__c,
        School__r.School_Status__c,
        School__r.Grade_Span__c,
        Grade_Span__c,
        Early_Childhood_School__c,
        EC_Program_Type__r.Name,
        Is_Valid__c,
        Is_District_School__c,
        Current_School_Option__c,
        Selective_School__c,
        Selective_School_Questions__c,
        Scholarship_School__c,
        Residents_Served__c,
        School__r.Neighborhood__c,
        Street__c,
        City__c,
        State__c,
        Zip__c,
        AddressLatitudeandLongitude__c,
        Proximity_Preference_Grades_Offered__c,
        Grade_INF_Zip_Preference__c,
        Grade_1YR_Zip_Preference__c,
        Grade_2YR_Zip_Preference__c,
        Grade_PK3_Zip_Preference__c,
        Grade_PK4_Zip_Preference__c,
        Grade_K_Zip_Preference__c,
        Grade_1_Zip_Preference__c,
        Grade_2_Zip_Preference__c,
        Grade_3_Zip_Preference__c,
        Grade_4_Zip_Preference__c,
        Grade_5_Zip_Preference__c,
        Grade_6_Zip_Preference__c,
        Grade_7_Zip_Preference__c,
        Grade_8_Zip_Preference__c,
        Grade_9_Zip_Preference__c,
        Grade_10_Zip_Preference__c,
        Grade_11_Zip_Preference__c,
        Grade_12_Zip_Preference__c,
        School_Type__c,
        Facility__c
      from Application_School__c
      "
    ),
    guess_types = FALSE
  ) %>%
    dplyr::select(
      id_appschool = Id,
      name_appschool = Name,
      code_site_appschool = Site_Code__c,
      code_appschool = School_Code__c,
      id_account = School__c,
      name_account = School__r.Name,
      code_site = School__r.School_Code_String__c,
      governance = School__r.Governance__c,
      status = School__r.School_Status__c,
      gradespan_account_nextyear = School__r.Grade_Span__c,
      gradespan_appschool = Grade_Span__c,
      is_ec = Early_Childhood_School__c,
      ec_type = EC_Program_Type__r.Name,
      is_valid = Is_Valid__c,
      is_districtschool = Is_District_School__c,
      is_currentschool = Current_School_Option__c,
      is_selective = Selective_School__c,
      questions_selective = Selective_School_Questions__c,
      is_scholarship = Scholarship_School__c,
      served = Residents_Served__c,
      neighborhood = School__r.Neighborhood__c,
      street = Street__c,
      city = City__c,
      state = State__c,
      zip = Zip__c,
      latlon = AddressLatitudeandLongitude__c,
      grades_distance = Proximity_Preference_Grades_Offered__c,
      zonepref_INF = Grade_INF_Zip_Preference__c,
      zonepref_1YR = Grade_1YR_Zip_Preference__c,
      zonepref_2YR = Grade_2YR_Zip_Preference__c,
      zonepref_PK3 = Grade_PK3_Zip_Preference__c,
      zonepref_PK4 = Grade_PK4_Zip_Preference__c,
      zonepref_K = Grade_K_Zip_Preference__c,
      zonepref_1 = Grade_1_Zip_Preference__c,
      zonepref_2 = Grade_2_Zip_Preference__c,
      zonepref_3 = Grade_3_Zip_Preference__c,
      zonepref_4 = Grade_4_Zip_Preference__c,
      zonepref_5 = Grade_5_Zip_Preference__c,
      zonepref_6 = Grade_6_Zip_Preference__c,
      zonepref_7 = Grade_7_Zip_Preference__c,
      zonepref_8 = Grade_8_Zip_Preference__c,
      zonepref_9 = Grade_9_Zip_Preference__c,
      zonepref_10 = Grade_10_Zip_Preference__c,
      zonepref_11 = Grade_11_Zip_Preference__c,
      zonepref_12 = Grade_12_Zip_Preference__c,
      schooltype = School_Type__c,
      id_facility = Facility__c
    ) %>%
    dplyr::mutate(dplyr::across(c(
      is_ec,
      is_valid,
      is_districtschool,
      is_currentschool,
      is_selective,
      is_scholarship
      ),
      as.logical
      )
    )

}



#' @export
getdata_appschoolranking <- function(round = "Round 1", start = date_appstart()) {

  salesforcer::sf_query(
    glue::glue_safe(
      "
      select
        CreatedDate,
        Application__r.OneApp_ID__c,
        Application__r.Student__r.SchoolForce__Contact_Id__c,
        Application__r.Student__c,
        Application__r.Student__r.SchoolForce__School__c,
        Application__c,
        Application__r.Grade_Applying_For__c,
        Id,
        Rank__c,
        Application_School__r.School__c,
        Application_School__r.School__r.Name,
        Application_School__c,
        Application_School__r.School_Code__c,
        Application_School__r.Selective_School__c,
        Application_School__r.Selective_School_Questions__c,
        Eligibility_Decision__c,
        EC_Ranking__c,
        EC_Program_Type__c,
        EC_Eligibility__c,
        Distance_From_Home__c,
        In_Proximity_Preference__c,
        In_School_Zip_Preference__c,
        Verified_Sibling__c,
        Child_Staff__c,
        Application__r.Address_Longitude__c,
        Application__r.Address_Latitude__c,
        Application_School__r.AddressLatitudeandLongitude__c
      from Application_School_Ranking__c
      where
        Application_School__c != null and
        Application__r.RecordType.Name = '{round}' and
        Application__r.CreatedDate >= {start} and
        CreatedDate >= {start}
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    dplyr::select(
      date_created = CreatedDate,
      oneappid = Application__r.OneApp_ID__c,
      id_contact = Application__r.Student__r.SchoolForce__Contact_Id__c,
      id_student = Application__r.Student__c,
      id_account_current = Application__r.Student__r.SchoolForce__School__c,
      id_app = Application__c,
      grade_applying = Application__r.Grade_Applying_For__c,
      id_appschoolranking = Id,
      rank = Rank__c,
      id_account = Application_School__r.School__c,
      name_account = Application_School__r.School__r.Name,
      id_appschool = Application_School__c,
      code_appschool = Application_School__r.School_Code__c,
      is_selective = Application_School__r.Selective_School__c,
      questions_selective = Application_School__r.Selective_School_Questions__c,
      eligibility_decision = Eligibility_Decision__c,
      is_ec = EC_Ranking__c,
      programtype = EC_Program_Type__c,
      eligibility = EC_Eligibility__c,
      distance = Distance_From_Home__c,
      is_priority_distance = In_Proximity_Preference__c,
      is_priority_zone = In_School_Zip_Preference__c,
      is_verifiedsibling = Verified_Sibling__c,
      is_staffchild = Child_Staff__c,
      lon_app = Application__r.Address_Longitude__c,
      lat_app = Application__r.Address_Latitude__c,
      latlon_appschool = Application_School__r.AddressLatitudeandLongitude__c
    ) %>%
    dplyr::mutate(across(c(
      is_selective,
      is_ec,
      is_priority_distance,
      is_priority_zone,
      is_verifiedsibling,
      is_staffchild
      ),
      as.logical
      )
    ) %>%
    dplyr::mutate(across(c(
      distance,
      lon_app,
      lat_app
      ),
      as.numeric
      )
    ) %>%
    fix_grades(var = grade_applying)

}



#' @export
getdata_appschoolranking_3year <- function(round = "Round 1", start = date_appstart_3year()) {

  getdata_appschoolranking(round = round, start = start)

}



#' @export
getdata_contact <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        FirstName,
        LastName,
        SchoolForce__Date_of_Birth__c,
        Email,
        Primary_Contact_Number__c,
        LastModifiedDate,
        RecordType.Name
      from Contact
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    dplyr::select(
      id_contact = Id,
      firstname = FirstName,
      lastname = LastName,
      dob = SchoolForce__Date_of_Birth__c,
      email = Email,
      phone = Primary_Contact_Number__c,
      date_modified = LastModifiedDate,
      contact_type = RecordType.Name
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
getdata_facility <- function() {
  sf_query(
    glue(
      "
      select
        Id,
        Name,
        Address__c,
        Ownership__c,
        Status__c,
        Parcel_Has_Structure__c,
        Longitude__c,
        Latitude__c,
        Program_Capacity__c,
        Census_Tract__c
      from Facility__c
      "
    ),
    api_type = "REST",
    guess_types = FALSE
  ) %>%
    select(
      id_facility = Id,
      name_facility = Name,
      address = Address__c,
      ownership = Ownership__c,
      status = Status__c,
      has_structure = Parcel_Has_Structure__c,
      lon = Longitude__c,
      lat = Latitude__c,
      program_capacity = Program_Capacity__c,
      tract = Census_Tract__c
    ) %>%
    mutate(across(c(
      lon,
      lat,
      program_capacity
      ),
      as.numeric
      )
    ) %>%
    mutate(across(c(
      has_structure
      ),
      as.logical
      )
    )

}



#' @export
getdata_feeder <- function() {

  sf_query(
    glue(
      "
      select
        Sending_School__c,
        Priority__r.Application_School__r.School__c,
        Grade__c
      from Feeder__c
      "
    ),
    api_type = "REST",
    guess_types = FALSE
  ) %>%
    select(
      id_account_current = Sending_School__c,
      id_account_applying = Priority__r.Application_School__r.School__c,
      grade_applying = Grade__c
    ) %>%
    fix_grades(grade_applying)

}



#' @export
getdata_gradecapacity <- function() {
  sf_query(
    glue(
      "
      select
        Id,
        School_Program__r.School__r.Name,
        School_Program__r.Name,
        Grade__c,
        School_Program__r.School__r.Governance__c,
        School_Program__r.School__r.School_Status__c,
        Available_Seats__c,
        Current_Active_Register__c,
        Current_Live_Register__c,
        Sections__c,
        Student_Sections__c,
        Projection_Seat_Target__c,
        Match_Target__c,
        Future_Match_Target__c,
        Future_10_1_Target__c,
        Requested_10_1_Target__c,
        Requested_Round_2_Target__c,
        Facility__c,
        Facility__r.Name,
        Facility__r.Address__c,
        Facility__r.Catchment_Zone__c,
        Facility__r.School_Board_District__c,
        Sibling_Unification__c,
        Reactivations__c
      from Grade_Level__c
      "
    ),
    api_type = "REST",
    guess_types = FALSE
  ) %>%
    select(
      id_gradecapacity = Id,
      name_accountability = School_Program__r.School__r.Name,
      name_account = School_Program__r.Name,
      grade = Grade__c,
      governance = School_Program__r.School__r.Governance__c,
      status = School_Program__r.School__r.School_Status__c,
      seats_available = Available_Seats__c,
      currentregister_active = Current_Active_Register__c,
      currentregister_live = Current_Live_Register__c,
      n_sections = Sections__c,
      students_per_section = Student_Sections__c,
      target_101 = Projection_Seat_Target__c,
      target_match = Match_Target__c,
      target_match_future = Future_Match_Target__c,
      target_101_future = Future_10_1_Target__c,
      target_101_requested = Requested_10_1_Target__c,
      target_requested_round_2 = Requested_Round_2_Target__c,
      id_facility = Facility__c,
      name_facility = Facility__r.Name,
      address = Facility__r.Address__c,
      catchment = Facility__r.Catchment_Zone__c,
      board_district = Facility__r.School_Board_District__c,
      is_siblingunification = Sibling_Unification__c,
      is_reactivation = Reactivations__c
    ) %>%
    fix_grades(var = grade) %>%
    mutate(across(c(
      seats_available,
      currentregister_active,
      target_101,
      target_101_future,
      target_101_requested,
      n_sections,
      students_per_section
      ),
      as.numeric
      )
    ) %>%
    mutate(across(c(
      is_siblingunification,
      is_reactivation
      ),
      as.logical
      )
    )

}



#' @export
getdata_guardian <- function() {

  sf_query(
    glue::glue(
      "
      select
        Student_OneApp_ID__c,
        Reference_Id__c
      from Family_Relationship__c
      where
        Relationship_to_Student__c in ('Guardian', 'Parent', 'Relative')
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    select(
      student_oneappid = Student_OneApp_ID__c,
      id_relationship = Reference_Id__c
    ) %>%
    separate(
      col = id_relationship,
      into = c("id_contact_student", "id_contact_guardian"),
      sep = "_"
    )

}



#' @export
getdata_placement <- function(years) {

  years <-
    stringr::str_flatten(years, "', '") %>%
    stringr::str_c("('", ., "')")

  salesforcer::sf_query(
    glue::glue_safe(
      "
      select
        CreatedDate,
        Id,
        Placement_Active__c,
        School_Year_Name__c,
        Placement_Student__r.SchoolForce__Contact_Id__c,
        Placement_Student__c,
        Future_School_Grade__c,
        Future_School_Name__c,
        Future_School_Name__r.Name,
        RecordType.Name
      from Placement__c
      where
        School_Year_Name__c in {years}
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    dplyr::select(
      date_created = CreatedDate,
      id_placement = Id,
      is_active = Placement_Active__c,
      year_placement = School_Year_Name__c,
      id_contact = Placement_Student__r.SchoolForce__Contact_Id__c,
      id_student = Placement_Student__c,
      grade_future = Future_School_Grade__c,
      id_account_future = Future_School_Name__c,
      name_account_future = Future_School_Name__r.Name,
      recordtype = RecordType.Name
    ) %>%
    dplyr::mutate(across(c(
      is_active
      ),
      as.logical
      )
    ) %>%
    fix_grades(grade_future)

}



#' @export
getdata_priority <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        Application_School__c,
        Application_School__r.School__c,
        Application_School__r.School__r.Name,
        Application_School__r.School__r.School_Code_String__c,
        Application_School__r.School_Code__c,
        Grade__c,
        Application_School__r.School__r.Governance__c,
        Application_School__r.EC_Program_Type__r.Name,

        Order_100_Federal_Poverty__c,
        Order_Attending_C__c,
        Order_Attending_DF__c,
        Order_Closing_Non_Public__c,
        Order_Closing_Public__c,
        Order_Disadvantage__c,
        Order_Distance__c,
        Order_Feeder__c,
        Order_French__c,
        Order_IEP__c,
        Order_Military__c,
        Order_Montessori__c,
        Order_Non_Transition__c,
        Order_NSECD__c,
        Order_Sibling__c,
        Order_Sibling_OR_Staff_Child__c,
        Order_Staff_Child__c,
        Order_Transition__c,
        Order_UNO_Staff__c,
        Order_Zone__c,

        Percentage_100_Federal_Poverty__c,
        Percentage_Attending_C__c,
        Percentage_Attending_DF__c,
        Percentage_Closing_Non_Public__c,
        Percentage_Closing_Public__c,
        Percentage_Disadvantage__c,
        Percentage_Distance__c,
        Percentage_Feeder__c,
        Percentage_French__c,
        Percentage_IEP__c,
        Percentage_Military__c,
        Percentage_Montessori__c,
        Percentage_Non_Transition__c,
        Percentage_NSECD__c,
        Percentage_Sibling__c,
        Percentage_Sibling_OR_Staff_Child__c,
        Percentage_Staff_Child__c,
        Percentage_Transition__c,
        Percentage_UNO_Staff__c,
        Percentage_Zone__c

      from Priority__c
      where
        Application_School__r.School__r.School_Status__c = 'Open' or
        Application_School__r.School__r.School_Status__c = 'Opening Next Year' or
        Application_School__r.School__r.School_Status__c = 'Closing at End of School Year'
      "
    )
  ) %>%
    dplyr::select(
      id_priority = Id,
      id_appschool = Application_School__c,
      id_account = Application_School__r.School__c,
      name_account = Application_School__r.School__r.Name,
      code_site = Application_School__r.School__r.School_Code_String__c,
      code_appschool = Application_School__r.School_Code__c,
      grade = Grade__c,
      governance = Application_School__r.School__r.Governance__c,
      ec_type = Application_School__r.EC_Program_Type__r.Name,

      Order_100_Federal_Poverty__c,
      Order_Attending_C__c,
      Order_Attending_DF__c,
      Order_Closing_Non_Public__c,
      Order_Closing_Public__c,
      Order_Disadvantage__c,
      Order_Distance__c,
      Order_Feeder__c,
      Order_French__c,
      Order_IEP__c,
      Order_Military__c,
      Order_Montessori__c,
      Order_Non_Transition__c,
      Order_NSECD__c,
      Order_Sibling__c,
      Order_Sibling_OR_Staff_Child__c,
      Order_Staff_Child__c,
      Order_Transition__c,
      Order_UNO_Staff__c,
      Order_Zone__c,

      Percentage_100_Federal_Poverty__c,
      Percentage_Attending_C__c,
      Percentage_Attending_DF__c,
      Percentage_Closing_Non_Public__c,
      Percentage_Closing_Public__c,
      Percentage_Disadvantage__c,
      Percentage_Distance__c,
      Percentage_Feeder__c,
      Percentage_French__c,
      Percentage_IEP__c,
      Percentage_Military__c,
      Percentage_Montessori__c,
      Percentage_Non_Transition__c,
      Percentage_NSECD__c,
      Percentage_Sibling__c,
      Percentage_Sibling_OR_Staff_Child__c,
      Percentage_Staff_Child__c,
      Percentage_Transition__c,
      Percentage_UNO_Staff__c,
      Percentage_Zone__c
    ) %>%
    fix_grades(grade) %>%
    dplyr::arrange(name_account, grade)

}



#' @export
getdata_registration <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        K12_MR_Registration_Deadline__c,
        K12_R2_Registration_Deadline__c,
        EC_MR_Registration_Deadline__c
      from application_settings__c
      "
    ),
    guess_types = FALSE
  ) %>%
    dplyr::select(
      deadline_k12 = K12_MR_Registration_Deadline__c,
      deadline_ec = EC_MR_Registration_Deadline__c
    ) %>%
    dplyr::mutate(dplyr::across(.fns = as.character))

}



#' @export
getdata_sibling <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        Student_OneApp_ID__c,
        PG_Sibling_OneApp_ID__c,
        Reference_Id__c,
        PG_Sibling_Name__c
      from Family_Relationship__c
      where
        Relationship_to_Student__c = 'Sibling'
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    dplyr::select(
      id_relationship = Id,
      student_oneappid = Student_OneApp_ID__c,
      sibling_oneappid = PG_Sibling_OneApp_ID__c,
      id_crosswalk = Reference_Id__c,
      id_sibling = PG_Sibling_Name__c
    ) %>%
    tidyr::separate(
      col = id_crosswalk,
      into = c("id_contact_student", "id_contact_sibling"),
      sep = "_"
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
getdata_roundrobin <- function() {

  sf_query(
    glue(
      "
      select
        Id,
        Grade_Capacity__c,
        Exempt_from_Current_Cycle__c,
        Match_to_Seat_Availability__c
      from Round_Robin__c
      "
    )
  ) %>%
    select(
      id_roundrobin = Id,
      id_gradecapacity = Grade_Capacity__c,
      is_exempt_roundrobin = Exempt_from_Current_Cycle__c,
      match_to_10_1 = Match_to_Seat_Availability__c
    ) %>%
    mutate(
      across(c(
        is_exempt_roundrobin,
        match_to_10_1
      ),
      as.logical
      )
    ) %>%
    mutate(is_exempt_10_1 = !match_to_10_1)

}



# Student -----------------------------------------------------------------



#' @export
query_student <- function() {

  glue::glue(
    "
    select
      CreatedDate,
      School_Year__c,
      Recent_Record__c,
      SchoolForce__Active__c,
      eScholar_LASID__c,
      Local_ID__c,
      OneApp_ID__c,
      SchoolForce__Individual__c,
      Id,
      SchoolForce__Student_First_Name__c,
      SchoolForce__Student_Last_Name__c,
      SchoolForce__Date_of_Birth__c,
      SchoolForce__Gender__c,
      SchoolForce__Address__c,
      SchoolForce__City__c,
      SchoolForce__State__c,
      SchoolForce__Zip_Code__c,
      SchoolForce__Email__c,
      Primary_Contact_Number__c,
      Direct_Cert_Medicaid__c,
      Direct_Cert_SNAP__c,
      Current_Grade__c,
      SchoolForce__School__c,
      SchoolForce__School__r.Name,
      SchoolForce__School__r.School_Code_String__c,
      SchoolForce__School__r.Governance__c,
      Is_Student_In_Terminal_Grade__c,
      Application_Needed__c,
      MR_Application_Submitted__c,
      R2_Application_Submitted__c,
      Future_School_Grade__c,
      Future_School__c,
      Future_School__r.Name,
      Future_School__r.School_Code_String__c,
      Promotion_Decision__c,
      Rising_T9__c,
      Placement_Letter_Year__c,
      Placement_Letter_Template__c,
      Future_School__r.BillingStreet,
      Future_School__r.BillingCity,
      Future_School__r.BillingState,
      Future_School__r.BillingPostalCode,
      Future_School__r.Phone,
      Future_School__r.Registration_Details__c,
      Future_School__r.School_Welcome_Message__c,
      Expelled_From__c,
      Expulsion_ReEntry_Status__c,
      Expulsion_End_Date__c,
      SchoolForce__School__r.Closing_School_Priority__c,
      SchoolForce__School__r.Terminal_Grade__c
    from Schoolforce__Student__c
    "
  )

}



#' @export
format_student <- function(x) {

  x %>%
    dplyr::select(
      date_created = CreatedDate,
      year_student = School_Year__c,
      is_recent = Recent_Record__c,
      is_active = SchoolForce__Active__c,
      lasid_escholar = eScholar_LASID__c,
      lasid = Local_ID__c,
      oneappid = OneApp_ID__c,
      id_contact = SchoolForce__Individual__c,
      id_student = Id,
      student_firstname = SchoolForce__Student_First_Name__c,
      student_lastname = SchoolForce__Student_Last_Name__c,
      student_dob = SchoolForce__Date_of_Birth__c,
      student_gender = SchoolForce__Gender__c,
      student_street = SchoolForce__Address__c,
      student_city = SchoolForce__City__c,
      student_state = SchoolForce__State__c,
      student_zip = SchoolForce__Zip_Code__c,
      student_email = SchoolForce__Email__c,
      student_phone = Primary_Contact_Number__c,
      directcert_medicaid = Direct_Cert_Medicaid__c,
      directcert_snap = Direct_Cert_SNAP__c,
      grade_current = Current_Grade__c,
      id_account_current = SchoolForce__School__c,
      name_account_current = SchoolForce__School__r.Name,
      code_site_current = SchoolForce__School__r.School_Code_String__c,
      governance = SchoolForce__School__r.Governance__c,
      is_terminalgrade = Is_Student_In_Terminal_Grade__c,
      appneeded_r1 = Application_Needed__c,
      appsubmitted_r1 = MR_Application_Submitted__c,
      appsubmitted_r2 = R2_Application_Submitted__c,
      grade_future = Future_School_Grade__c,
      id_account_future = Future_School__c,
      name_account_future = Future_School__r.Name,
      code_site_future = Future_School__r.School_Code_String__c,
      promotion = Promotion_Decision__c,
      is_t9 = Rising_T9__c,
      year_matchletter = Placement_Letter_Year__c,
      lettertype = Placement_Letter_Template__c,
      school_street = Future_School__r.BillingStreet,
      school_city = Future_School__r.BillingCity,
      school_state = Future_School__r.BillingState,
      school_zip = Future_School__r.BillingPostalCode,
      school_phone = Future_School__r.Phone,
      school_registration = Future_School__r.Registration_Details__c,
      school_welcome = Future_School__r.School_Welcome_Message__c,
      id_account_expelled = Expelled_From__c,
      expelled_status = Expulsion_ReEntry_Status__c,
      expelled_date_end = Expulsion_End_Date__c,
      is_priority_closing = SchoolForce__School__r.Closing_School_Priority__c,
      grade_terminal = SchoolForce__School__r.Terminal_Grade__c
    ) %>%
    dplyr::mutate(across(c(
      is_active,
      is_recent,
      is_terminalgrade,
      is_t9,
      is_priority_closing,
      appneeded_r1,
      appsubmitted_r1,
      appsubmitted_r2
      ),
      as.logical
      )
    ) %>%
    dplyr::mutate(across(c(
      student_dob,
      expelled_date_end
      ),
      lubridate::as_date
      )
    ) %>%
    fix_grades(grade_current) %>%
    fix_grades(grade_future) %>%
    fix_grades(grade_terminal)

}



#' @export
getdata_student_active <- function() {

  salesforcer::sf_query(
    glue::glue(
      query_student(),
      "
      where
        SchoolForce__Active__c = true and
        SchoolForce__School__c != null
      ",
      .sep = " "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    format_student()

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
getdata_student_recent <- function() {

  salesforcer::sf_query(
    glue::glue(
      query_student(),
      "
      where
        Recent_Record__c = 'true'
      ",
      .sep = " "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    format_student()

}



#' @export
getdata_student_year <- function(years = date_currentyear()) {

  years <-
    stringr::str_flatten(years, "', '") %>%
    stringr::str_c("('", ., "')")

  salesforcer::sf_query(
    glue::glue_safe(
      "
      select
        School_Year__c,
        OneApp_ID__c,
        SchoolForce__Contact_Id__c,
        Id,
        MR_Application_Submitted__c,
        R2_Application_Submitted__c,
        SchoolForce__School__c,
        SchoolForce__School__r.Name,
        SchoolForce__Active__c
      from Schoolforce__Student__c
      where
        School_Year__c in {years}
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    dplyr::select(
      year = School_Year__c,
      oneappid = OneApp_ID__c,
      id_contact = SchoolForce__Contact_Id__c,
      id_student = Id,
      appsubmitted_r1 = MR_Application_Submitted__c,
      appsubmitted_r2 = R2_Application_Submitted__c,
      id_account_current = SchoolForce__School__c,
      name_account_current = SchoolForce__School__r.Name,
      is_active = SchoolForce__Active__c
    ) %>%
    dplyr::mutate(across(c(
      appsubmitted_r1,
      appsubmitted_r2,
      is_active
      ),
      as.logical
      )
    )

}



#' @export
getdata_waitlist <- function() {

  salesforcer::sf_query(
    glue::glue(
      "
      select
        Id,
        Student__c,
        Grade__c,
        Account_School__c,
        Account_School__r.Name,
        Active__c,
        Archived__c
      from Waitlist_School_Ranking__c
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) %>%
    dplyr::select(
      id_waitlist = Id,
      id_student = Student__c,
      grade_applying = Grade__c,
      id_account = Account_School__c,
      name_account = Account_School__r.Name,
      is_active = Active__c,
      is_archived = Archived__c
    ) %>%
    fix_grades(var = grade_applying)

}


