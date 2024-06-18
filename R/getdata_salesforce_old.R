#' @export
mutate_code_site_group <- function(x) {
  x %>%
    mutate(code_site_group = case_when(
      str_detect(code_site, "^[:alnum:]{5,6}") ~ str_pad(
        str_extract(code_site, "^[:alnum:]{5,6}"),
        width = 6,
        side = "left",
        pad = "0"
      ),
      .default = code_site
    ))
}


#' @export
getdata_account <- function() {
  salesforcer::sf_query(
    glue(
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
    dplyr::mutate(dplyr::across(
      c(
        is_highdemand
      ),
      as.logical
    )) %>%
    dplyr::mutate(dplyr::across(
      c(
        lon, lat
      ),
      as.numeric
    ))
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
        Operator__r.Name,
        Contract_Start_Date__c,
        Contract_End_Date__c,
        Initial_Contract_Term__c,
        Total_Enrollment__c,
        Letter_Grade__c,
        School_Performance_Score__c,
        Rate_Economically_Disadvantaged__c,
        Rate_Minority__c,
        Rate_Limited_English_Proficiency__c,
        Rate_Disability__c
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
      name_operator = Operator__r.Name,
      date_contract_start = Contract_Start_Date__c,
      date_contract_end = Contract_End_Date__c,
      is_initialterm = Initial_Contract_Term__c,
      n_total = Total_Enrollment__c,
      grade_sps_cb = Letter_Grade__c,
      index_sps_cb = School_Performance_Score__c,
      rate_disadvantage = Rate_Economically_Disadvantaged__c,
      rate_minority = Rate_Minority__c,
      rate_limeng = Rate_Limited_English_Proficiency__c,
      rate_swd = Rate_Disability__c
    ) %>%
    mutate(across(
      c(
        n_total,
        starts_with("rate_")
      ),
      as.numeric
    )) %>%
    mutate(across(
      c(
        starts_with("date_")
      ),
      as_date
    )) %>%
    mutate(across(
      c(
        starts_with("is_")
      ),
      as.logical
    ))
}


#' @export
getdata_contact <- function() {
  salesforcer::sf_query(
    glue(
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
getdata_facility_old <- function() {
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
        Capacity_Lower__c,
        Capacity_Upper__c,
        Facility_Condition_Index__c,
        Cost_per_Square_Foot__c,
        Cost_Tier__c,
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
      capacity_lower = Capacity_Lower__c,
      capacity_upper = Capacity_Upper__c,
      condition_index = Facility_Condition_Index__c,
      cost_squarefoot = Cost_per_Square_Foot__c,
      cost_tier = Cost_Tier__c,
      tract = Census_Tract__c
    ) %>%
    mutate(across(
      c(
        lon,
        lat,
        program_capacity,
        capacity_lower,
        capacity_upper,
        condition_index,
        cost_squarefoot,
      ),
      as.numeric
    )) %>%
    mutate(across(
      c(
        has_structure
      ),
      as.logical
    ))
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
    mutate(across(
      c(
        seats_available,
        currentregister_active,
        target_101,
        target_101_future,
        target_101_requested,
        n_sections,
        students_per_section
      ),
      as.numeric
    )) %>%
    mutate(across(
      c(
        is_siblingunification,
        is_reactivation
      ),
      as.logical
    ))
}


#' @export
getdata_sibling <- function() {
  salesforcer::sf_query(
    glue(
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
getdata_waitlist <- function() {
  salesforcer::sf_query(
    glue(
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
