#' @export
getdata_appschoolranking <- function(date_start = date_appstart()) {
  sf_query(
    glue_safe(
      "
      select
        Application__r.ContactId,
        Application__c,
        Student_OneApp_ID__c,
        Id,
        Grade_Level__c,
        Numerical_Rank__c,
        EC_Program_Type__c,
        Has_Eligibility_Requirements__c,
        Eligibility_Display__c,

        Application__r.Reference_ID__c,
        Reference_ID__c,
        Grade_Level__r.Avela_Grade_Id__c

      from Application_School_Ranking__c

      where
        Application__r.Academic_Term__r.Name = '2025-2026'
        and Application__r.CreatedDate >= {date_start}
        and Application__r.InternalStatus = 'Submitted'
        and Numerical_Rank__c > 0
        and Grade_Level__r.Grade__c != null
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) |>
    select(
      id_contact = Application__r.ContactId,
      id_app = Application__c,
      oneappid = Student_OneApp_ID__c,
      id_appschoolranking = Id,
      id_gradelevel = Grade_Level__c,
      rank = Numerical_Rank__c,
      type_program_ec = EC_Program_Type__c,
      needs_eligibility_k12 = Has_Eligibility_Requirements__c,
      eligibility_k12 = Eligibility_Display__c,

      id_avela_app = Application__r.Reference_ID__c,
      id_avela_choice = Reference_ID__c,
      id_avela_gradelevel = Grade_Level__r.Avela_Grade_Id__c
    ) |>
    mutate(across(c(rank), as.numeric)) |>
    mutate(across(c(needs_eligibility_k12), as.logical)) |>
    group_by(id_app) |>
    arrange(rank) |>
    ungroup()
}


#' Get eligibility/priority records by choice
#'
#' @param term character like "2020-2021"
#' @param date_start character in datetime format like "2020-01-01T00:00:00Z"
#'
#' @return tibble with new field names
#'
#' @export
getdata_ep_choice <- function(
    term = term_next(), date_start = date_appstart()) {
  fields <- tribble(
    ~new, ~old,
    "id_appschoolranking", "Application_School_Ranking__c",
    "id_gradelevel", "Application_School_Ranking__r.Grade_Level__c",
    "id_ep_choice", "Id",
    "name_ep", "Eligibility_Priority__r.Name",
    "status", "Overall_Status__c"
  )

  object_from <- "Student_Eligibility_Priority__c"

  clause_where <- glue_safe(
    "
where
Application_School_Ranking__r.Application__r.Academic_Term__r.Name = '{term}'
and Application_School_Ranking__r.Application__r.CreatedDate >= {date_start}
and Application_School_Ranking__r.Application__r.InternalStatus = 'Submitted'
and Application_School_Ranking__r.Numerical_Rank__c > 0"
  )

  template_query(
    fields$old, fields$new, object_from, clause_where,
    clause_limit = ""
  )
}


#' Get eligibility/priority records by grade level
#'
#' @return tibble with new field names
#'
#' @export
getdata_ep_gradelevel <- function() {
  fields <- tribble(
    ~new, ~old,
    "id_gradelevel", "Grade_Level__c",
    "id_ep_gradelevel", "Id",
    "name_ep", "Eligibility_Priority__r.Name",
    "rank_priority", "Rank__c",
    "perc_priority", "Percentage__c",
  )

  object_from <- "School_Eligibility_Priority__c"

  template_query(
    fields$old, fields$new, object_from,
    clause_where = "", clause_limit = ""
  ) |>
    filter(!is.na(.data$rank_priority)) |>
    mutate(across(c(.data$rank_priority, .data$perc_priority), as.numeric)) |>
    group_by(.data$id_gradelevel) |>
    arrange(.data$id_gradelevel, .data$rank_priority) |>
    ungroup()
}


#' Get expulsion records
#'
#' @return tibble with new field names
#'
#' @export
getdata_expulsion <- function() {
  fields_old <- c(
    "Student_Contact__c",
    "Id",
    "Academic_Term_Enrollment__r.Grade_Level__r.School_Program__c",
    "Re_Apply_Status__c",
    "End_Date__c"
  )
  fields_new <- c(
    "id_contact",
    "id_expulsion",
    "id_program_expelledfrom",
    "status_reapply",
    "date_expulsion_end"
  )

  object_from <- "Expulsion__c"

  clause_where <- glue(
    "
where
Re_Apply_Status__c != null and
((Academic_Term_Enrollment__r.Grade_Level__r.School_Program__c != null and
Re_Apply_Status__c = 'Re-Entry Prohibited') or
(End_Date__c >= 2025-07-01))"
  )

  template_query(
    fields_old, fields_new, object_from, clause_where,
    clause_limit = ""
  )
}


# Utils -------------------------------------------------------------------


template_soql <- function(
    fields_select, object_from, clause_where, clause_limit) {
  glue_safe(
    "
    select  {fields_select}
    from    {object_from}
    {clause_where}
    {clause_limit}
    "
  )
}


template_query <- function(
    fields_old, fields_new, object_from, clause_where, clause_limit) {
  fields_select <- str_flatten(fields_old, collapse = ", ")

  sf_query(
    template_soql(
      fields_select,
      object_from,
      clause_where,
      clause_limit
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) |>
    select(fields_old) |>
    setNames(fields_new)
}
