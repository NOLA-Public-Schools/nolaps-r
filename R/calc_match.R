#' @param x tibble of match records
#' @param schools_waitlist character vector of choice school codes
#'
#' @param ... grouping variables present in data
#'
#' @export
# Function to calculate results for students seeking new assignments
matchcalc_results_seekingnew <- function(x, schools_waitlist, ...) {
  # Get all match parts
  p <- match_parts_all(x, schools_waitlist, ...)

  # Get students seeking new assignments
  seekingnew <- match_parts_seekingnew(x, schools_waitlist, ...)
  # Get fallback ineligible students
  fallback_inelig <- matchcalc_fallback_inelig(p)
  # Get unassigned ineligible students
  unassign_inelig <- matchcalc_unassign_inelig(p)

  x |>
    # Filter to include only students seeking new assignments
    filter((.data$`STUDENT ID` %in% seekingnew$`STUDENT ID`)) |>
    # Exclude fallback ineligible students
    filter(!(.data$`STUDENT ID` %in% fallback_inelig$`STUDENT ID`)) |>
    # Exclude unassigned ineligible students
    filter(!(.data$`STUDENT ID` %in% unassign_inelig$`STUDENT ID`)) |>
    # Exclude students with "Ineligible" assignment status
    filter(.data$`ASSIGNMENT STATUS` != "Ineligible") |>
    # Exclude students with "Not Processed" assignment status
    filter(.data$`ASSIGNMENT STATUS` != "Not Processed") |>
    # Add columns to indicate if the student is accepted and if they are guaranteed
    mutate(
      is_accepted = .data$`ASSIGNMENT STATUS` == "Accepted",
      is_guaranteed = !is.na(.data$`GUARANTEED?`)
    ) |>
    # Group by the specified variables
    group_by(...) |>
    # Summarize the data to calculate various metrics
    summarize(
      n_seekingnew = length(unique(.data$`STUDENT ID`)),  # Number of students seeking new assignments
      n_seekingnew_accept =
        sum(.data$is_accepted & !.data$is_guaranteed),  # Number of accepted students not guaranteed
      n_seekingnew_accept_top3 =
        sum(.data$is_accepted & !.data$is_guaranteed & .data$`CHOICE RANK` %in% 1:3),  # Number of accepted students not guaranteed in top 3 choices
      n_seekingnew_accept_top1 =
        sum(.data$is_accepted & !.data$is_guaranteed & .data$`CHOICE RANK` == 1),  # Number of accepted students not guaranteed in top 1 choice
      n_seekingnew_fallback =
        sum(.data$is_accepted & .data$is_guaranteed),  # Number of accepted students guaranteed
      n_seekingnew_unassigned =
        .data$n_seekingnew - .data$n_seekingnew_accept - .data$n_seekingnew_fallback  # Number of unassigned students
    ) |>
    # Calculate rates based on the summarized data
    mutate(
      rate_accept = .data$n_seekingnew_accept / .data$n_seekingnew,  # Acceptance rate
      rate_accept_top3 = .data$n_seekingnew_accept_top3 / .data$n_seekingnew,  # Acceptance rate for top 3 choices
      rate_accept_top1 = .data$n_seekingnew_accept_top1 / .data$n_seekingnew,  # Acceptance rate for top 1 choice
      rate_fallback = .data$n_seekingnew_fallback / .data$n_seekingnew,  # Fallback rate
      rate_unassigned = .data$n_seekingnew_unassigned / .data$n_seekingnew  # Unassigned rate
    )
}

# Function to summarize match outcomes by program
match_calc_program_all <- function(m, schools_waitlist, ...) {
  m |>
    # Add columns to indicate various statuses and conditions
    mutate(
      is_eligproc = !(.data$`ASSIGNMENT STATUS` %in% c(
        "Ineligible", "Not Processed"
      )),  # Check if eligible and processed
      is_gtee = !is.na(.data$`GUARANTEED?`),  # Check if guaranteed
      is_seeknew = !.data$is_gtee,  # Check if seeking new assignment
      is_accept = .data$`ASSIGNMENT STATUS` == "Accepted",  # Check if accepted
      is_acceptgtee = .data$is_accept & .data$is_gtee,  # Check if accepted and guaranteed
      is_acceptnew = .data$is_accept & .data$is_seeknew,  # Check if accepted and seeking new assignment
      is_top1 = .data$`CHOICE RANK` == 1,  # Check if top 1 choice
      is_top3 = .data$`CHOICE RANK` %in% 1:3  # Check if top 3 choices
    ) |>
    # Group by the specified variables
    group_by(...) |>
    # Summarize the data to calculate various metrics
    summarize(
      n_choices = n(),  # Number of choices
      n_gtee = sum(.data$is_gtee),  # Number of guaranteed choices
      n_seeknew = sum(.data$is_seeknew),  # Number of students seeking new assignments
      n_seeknew_top3 = sum(.data$is_seeknew & .data$is_top3),  # Number of students seeking new assignments in top 3 choices
      n_seeknew_top1 = sum(.data$is_seeknew & .data$is_top1),  # Number of students seeking new assignments in top 1 choice
      n_choices_eligproc = sum(.data$is_eligproc),  # Number of eligible and processed choices
      n_seeknew_eligproc = sum(.data$is_eligproc & .data$is_seeknew),  # Number of eligible and processed students seeking new assignments
      n_seeknew_top3_eligproc = sum(
        .data$is_eligproc & .data$is_seeknew & .data$is_top3
      ),  # Number of eligible and processed students seeking new assignments in top 3 choices
      n_seeknew_top1_eligproc = sum(
        .data$is_eligproc & .data$is_seeknew & .data$is_top1
      ),  # Number of eligible and processed students seeking new assignments in top 1 choice
      n_accept = sum(.data$is_accept),  # Number of accepted students
      n_rollforward = sum(.data$is_acceptgtee & .data$is_top1),  # Number of accepted and guaranteed students in top 1 choice
      n_fallback = sum(.data$is_acceptgtee & !.data$is_top1),  # Number of accepted and guaranteed students not in top 1 choice
      n_acceptnew = sum(.data$is_acceptnew),  # Number of accepted students seeking new assignments
      n_acceptnew_top3 = sum(.data$is_acceptnew & .data$is_top3),  # Number of accepted students seeking new assignments in top 3 choices
      n_acceptnew_top1 = sum(.data$is_acceptnew & .data$is_top1)  # Number of accepted students seeking new assignments in top 1 choice
    ) |>
    # Calculate rates based on the summarized data
    mutate(
      rate_acceptnew = .data$n_acceptnew / .data$n_seeknew_eligproc,  # Acceptance rate for new assignments
      rate_acceptnew_top3 = .data$n_acceptnew_top3 / .data$n_seeknew_eligproc,  # Acceptance rate for new assignments in top 3 choices
      rate_acceptnew_top1 = .data$n_acceptnew_top1 / .data$n_seeknew_eligproc  # Acceptance rate for new assignments in top 1 choice
    )
}

# Function to calculate results for students seeking new assignments with sibling priority
matchcalc_results_seekingnew_sibling <- function(x, schools_waitlist, ...) {
  # Get all match parts
  p <- match_parts_all(x, schools_waitlist, ...)

  # Get students seeking new assignments
  seekingnew <- match_parts_seekingnew(x, schools_waitlist, ...)
  # Get fallback ineligible students
  fallback_inelig <- matchcalc_fallback_inelig(p)
  # Get unassigned ineligible students
  unassign_inelig <- matchcalc_unassign_inelig(p)

  x |>
    # Filter to include only students seeking new assignments
    filter((`STUDENT ID` %in% seekingnew$`STUDENT ID`)) |>
    # Exclude fallback ineligible students
    filter(!(`STUDENT ID` %in% fallback_inelig$`STUDENT ID`)) |>
    # Exclude unassigned ineligible students
    filter(!(`STUDENT ID` %in% unassign_inelig$`STUDENT ID`)) |>
    # Include only students with sibling priority
    filter(str_detect(.data$`QUALIFIED PRIORITIES`, "Sibling")) |>
    # Exclude students with "Ineligible" assignment status
    filter(`ASSIGNMENT STATUS` != "Ineligible") |>
    # Exclude students with "Not Processed" assignment status
    filter(`ASSIGNMENT STATUS` != "Not Processed") |>
    # Group by student ID
    group_by(`STUDENT ID`) |>
    # Summarize to check if the student is accepted with sibling priority
    summarize(is_acceptednew_sibling = "Accepted" %in% `ASSIGNMENT STATUS`) |>
    # Calculate the acceptance rate for new assignments with sibling priority
    summarize(rate_acceptednew_sibling = mean(is_acceptednew_sibling))
}

# Function to calculate results for students seeking new assignments who are unassigned
matchcalc_results_seekingnew_unassigned <- function(x, schools_waitlist, ...) {
  # Get all match parts
  p <- match_parts_all(x, schools_waitlist, ...)

  # Get students seeking new assignments
  seekingnew <- match_parts_seekingnew(x, schools_waitlist, ...)
  # Get fallback ineligible students
  fallback_inelig <- matchcalc_fallback_inelig(p)
  # Get unassigned ineligible students
  unassign_inelig <- matchcalc_unassign_inelig(p)

  seekingnew |>
    # Exclude fallback ineligible students
    filter(!(`STUDENT ID` %in% fallback_inelig$`STUDENT ID`)) |>
    # Exclude unassigned ineligible students
    filter(!(`STUDENT ID` %in% unassign_inelig$`STUDENT ID`)) |>
    # Include only unassigned students
    filter(is.na(rank_accepted)) |>
    # Group by the specified variables
    group_by(...) |>
    # Summarize the data to calculate various metrics
    summarize(
      n_seekingnew_unassigned = n(),  # Number of unassigned students seeking new assignments
      n_seekingnew_unassigned_k9 = sum(GRADE %in% c("K", "9")),  # Number of unassigned students seeking new assignments in grades K and 9
      n_seekingnew_unassigned_3orless = sum(n_choices <= 3)  # Number of unassigned students seeking new assignments with 3 or fewer choices
    ) |>
    # Calculate rates based on the summarized data
    mutate(
      rate_k9 = n_seekingnew_unassigned_k9 / n_seekingnew_unassigned,  # Rate of unassigned students seeking new assignments in grades K and 9
      rate_3orless = n_seekingnew_unassigned_3orless / n_seekingnew_unassigned  # Rate of unassigned students seeking new assignments with 3 or fewer choices
    )
}