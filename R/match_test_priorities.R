match_test_priorities <- function(dir_review, match, gradelevels, eps_gradelevel, eps_choice) {
  match_test_closing(
    dir_review = dir_review,
    match = match,
    gradelevels = gradelevels,
    eps_gradelevel = eps_gradelevel,
    eps_choice = eps_choice
  )

  match_test_sibling(
    dir_review = dir_review,
    match = match,
    gradelevels = gradelevels,
    eps_gradelevel = eps_gradelevel,
    eps_choice = eps_choice
  )

  match_test_distance(
    dir_review = dir_review,
    match = match,
    gradelevels = gradelevels,
    eps_gradelevel = eps_gradelevel,
    eps_choice = eps_choice
  )

  match_test_zone(
    dir_review = dir_review,
    match = match,
    gradelevels = gradelevels,
    eps_gradelevel = eps_gradelevel,
    eps_choice = eps_choice
  )
}
