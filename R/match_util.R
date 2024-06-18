#' K12 programs with waitlists
#'
#' @return character vector
#'
#' @export
schools_waitlist <- function() {
  c(
    "331001_C_FISLCampFrench",
    "331001_C_SISLCampSpanish",
    "331001_E_FISLDixonFrench",
    "331001_E_SISLDixonSpanish",
    "331001_O_SISLOlivierSpanish",
    "347001LFNOPK4T2",
    "348001NOMMA",
    "3C2001_FHLFrench",
    "3C2002_FHUFrench",
    "WAZ001_FAUFrenchLS",
    "WAZ001_FAUFrenchUS",
    "WAZ001_MAUMontessoriLSLA4",
    "WAZ001_MAUMontessoriUS",
    "WBE001Willow",
    "WBE001Willow_community_1",
    "WBE001Willow_community_2",
    "WBE001Willow_ed_1",
    "WBE001Willow_tulane_1",
    "WBE001Willow_tulane_2",
    "WBH001LakeForest",
    "WBH001LakeForest_tier_1",
    "WBH001LakeForest_tier_2"
  )
}


#' Programs requiring 8th grade applicants to be at least 15 by 9/30
#'
#' @return character vector
#'
#' @export
schools_net <- function() {
  c(
    "360001NETCC",
    "360002NETGentilly",
    "360003NETEast"
  )
}


#' Match outcome categories used in Salesforce
#'
#' @return tibble
#'
#' @export
matchtypes_salesforce <- function() {
  tribble(
    ~matchtype,           ~matchtype_salesforce,
    "ec_acceptednew",     "EC_Matched",
    "ec_acceptednew_wl",  "EC_Matched+WL",
    "ec_fallback",        "EC_Default",
    "ec_fallback_wl",     "EC_Default+WL",
    "ec_unassigned",      "EC_Unmatched",
    "ec_unassigned_wl",   "EC_Unmatched+WL",
    "ec_guaranteed",      "EC_Rollforward",
    "k12_acceptednew",    "Matched",
    "k12_acceptednew_wl", "Matched+WL",
    "k12_fallback",       "Default",
    "k12_fallback_wl",    "Default+WL",
    "k12_unassigned",     "Unmatched",
    "k12_unassigned_wl",  "Unmatched+WL",
    "k12_guaranteed",     "Rollforward",
  )
}
