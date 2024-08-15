## -----------------------------------------------------------------------------
##
## Script name: labor_supply_elasticity_analysis_dataset_creation.R
##
## Purpose of script: Employment Family 1: Labor Supply Elasticity dataset generation
##
## Author: Ethan Sansom
##
## Date Created: 2023-07-19
##
## -----------------------------------------------------------------------------

# Documentation ----------------------------------------------------------------

# Final Outcome coding instructions here (Employment Outcomes Documentation):
# https://docs.google.com/document/d/1-CsM7F7S9Kxf2UXhhTGthMhwobviusnbNRHgd1V2sBc/edit#heading=h.30j0zll

# ORDER OF OPERATION -----------------------------------------------------------
# I.   Load ALL Clean Survey Datasets + RID checks
# II.  Create long (RID x Survey) panel datsets for each module / datasource
# III. Create Master long  (RID x Survey) panel datset for family
# IV.  Transform to final wide Analysis Dataset
# V.   Save outputs

# OUTSTANDING TODOS ------------------------------------------------------------

# TODO ES 24/05/27:
# Do we want `partner_in_hh` to have a post-treatment version defined only via
# `so` post-treatment surveys? The main version of `partner_in_hh` relies on the
# `roster` (and uses `so` only when it's missing). We currently don't use the PT
# surveys from `so`.

# TODO ES 2024/08/06: There is a discrepancy in the definition of `partner_in_hh`
# described below. Need to decide which version to use.
#
## Household Stability
# In `final_intratd` the outcome is defined only at Baseline, using `intratd1`,
# which just asks “are you living with a romantic partner or spouse?”
# (despite the variable name it is not just spouse)
# - `partner_in_hh_bl` <- uses `intratd1_living_spouse` (MB1)
#
# In `final_so` , having a partner in the household is determined by `so3_live_together`
# only. It is conditional, meaning that participants who don’t have a partner
# (i.e `so1_relationship == 0`) have `partner_in_hh` is NA
# - `partner_in_hh_bl`  <- uses `so3_live_together` (MB3)
# - `partner_in_hh_ml` <- uses `so3_live_together` (MM2)
# - `parnter_in_hh_el`  <- uses `so3_live_together` (ME3)
#
# In `final_roster` we use the `partner_in_hh` variable already defined in the
# `read_src(“new_roster”)` dataset at SRC BL, ML, EL
#
# Aggregation:
# Uses `any_one()` as the aggregation function and `combine_surveys()` as the
# merge function, meaning that `partner_in_hh_* == 1` if any of `final_intratd`,
# `final_so`, or `final_roster` have `partner_in_hh_* == 1`
# - BL <- uses final_intratd, final_so, final_roster
# - ML <- uses final_so, final_roster,
# - EL <- uses final_so, final_roster
#
## Labor Supply
# In `final_so` we use `intratd1_living_spouse` at Baseline and `so1_relationship`,
# `so3_live_together` at ML, EL. The outcome is unconditional, `partner_in_hh == 0`
# when the participant does not have a partner (i.e. `so1_relationship == 0`)
# - `partner_in_hh_bl`  <- uses `so1_relationship/so3_live_together` (MB3)
# - `partner_in_hh_ml` <- uses `so1_relationship/so3_live_together` (MM2)
# - `parnter_in_hh_el`  <- uses `so1_relationship/so3_live_together` (ME3)
#
# In `final_roster` this is defined the same as in Household Stability (i.e. we use
# the `partner_in_hh` outcome defined in `read_src(“new_roster”)` for BL/ML/EL)
#
# Aggregation:
# Uses `non_missing_src()` as the merge function, meaning that at BL, ML, EL
#`final_roster` is used by default to create `partner_in_hh` and `final_so` is
# used only when the `final_roster$partner_in_hh` is missing
# This version of `partner_in_hh` mechanically has fewer participant’s living
# with a partner than the version in Household Stability because when:
# `final_so$partner_in_hh == 1 & final_roster$partner_in_hh == 0`
# - Household Stability has: aggregate `partner_in_hh == 1`
# - Labor Supply has: aggregate `partner_in_hh == 0`


# RESOLVED TODOS ---------------------------------------------------------------

# DONE SS 12/29
# RESOLVED Eva:
# - Should the exploratory unemploy_rsn_* dummies (reasons for unemployment) be
# made unconditional (i.e. == 0 IF employed)?
# EV: Yes, that would be great.

# DONE SS 12/29
# - How should missing values in the work_hrs_wk sum be treated? The standard approach
#   of replacing with the within-survey-treatment mean may not be appropriate in this case.
# EV: I agree with the intuition that we should be taking the mean among those who report a 3rd/etc. job.

# DONE Currently "partner_works" is conditional and "partner_work_hrs", "is_breadwinner"
#   are unconditional, should we make them all unconditional?
# EV: Yes, these should all be unconditional. Actually, I see I left a comment in
# the employment documentation gdoc about this.

# DONE Should the variables for the (outside of index) income elasticity of labor supply estimate be aggregated?
# EV: I think these could be helpful to have in the wide data set as well, thanks!

# DONE EV: FLAG/clarification sought: it seems like for some variables
# (e.g., unemp_elderly_bl, unemp_elderly_el, unemp_gave_up_el) skim(test_agg, -matches("(_ms|_n_nm)$"))
# yields all 0s, but for work_hrs_wk_alt_el it yields NaN. Not sure what is going on here.
#
# ES: `test_agg` runs the aggregation on a small subset of RIDs, so rare outcomes
# `unemp_*` will be all 0. `work_hrs_wk_alt_el` is not asked at Endline, has been
# removed.

# RESOLVED Ethan: `work_hrs_wk_alt_el` is all NA. Check the docs to see if `work_hrs_wk_alt`
#             was asked at Endline and remove `work_hrs_wk_alt_el` from wide if not asked.
# SS 12/29/2023 Removing the var work_hrs_wk_alt_el due to not being asked in EL

# DONE SS 1/5/23:
# - Add Household Roster Sections C and D once available
# - Create the following missing HH Roster outcomes:
# - partner_work_hrs

# 0. Declare Constants ---------------------------------------------------------

# Whether numeric outcomes are winsorized
WINSOR <- TRUE

# 0.5 Define Helper Functions --------------------------------------------------

# Calls tabyl(...), but with all numeric inputs truncated to range (-inf, 1]
trim_tabyl <- function(dataframe, ...) {
  dataframe %>%
    mutate(across(c(...), ~if_else(is.numeric(.x) & .x > 0, 1, .x))) %>%
    tabyl(...)
}

# 1. Load ALL Clean Survey Datasets + RID checks -------------------------------

### SRC Data -------------------------------------------------------------------

# Load SRC Section Datasets
section_g <- read_src("G")
section_i <- read_src("I")
section_j <- read_src("J")
section_ab <- read_src("AB")
section_h <- read_src("H")
section_m <- read_src("M")
section_k <- read_src("K")

# ES 24/05/15: Load new roster
roster <- read_src("new_roster")

### Qualtrics Data -------------------------------------------------------------

# Load Qualtrics Module Level Datasets
workhrs <- read_qualtrics("workhrs")
workqual <- read_qualtrics("workqual")
intratd <- read_qualtrics("intratd")
so <- read_qualtrics("so")

# 2. Create long (RID x Survey) panel datasets for each module / datasource ----

## ROSTER ######################################################################

# NOTE ES 24/05/15: See the link below for details on the `roster` outcomes.
# https://docs.google.com/document/d/1IOfHr69904ySPb7fjA7U7kfAANc0tuKgYa17tg_z0yE/edit

# NOTE ES 24/05/15: Map of `Old Name -> Current Name` for `final_roster` outcomes.
# 1.  spouse_in_hh              -> spouse_in_hh
# 2.  partner_gf_bf_in_hh       -> partner_gf_bf_in_hh
# 3.  rom_rel_in_hh             -> partner_in_hh
# 4.  eitc_child_in_hh          -> eitc_child_in_hh
# 5.  eitc_child_hh_n           -> eitc_child_hh_n
# 6.  head_of_hh                -> head_of_hh
# 7.  hh_n_adults               -> adult_hh_n
# 8.  dual_inc                  -> dual_inc
# 9.  dual_bl_inc               -> dual_bl_inc
# 10. partner_employed          -> partner_works
# 11. partner_work_hrs_wk       -> partner_work_hrs
# 12. partner_work_hrs_wk_cnd   -> partner_work_hrs_cd
# 13. n_oth_hhm_emp             -> adult_works_n
# 14. n_oth_hhm_emp_cnd         -> adult_works_n_cnd
# 15. non_rom_works             -> non_rom_works
# 16. non_rom_work_hrs          -> non_rom_work_hrs
# 17. parent_works              -> parent_works
# 18. parent_work_hrs           -> parent_work_hrs
# 19. adult_child_works         -> adult_kid_works
# 20. adult_child_work_hrs      -> adult_kid_work_hrs

### Create Outcomes ------------------------------------------------------------

final_roster <- roster %>%
  # Join in `employed` from Section G
  left_join(
    section_g %>% select(rid, survey_name, employed),
    by = c("rid", "survey_name")
  ) %>%

  mutate(
    # Set all DKR values to NA
    across(everything(), dkr_to_na),

    # `partner_work_hrs`, conditional on the participant having a partner, spouse,
    # girlfriend, or boyfriend in the household.
    partner_work_hrs_cd = case_when(partner_in_hh %in% 0 ~ NA_real_, TRUE ~ partner_work_hrs),

    # `adult_works_n`, conditional on the participant having other adult HHM's.
    adult_works_n_cnd = case_when(adult_in_hh %in% 0 ~ NA_real_, TRUE ~ adult_works_n),

    # Whether the participant lives in a dual income household
    dual_inc = case_when(
      # IF either the participant's or their partner's work status is unknown, THEN set to NA
      is_missing(partner_works) | is_missing(employed) ~ NA_real_,

      # OTHERWISE, check whether both the participant and their partner is employed
      TRUE ~ as.numeric(partner_works %in% 1 & employed %in% 1)
    )
  ) %>%

  # Whether the participant lived in a dual income household at Baseline
  mutate(dual_bl_inc = dual_inc[survey_name == "SRC Baseline"], .by = rid) %>%

  # Checks
  expect_col_relation(
    dual_inc,
    list(
      is_missing(employed) | is_missing(partner_works) ~ is.na,
      partner_works == 0 | employed == 0 ~ \(x) x == 0,
      partner_works == 1 & employed == 1 ~ \(x) x == 1
    )
  ) %>%
  expect_col_relation(partner_work_hrs_cd, partner_in_hh %in% 0 ~ is.na) %>%
  expect_col_relation(adult_works_n_cnd, adult_in_hh %in% 0 ~ is.na) %>%
  expect_df_expr(survey_name != "SRC Baseline" | is_same(dual_inc, dual_bl_inc)) %>%

  # Select required module and outcome variables
  select(
    all_of(module_vars),

    # Participant's spouse
    spouse_in_hh,
    # Participant's partner, boyfriend, or girlfriend
    partner_gf_bf_in_hh,
    # Participant's spouse, partner, boyfriend, or girlfriend
    partner_in_hh,
    partner_works,
    partner_work_hrs,
    partner_work_hrs_cd,
    # Participant's EITC eligible dependent
    eitc_child_in_hh,
    eitc_child_hh_n,
    # Adult (aged 18+)
    adult_in_hh,
    adult_hh_n,
    adult_works_n,
    adult_works_n_cnd,
    # Participant's adult (aged 18+) biological/adopted/foster/step child
    adult_kid_works,
    adult_kid_work_hrs,
    # Participant's adult household member who is not a romantic relation
    non_rom_works,
    non_rom_work_hrs,
    # Participant's parent
    parent_works,
    parent_work_hrs,

    # Other outcomes
    hh_size_n,
    head_of_hh,
    dual_inc,
    dual_bl_inc
  ) %>%

  # NOTE ES 24/05/15: Can safely ignore `adult_hh_n`, `adult_works_n` warnings. These
  #                   are due to (correct) values which rarely occur.
  expect_valid_long() %>%
  display_expectations(levels = c("fail", "caution"))
# display_expectations()

### Checks ---------------------------------------------------------------------
skim_survey(final_roster)

final_roster %>%
  summarize(partner_works_perc = 100 * (sum(partner_works %in% 1) / n()), .by = survey_name)

final_roster %>%
  group_by(survey_name) %>%
  count(partner_works)

# Check: `partner_works` vs. `partner_in_hh`
final_roster %>%
  reorder_surveys(reverse = FALSE) %>%
  group_by(survey_name) %>%
  count(partner_in_hh, partner_works)

# Check: `partner_in_hh` vs. `partner_work_hrs_cd`
final_roster %>%
  reorder_surveys(reverse = FALSE) %>%
  group_by(survey_name) %>%
  mutate(partner_work_hrs_cd = cut(partner_work_hrs_cd, c(-1, 0, 20, 40, 60, 80))) %>%
  count(partner_in_hh, partner_work_hrs_cd) %>%
  print(n = nrow(.))

# Check: `partner_in_hh` vs. `partner_work_hrs`
final_roster %>%
  reorder_surveys(reverse = FALSE) %>%
  group_by(survey_name) %>%
  mutate(
    partner_work_hrs = cut(partner_work_hrs, c(-1, 0, 20, 40, 60, 80)),
    partner_work_hrs = fct_relabel(partner_work_hrs, ~ifelse(.x == "(-1,0]", "[0]", .x))
  ) %>%
  count(partner_in_hh, partner_work_hrs) %>%
  print(n = nrow(.))

final_roster %>%
  reorder_surveys(reverse = FALSE) %>%
  group_by(survey_name) %>%
  count(partner_in_hh) %>%
  mutate(
    perc = percent(n / sum(n)),
    perc_of_non_missing = {
      temp_n <- if_else(is.na(partner_in_hh), 0, n)
      percent(temp_n / sum(temp_n))
    }
  )

## SECTION G ###################################################################
# 1. employed
# 2. work_hrs_wk_min
# 3. work_hrs_wk_max

# Create outcomes
final_g <- section_g %>%
  mutate(
    # Dummy for if participant is employed
    employed = dkr_to_na(employed),

    # Minimum hours worked weekly; IF unemployed, THEN set to 0
    work_hrs_wk_min = if_else(employed %in% 0, 0, dkr_to_na(work_hrs_per_week_least_main)),

    # Maximum hours worked weekly; IF unemployed, THEN set to 0
    work_hrs_wk_max = if_else(employed %in% 0, 0, dkr_to_na(work_hrs_per_week_most_main))
  ) %>%

  # Select module and outcome variables
  select(all_of(module_vars), employed, work_hrs_wk_min, work_hrs_wk_max) %>%

  # Checks
  expect_col_dummy(employed) %>%
  expect_col_between(work_hrs_wk_min, left = 0, right = 140) %>%
  expect_col_between(work_hrs_wk_max, left = 0, right = 140) %>%
  expect_col_relation(
    c(work_hrs_wk_min, work_hrs_wk_max),
    employed %in% 0 ~ \(x) x %in% 0
  ) %>%

  expect_valid_long() %>%
  display_expectations(c("fail", "caution"))

### Checks  --------------------------------------------------------------------

# Skim
skim_survey(final_g)

# Confirm: If unemployed, then work hours pre week min (least) is 0
trim_tabyl(section_g, employed, work_hrs_per_week_least_main)
trim_tabyl(final_g, employed, work_hrs_wk_min)

# Confirm: If unemployed, then work hours pre week max (most) is 0
trim_tabyl(section_g, employed, work_hrs_per_week_most_main)
trim_tabyl(final_g, employed, work_hrs_wk_max)

## SECTION M ###################################################################

# NOTE ES 24/05/27: Map of `Old Name -> Current Name` for `final_m` outcomes.
#      Names have been changed to match those of the identical Income outcomes.
#
# 1. inc_ind_total_rep      -> inc_ind_tot_rep
# 2. inc_hh_total           -> inc_hh_total_rep_win
# 3. inc_ind_passive
# 4. inc_ind_transfers
# 5. inc_ind_gifts
# 6. inc_spouse

### Create outcomes ------------------------------------------------------------

# NOTE ES: Per discussion with PK 24/05/24:
# - use the measures from the Income script in financial health to create these outcomes
# - in particular, use `inc_oth_hh` in the construction of `inc_spouse`

# FLAG: Loading income variables from the wide dataset, to ensure that this script
#       is consistent with the income family.
income_data_long <- read_rds("financial_health/02_data/income_rid_x_survey_long.Rds")

# Select the required Income outcomes
final_m <- inner_join(
  # Outcomes created in Income module "I/J/M"
  income_data_long %>%
    filter(module == "I/J/M") %>%
    # Change module in preparation for joining with module "M" variables
    mutate(module = "M") %>%
    select(
      all_of(module_vars),
      inc_oth_hh
    ) %>%
    # FLAG: This module contains a duplicate NA observations for every RID x Survey.
    #       Removing these duplicates.
    group_by(rid, survey_name) %>%
    # The duplicate value is always NA, which is sorted last in `arrange()`. So
    # slicing the first observation gets the actual (non-duplicate) value.
    arrange(inc_oth_hh) %>%
    slice_head(n = 1) %>%
    ungroup(),

  # Outcomes created in Income module "M"
  income_data_long %>%
    filter(module == "M") %>%
    select(
      all_of(module_vars),
      inc_ind_tot_rep,
      inc_hh_total_rep_win,
      inc_ind_passive,
      inc_ind_transfers,
      inc_ind_gifts
    ),
  by = module_vars
) %>%
  expect_valid_long() %>%
  display_expectations(levels = c("fail", "caution"))
# display_expectations()

# Stop if the join went wrong
stopifnot("`final_m` must contain 9,000 observations." = nrow(final_m) == 9000)

# Create outcomes
final_m <- final_m %>%

  # Join in `spouse_in_hh` from `roster`
  left_join(
    roster %>% select(rid, survey_name, spouse_in_hh),
    by = c("rid", "survey_name")
  ) %>%

  mutate(
    # Individual Income Total Reported
    inc_ind_tot_rep = dkr_to_na(inc_ind_tot_rep),

    # Total household income before taxes over the last 12 months.
    inc_hh_total_rep_win = dkr_to_na(inc_hh_total_rep_win),

    # Total passive individual income  (i.e. interest, dividends, or rental income)
    # before taxes over the last 12 months.
    inc_ind_passive = dkr_to_na(inc_ind_passive),

    # Total individual income from transfers
    inc_ind_transfers = dkr_to_na(inc_ind_transfers),

    # Total individual income from gifts
    inc_ind_gifts = dkr_to_na(inc_ind_gifts),

    # Income of other household members (intermediate variables)
    inc_oth_hh = dkr_to_na(inc_oth_hh)
  ) %>%

  mutate(
    # Total income of participant's spouse before taxes over the last 12 months
    inc_spouse = case_when(
      # IF participant does not have an in-household spouse OR spouse-status is unknown,
      # THEN set to NA
      is_missing(spouse_in_hh) | spouse_in_hh %notin% 1 ~ NA_real_,

      # IF income of other (non-participant) household members is unknown, THEN set to NA
      is_missing(inc_oth_hh) ~ NA_real_,

      # OTHERWISE, assume spouse income is the income of non-participant household
      # members (this is likely not true if there are multiple working adults in
      # the household) with a floor of $0.
      TRUE ~ pmax(inc_oth_hh, 0)
    )
  ) %>%

  # Checks
  expect_df_expr(spouse_in_hh %notin% 1 | is_same(inc_spouse, inc_oth_hh)) %>%

  # Select module and outcome variables
  select(
    all_of(module_vars),
    inc_ind_tot_rep,
    inc_hh_total_rep_win,
    inc_ind_passive,
    inc_ind_transfers,
    inc_ind_gifts,
    inc_spouse
  ) %>%

  # Checks
  expect_col_between("inc_spouse", left = 0, right = 350000) %>%
  expect_valid_long() %>%
  display_expectations(c("fail", "caution"))

### Checks  --------------------------------------------------------------------
skim_survey(final_m)

## SECTION H ###################################################################
# 1.  unemp_unavail
# 2.  unemp_lack_skill
# 3.  unemp_transport
# 4.  unemp_ill
# 5.  unemp_childcare
# 6.  unemp_elderly
# 7.  unemp_edu
# 8.  unemp_prefer_sah
# 9.  unemp_pers_resp
# 10. unemp_gave_up
# 11. unemp_other

### Create outcomes ------------------------------------------------------------

final_h <- section_h %>%
  # Join in `employed` from Section G
  left_join(
    section_g %>% select(rid, survey_name, employed),
    by = c("rid", "survey_name")
  ) %>%

  # Rename `unemployed_why_*` dummies to `unemp_*`, excluding `unemployed_why_other_spec`
  select(-ends_with("_spec")) %>%
  rename_with(
    .cols = starts_with("unemploy_why"),
    .fn = ~str_replace(.x, "unemploy_why_", "unemp_")
  ) %>%

  # Create `unemp_{reason}` indicators for "participant was unemployed due to {reason}"
  mutate(
    across(
      starts_with("unemp_"),
      ~case_when(
        # IF participant is employed, THEN set all dummies to 0
        employed %in% 1 ~ 0,

        # OTHERWISE, use raw response
        TRUE ~ dkr_to_na(.x)
      )
    )
  ) %>%

  # Checks
  expect_col_relation(starts_with("unemp_"), employed %in% 1 ~ \(x) x %in% 0) %>%
  expect_col_dummy(starts_with("unemp_")) %>%

  # Select required module and outcome variables
  select(all_of(module_vars), starts_with("unemp_")) %>%

  expect_valid_long() %>%
  display_expectations(c("fail", "caution"))

### Checks  --------------------------------------------------------------------
skim_survey(final_h)

## SECTION I/J #################################################################
# 1. work_hrs_wk
# 2. work_hrs_wk_imp
# 3. has_job[2-4]
# 4. job[1-4]_hours_per_week
# 5. job[1-4]_hrs_per_wk_cnd
# 5. hh_work_hrs
# 6. joint_work_hrs
# 7. joint_work_hrs_cnd

### Create Outcomes ------------------------------------------------------------

# NOTE ES: Per discussion with PK 24/05/24:
# - conditionally winsorize the following outcomes:
#   - work_hrs_wk           (conditional on being employed at all)
#   - work_hrs_wk_imp       (conditional on being employed at all)
#
# - conditionally winsorize inputs to the following outcomes prior to their
#   creation using `create_sum_outcome`:
#   - hh_work_hrs           -> inputs are `work_hrs_wk`, `adult_work_hrs` (conditional on working adult in HH)
#   - joint_work_hrs        -> inputs are `work_hrs_wk`, `partner_work_hrs` (conditional on working partner in HH)
#   - joint_work_hrs_cnd    -> input is `joint_work_hrs`, so no need to double winsorize

# NOTE ES 24/05/27:
# Hours worked at individual jobs (i.e. `job[1-4]_hours_per_week` and
# `job[1-4]_hrs_per_wk_cnd`) are not currently winsorized.

# A flag value to check that `case_when` never reaches an error state
CASE_WHEN_ERROR_FLAG <- -999

# Names for `has_job[n]` and `job[n]_hours_per_week` outcomes
(HAS_JOB_N_COLS <- glue("has_job{1:4}"))
(JOB_N_HOURS_PER_WEEK_COLS <- glue("job{1:4}_hours_per_week"))
(JOB_N_HRS_PER_WK_CND_COLS <- glue("job{1:4}_hrs_per_wk_cnd"))

# Create the outcomes
final_ij <- left_join(
  section_i %>% mutate(module = "I/J"),
  section_j %>% mutate(module = "I/J"),
  by = module_vars
) %>%

  # Join in `employed`, `job_multiple`, `job_n_other` from Section G
  left_join(
    select(section_g, rid, survey_name, employed, job_multiple, job_n_other),
    by = c("rid", "survey_name")
  ) %>%

  #### Participant Work Hours ----
mutate(
  # Set all raw variables to NA if DKR
  across(everything(), dkr_to_na),

  # Initialize columns `HAS_JOB_N_COLS` with all NA values
  !!!rep_named(HAS_JOB_N_COLS, NA_real_),

  # Create `HAS_JOB_N_COLS` outcomes. Whether the participant has a job [n].
  across(
    all_of(HAS_JOB_N_COLS),
    ~case_when(
      # IF this is job 1, THEN set to `employed`
      parse_number(cur_column()) == 1 ~ employed,

      # IF unemployed, THEN set to 0
      employed == 0 ~ 0,

      # IF number of jobs < job number, THEN set to 0
      job_n_other + 1 < parse_number(cur_column()) ~ 0,

      # IF number of jobs >= job number, THEN set to 1
      job_n_other + 1 >= parse_number(cur_column()) ~ 1,

      # OTHERWISE, set to NA
      is_missing(employed) | is_missing(job_n_other) ~ NA_real_,

      # This is a flag, we should never reach this value
      TRUE ~ CASE_WHEN_ERROR_FLAG
    )
  ),

  # Create `JOB_N_HOURS_PER_WEEK_COLS` outcomes. Participant's weekly hours worked at job [n].
  across(
    all_of(JOB_N_HOURS_PER_WEEK_COLS),
    ~{
      # Get the current `job[n]_hours_per_week` and `has_job[n]` columns
      job_n_hours <- .x
      has_job_n <- pick(paste0("has_job", parse_number(cur_column())))[[1]]

      # IF doesn't have job [n], THEN set hours to 0
      job_n_hours[has_job_n %in% 0] <- 0
      job_n_hours
    }
  ),

  # Initialize columns `JOB_N_HRS_PER_WK_CND_COLS` with all NA values
  !!!rep_named(JOB_N_HRS_PER_WK_CND_COLS, NA_real_),

  # Create `JOB_N_HRS_PER_WK_CND_COLS` outcomes. Participant's weekly hours worked
  # at job [n], conditional on having a job [n].
  across(
    all_of(JOB_N_HRS_PER_WK_CND_COLS),
    ~{
      # Get the current `job[n]_hours_per_week` and `has_job[n]` columns
      job_n_hours <- pick(paste0("job", parse_number(cur_column()), "_hours_per_week"))[[1]]
      has_job_n <- pick(paste0("has_job", parse_number(cur_column())))[[1]]

      # IF doesn't have job [n], THEN set hours to NA
      job_n_hours[has_job_n %in% 0] <- NA_real_
      job_n_hours
    }
  ),

  # Participant's total work hours per week at jobs 1-n. IF any of
  # `JOB_N_HOURS_PER_WEEK_COLS` is NA, THEN this outcome is NA also.
  work_hrs_wk =
    pick(survey_name, treat, all_of(JOB_N_HOURS_PER_WEEK_COLS)) %>%
    create_sum_outcome("work_hrs_wk", all_of(JOB_N_HOURS_PER_WEEK_COLS), strict_missing = TRUE) %>%
    pull("work_hrs_wk"),

  # Intermediate indicator for if all of `JOB_N_HOURS_PER_WEEK_COLS` are missing.
  #
  # NOTE: It's important that this occurs after each of the `job[n]_hours_per_week`
  #       columns have been imputed with 0 when the participant does not have job [n].
  all_job_n_hours_missing =
    pick(all_of(JOB_N_HOURS_PER_WEEK_COLS)) %>%
    pmap_int(\(...) is_missing(c(...)) %>% all() %>% as.integer()),

  # Participant's total work hours per week at jobs 1-n. This outcome uses an
  # imputed version of variables `JOB_N_HOURS_PER_WEEK_COLS` (created below).
  work_hrs_wk_imp =
    # Retrieve the columns necessary to create this outcome
    pick(
      survey_name, treat, all_job_n_hours_missing,
      all_of(c(JOB_N_HOURS_PER_WEEK_COLS, HAS_JOB_N_COLS))
    ) %>%

    # Replace missing values of `job[n]_hours_per_week` with the within-survey-treat
    # mean of `job[n]_hours_per_week` for those who HAVE job [n].
    group_by(survey_name, treat) %>%
    mutate(
      across(
        all_of(JOB_N_HOURS_PER_WEEK_COLS),
        ~{
          # Get the current `job[n]_hours_per_week` and `has_job[n]` columns
          job_n_hours <- .x
          has_job_n <- pick(paste0("has_job", parse_number(cur_column())))[[1]]

          # Calculate the within-survey-treat mean of hours worked at job [n],
          # only for those who had a job [n].
          job_n_hours_mean <- mean(job_n_hours[has_job_n %in% 1], na.rm = TRUE)

          # Replace missing values of `job_n_hours` with the mean
          job_n_hours[is_missing(job_n_hours)] <- job_n_hours_mean

          # If all `JOB_N_HOURS_PER_WEEK_COLS` are missing, THEN preserve the missingness
          job_n_hours[all_job_n_hours_missing %in% 1] <- NA_real_
          job_n_hours
        }
      )
    ) %>%
    ungroup() %>%

    # Sum the imputed `job[n]_hours_per_week` columns and retrieve the outcome `work_hrs_wk_imp`
    create_sum_outcome("work_hrs_wk_imp", all_of(JOB_N_HOURS_PER_WEEK_COLS)) %>%
    pull("work_hrs_wk_imp")

) %>%

  #### Household Work Hours ----

# Join in intermediate variables `adult_work_hrs`, `partner_work_hrs`, `adult_in_hh`,
# `partner_in_hh`, `adult_works`, `partner_works` from the Roster.
# NOTE: Preemptively setting these raw variables to NA if DKR.
left_join(
  roster %>%
    select(
      rid, survey_name, adult_work_hrs, partner_work_hrs, adult_in_hh,
      partner_in_hh, adult_works, partner_works
    ) %>%
    mutate(across(everything(), dkr_to_na)),
  by = c("rid", "survey_name")
) %>%

  # Conditionally winsorize within-survey:
  # - the outcomes: `work_hrs_wk`, `work_hrs_wk_imp`
  # - the inputs to: `hh_work_hrs`, `joint_work_hrs`, `joint_work_hrs_cnd`
  #   - namely: `adult_work_hrs` and `partner_work_hrs`
  group_by(survey_name) %>%
  mutate(
    # Winsorize `work_hrs_wk` conditional on being employed (at any job)
    work_hrs_wk = conditional_winsorize(
      x = work_hrs_wk,
      winsor = WINSOR,
      conditional_variable = employed,
      conditional_value = 1
    ),

    # Winsorize `work_hrs_wk_imp` conditional on being employed (at any job)
    work_hrs_wk_imp = conditional_winsorize(
      x = work_hrs_wk_imp,
      winsor = WINSOR,
      conditional_variable = employed,
      conditional_value = 1
    ),

    # Winsorize `adult_work_hrs` conditional on having working adults in the household
    adult_work_hrs = conditional_winsorize(
      x = adult_work_hrs,
      winsor = WINSOR,
      conditional_variable = adult_works,
      conditional_value = 1
    ),

    # Winsorize `partner_work_hrs` conditional on having a working partner in the household
    partner_work_hrs = conditional_winsorize(
      x = partner_work_hrs,
      winsor = WINSOR,
      conditional_variable = partner_works,
      conditional_value = 1
    )
  ) %>%
  ungroup() %>%

  # NOTE ES 2024/06/28: Per EV, in the following two outcomes `hh_work_hrs` and
  # `joint_work_hrs` we:
  # - switched `work_hrs_wk` to `work_hrs_wk_imp`
  # - switched `strict_missing = TRUE` to `strict_missing = FALSE`

  # Total weekly work hours of all adults in the household.
  # NOTE: Inputs `work_hrs_wk` and `adult_work_hrs` are already conditionally winsorized
  create_sum_outcome("hh_work_hrs", work_hrs_wk_imp, adult_work_hrs, strict_missing = FALSE) %>%

  # Total weekly work hours of the participant and their partner.
  # NOTE: Inputs `work_hrs_wk` and `partner_work_hrs` are already conditionally winsorized
  create_sum_outcome("joint_work_hrs", work_hrs_wk_imp, partner_work_hrs, strict_missing = FALSE) %>%

  # Total weekly work hours of the participant and their partner, conditional on having a partner.
  # NOTE: `joint_work_hrs` is already made from conditionally winsorized inputs
  mutate(
    joint_work_hrs_cnd = case_when(
      partner_in_hh %in% 0 ~ NA_real_,
      TRUE ~ joint_work_hrs
    )
  ) %>%

  #### Expectations ----

# Confirm: `HAS_JOB_N_COLS` are dummies
expect_col_lmap_fn(all_of(HAS_JOB_N_COLS), \(x) x %notin% CASE_WHEN_ERROR_FLAG) %>%
  expect_col_dummy(all_of(HAS_JOB_N_COLS)) %>%

  # Confirm: IF doesn't have job [n], THEN `job[n]_hours_per_week` is 0
  expect_col_relation(
    cols = job1_hours_per_week,
    relation = list(is_missing(has_job1) ~ is.na, has_job1 == 0 ~ \(x) x == 0)
  ) %>%
  expect_col_relation(
    cols = job2_hours_per_week,
    relation = list(is_missing(has_job2) ~ is.na, has_job2 == 0 ~ \(x) x == 0)
  ) %>%
  expect_col_relation(
    cols = job3_hours_per_week,
    relation = list(is_missing(has_job3) ~ is.na, has_job3 == 0 ~ \(x) x == 0)
  ) %>%
  expect_col_relation(
    cols = job4_hours_per_week,
    relation = list(is_missing(has_job4) ~ is.na, has_job4 == 0 ~ \(x) x == 0)
  ) %>%

  # Confirm: IF doesn't have job [n], THEN `job[n]_hrs_per_wk_cnd` is NA
  expect_col_relation(
    cols = job1_hrs_per_wk_cnd,
    relation = list(is_missing(has_job1) ~ is.na, has_job1 == 0 ~ is.na)
  ) %>%
  expect_col_relation(
    cols = job2_hrs_per_wk_cnd,
    relation = list(is_missing(has_job2) ~ is.na, has_job2 == 0 ~ is.na)
  ) %>%
  expect_col_relation(
    cols = job3_hrs_per_wk_cnd,
    relation = list(is_missing(has_job3) ~ is.na, has_job3 == 0 ~ is.na)
  ) %>%
  expect_col_relation(
    cols = job4_hrs_per_wk_cnd,
    relation = list(is_missing(has_job4) ~ is.na, has_job4 == 0 ~ is.na)
  ) %>%

  # Confirm: `job[n]_hours_per_week` and `job[n]_hrs_per_wk_cnd` are equivalent if has job [n]
  expect_df_expr(has_job1 %in% 0 | is_same(job1_hours_per_week, job1_hrs_per_wk_cnd)) %>%
  expect_df_expr(has_job2 %in% 0 | is_same(job2_hours_per_week, job2_hrs_per_wk_cnd)) %>%
  expect_df_expr(has_job3 %in% 0 | is_same(job3_hours_per_week, job3_hrs_per_wk_cnd)) %>%
  expect_df_expr(has_job4 %in% 0 | is_same(job4_hours_per_week, job4_hrs_per_wk_cnd)) %>%

  # Confirm: `joint_work_hrs_cnd` is conditional on having a partner
  expect_col_relation(joint_work_hrs_cnd, partner_in_hh %in% 0 ~ is.na) %>%

  # Select required module and outcome variables
  select(
    all_of(module_vars),
    work_hrs_wk,
    work_hrs_wk_imp,
    hh_work_hrs,
    joint_work_hrs,
    joint_work_hrs_cnd,
    all_of(glue("has_job{2:4}")),
    all_of(JOB_N_HOURS_PER_WEEK_COLS),
    all_of(JOB_N_HRS_PER_WK_CND_COLS)
  ) %>%

  # Final checks
  expect_valid_long() %>%
  display_expectations(levels = c("fail", "caution"))

### Checks -----------------------------------------------------------------------
skim_survey(final_ij)

## SECTION AB ##################################################################
# 1. married

### Create outcomes ------------------------------------------------------------

# Create outcomes
final_ab <- section_ab %>%
  # Whether the participant is married
  mutate(married = dkr_to_na(married)) %>%

  # Select module and outcome variables
  select(all_of(module_vars), married) %>%

  # Checks
  expect_col_dummy(married) %>%
  expect_valid_long() %>%
  display_expectations(c("fail", "caution"))

### Checks  --------------------------------------------------------------------
skim_survey(final_ab)

# Confirm: Work status only asked at BL/ML
section_ab %>% surveys_asked(rid, work_ft, work_pt, work_status_raw)
section_ab %>% var_labels(work_ft, work_pt, work_status_raw)

## WORKQUAL ####################################################################
# 1. employed

### Create outcomes ------------------------------------------------------------

final_workqual <- workqual %>%
  # FLAG: Not using SM9 for this outcome
  dplyr::filter(survey_name != "Study Monthly 9") %>%

  # Whether currently employed
  mutate(employed = dkr_to_na(work_for_pay)) %>%

  # Select module vars and outcomes
  select(all_of(module_vars), employed) %>%

  # Checks
  expect_col_dummy("employed") %>%
  expect_valid_long() %>%
  display_expectations(c("fail", "caution"))

### Checks ---------------------------------------------------------------------
skim_survey(final_workqual)

## WORKHRS #####################################################################
# 1. work_hrs_wk_min
# 2. work_hrs_wk_max

### Create outcomes ------------------------------------------------------------

final_workhrs <- workhrs %>%
  mutate(
    # Minimum hours worked weekly
    work_hrs_wk_min = dkr_to_na(workhrs_least),

    # Maximum hours worked weekly
    work_hrs_wk_max = dkr_to_na(workhrs_most)
  ) %>%

  # Select module and outcome variables
  select(all_of(module_vars), work_hrs_wk_min, work_hrs_wk_max) %>%

  # Checks
  expect_col_between("work_hrs_wk_min", left = 0, right = 150) %>%
  expect_col_between("work_hrs_wk_max", left = 0, right = 150) %>%
  expect_valid_long() %>%
  display_expectations(c("fail", "caution"))

### Checks ---------------------------------------------------------------------
skim_survey(final_workhrs)

## INTRATD #####################################################################

# NOTE ES 24/05/15: Map of `Old Name -> Current Name` for `final_intratd` outcomes.
# 1. spouse_in_hh         -> partner_in_hh [1]
# 2. partner_employed     -> partner_works
# 3. partner_work_hrs_wk  -> partner_work_hrs
# 4. is_breadwinner       -> is_breadwinner
#
# [1] The phrasing of `intratd1_living_spouse` and `so1_relationship` is
#     "romantic partner or spouse" and "romantic partner" respectively. This
#     terminology falls under the definition of "partner", hence the name change.

### Resolve Edge Cases ---------------------------------------------------------

# NOTE ES:
# There are two cases where the skip logic between `partner_in_hh` and
# `intratd8_breadwinner` didn't work. `partner_in_hh` was DKR, but `intratd8_breadwinner`
# was still asked when it should have been skipped.
#
# Per discussion with PK 24/05/24:
# If these two RIDs have a partner in the survey according to the SRC Roster,
# then keep their response to `intratd8_breadwinner`. Otherwise, set their response to NA.

#### Case One ----

# FLAG: There is one case where `partner_in_hh` is DKR at Baseline, but participant
#       still responded to `intratd8_breadwinner` (`rid == 541682`)
intratd %>%
  filter(
    str_detect(survey_name, "Baseline"),
    is_missing(intratd1_living_spouse) & !is_missing(intratd8_breadwinner)
  ) %>%
  select(rid, survey_name, intratd1_living_spouse, intratd8_breadwinner)

# NOTE: `rid == 541682` reported no partner at Baseline in SRC Roster
final_roster %>%
  filter(rid == 541682 & survey_name == "SRC Baseline") %>%
  select(rid, survey_name, partner_in_hh)

#### Case Two ----

# FLAG: There is one case where `partner_in_hh` is DKR at ML/EL, but participant
#       still responded to `intratd8_breadwinner` (`rid == 343999`, at Midline)
intratd %>%
  filter(
    !str_detect(survey_name, "Baseline"),
    is_missing(so_3) & !is_missing(intratd8_breadwinner)
  ) %>%
  select(rid, survey_name, so_1, so_3, intratd8_breadwinner)

# NOTE: `rid == 343999` reported no partner at Midline in SRC Roster
final_roster %>%
  filter(rid == 343999 & survey_name == "SRC Midline") %>%
  select(rid, survey_name, partner_in_hh)

#### Resolution ----

# NOTE ES 24/05/27: Since neither RID has a partner according to the roster, setting
#                   their responses to `intratd8_breadwinner` to NA.

intratd[
  intratd$rid == 343999 & intratd$survey_name == "Mobile Midline 2",
  "intratd8_breadwinner"
] <- NA_real_

intratd[
  intratd$rid == 541682 & intratd$survey_name == "Monthly 1 (Mobile Baseline)",
  "intratd8_breadwinner"
] <- NA_real_

### Create outcomes ------------------------------------------------------------

# NOTE: At Monthly 1 (Mobile Baseline) the `intratd` module is skipped if
#       `intratd1_living_spouse != 1`. At MM2 and ME3 `intratd` is skipped if
#       `so1_relationship != 1`.
#
#       We combine these questions in the `partner_in_hh` outcome. Subsequent
#       outcomes' skip logic is based on `partner_in_hh`, which takes care or the
#       BL vs. ML/EL difference in skip logic.

# Confirm: `intratd1_living_spouse` actually refers to living with a partner (i.e.
#           gf, bf, romantic partner, or spouse).
var_label(intratd$intratd1_living_spouse)

surveys_asked(so, so3_live_together)

# Create outcomes
final_intratd <- intratd %>%
  # FLAG ES 24/05/27: Removing post-treatment survey "Q1 2024"
  #
  # TODO ES 24/05/27: Do we want `partner_in_hh` to have a post-treatment version
  # defined only via the `so` post-treatment surveys? The main version of
  # `partner_in_hh` relies on the `roster` (and uses `so` only when it's missing).
  # We currently don't use the PT surveys from `so`.
  filter(survey_name %notin% post_treatment_surveys()) %>%

  # Join in `so1_relationship` and `so3_live_together` from `so`
  left_join(
    so %>% select(rid, survey_name, so1_relationship, so3_live_together),
    by = c("rid", "survey_name")
  ) %>%

  # Re-code "Monthly 1 (Mobile Baseline)" to "Mobile Baseline 1"
  mutate(
    survey_name = case_match(
      survey_name,
      "Monthly 1 (Mobile Baseline)" ~ "Mobile Baseline 1",
      .default = survey_name
    )
  ) %>%

  # Create outcomes
  mutate(
    # Set all raw variables to NA if DKR
    across(everything(), dkr_to_na),

    # `intratd4_partner_hours_work` was not capped at Baseline. Setting this variable
    # to the manually capped version at Baseline.
    intratd4_partner_hours_work = case_when(
      survey_name == "Mobile Baseline 1" ~ intratd4_partner_hr_work_cap,
      TRUE ~ intratd4_partner_hours_work
    ),

    # Whether the participant lives with a spouse or romantic partner
    partner_in_hh = case_when(
      # At Baseline, this is asked in `intratd1_living_spouse`
      survey_name == "Mobile Baseline 1" ~ dkr_to_na(intratd1_living_spouse),

      # At ML/EL, this is asked via `so1_relationship` and `so3_live_together`
      TRUE ~ case_when(
        # IF not in a relationship, THEN 0
        so1_relationship == 0 ~ 0,

        # IF in a relationship, THEN use `so3_live_together` (whether participant
        # lives with their partner)
        so1_relationship == 1 ~ dkr_to_na(so3_live_together),
        TRUE ~ NA_real_
      )
    ),

    # Number of hours the participant's in-household romantic partner works per week
    partner_work_hrs = case_when(
      # IF no in-household partner, THEN set to 0 (unconditional)
      partner_in_hh == 0 ~ 0,

      # OTHERWISE, use reported work hours
      TRUE ~ dkr_to_na(intratd4_partner_hours_work)
    ),

    # Whether the participant's in-household romantic partner works
    partner_works = case_when(
      # IF no in-household partner, THEN set to 0 (unconditional)
      partner_in_hh == 0 ~ 0,

      # IF partner work hours is unknown, THEN set to missing
      is_missing(partner_work_hrs) ~ NA_real_,

      # OTHERWISE, check if partner works > 0 hours per week
      TRUE ~ as.numeric(partner_work_hrs > 0)
    ),

    # Whether the participant reported being the primary breadwinner
    is_breadwinner = case_when(
      # IF no in-household romantic partner, THEN set to 1 (infer participant is breadwinner)
      partner_in_hh == 0 ~ 1,

      # IF participant is breadwinner, THEN set to 1
      intratd8_breadwinner == 2 ~ 1,

      # IF the partner, someone else, or both the partner and the participant are
      # breadwinners, THEN set to 0
      intratd8_breadwinner %in% c(0, 1, 5) ~ 0,

      # OTHERWISE, set to missing
      TRUE ~ NA_real_
    )
  ) %>%

  # Confirm: IF no partner in household, THEN work hours and work status are 0
  expect_col_relation(
    cols = c(partner_work_hrs, partner_works),
    relation = list(
      is_missing(partner_in_hh) ~ is.na,
      partner_in_hh == 0 ~ \(x) x == 0
    )
  ) %>%

  # Confirm: IF no partner in household, THEN participant is considered breadwinner
  expect_col_relation(is_breadwinner, partner_in_hh %in% 0 ~ \(x) x %in% 1) %>%

  # Check: Double check that `partner_in_hh` is constructed as expected
  expect_col_relation(
    cols = partner_in_hh,
    relation = list(
      # Baseline Case
      survey_name %in% "Mobile Baseline 1" & is_missing(intratd1_living_spouse) ~ is.na,  # Unknown relationship status
      survey_name %in% "Mobile Baseline 1" & intratd1_living_spouse %in% 1 ~ \(x) x == 1, # Lives with partner
      survey_name %in% "Mobile Baseline 1" & intratd1_living_spouse %in% 0 ~ \(x) x == 0, # Does not live with partner
      # Non-Baseline Case
      !str_detect(survey_name, "Base") & is_missing(so1_relationship) ~ is.na,            # Unknown relationship status
      !str_detect(survey_name, "Base") & so1_relationship == 0 ~ \(x) x == 0,             # No partner
      !str_detect(survey_name, "Base") & is_missing(so3_live_together) ~ is.na,           # Unknown living status
      !str_detect(survey_name, "Base") & !is_missing(so1_relationship) & so3_live_together %in% 1 ~ \(x) x == 1, # Lives with partner
      !str_detect(survey_name, "Base") & !is_missing(so1_relationship) & so3_live_together %in% 0 ~ \(x) x == 0  # Does not live with partner
    )
  ) %>%

  # Select required module and outcome variables
  select(
    all_of(module_vars),
    partner_in_hh, partner_work_hrs, partner_works, is_breadwinner
  ) %>%

  # Final Checks
  expect_valid_long() %>%
  display_expectations(levels = c("fail", "caution"))
# display_expectations()

### Checks ---------------------------------------------------------------------
skim_survey(final_intratd)

## SO ##########################################################################
# 1. partner_in_hh

# NOTE ES: Per discussion with PK 24/05/24: Deleting `final_so` entirely. See below
#          for reasoning.
#
# NOTE ES 24/05/17: This is redundant. `partner_in_hh` is already created for
# Baseline, Midline, and Endline in `final_intratd`. This just adds `partner_in_hh_sy1`,
# using Study Monthly 5. Which of the versions below do we want to use?
#
# <final_intratd>
# `partner_in_hh_bl`  -> `intratd1_living_spouse` (MB1)
# `partner_in_hh_ml`  -> `so1_relationship/so3_live_together` (MM2)
# `parnter_in_hh_el`  -> `so1_relationship/so3_live_together` (ME3)
#
# <final_so>
# `partner_in_hh_sy1` -> `so1_relationship/so3_live_together` (SM5)
# `partner_in_hh_bl`  -> `so1_relationship/so3_live_together` (MB3)
# `partner_in_hh_ml`  -> `so1_relationship/so3_live_together` (MM2)
# `parnter_in_hh_el`  -> `so1_relationship/so3_live_together` (ME3)

## LOCATION ####################################################################
# 1. in_bl_texas

# Get the `texas_bl` dummy from shared data
final_location <-
  read_dta("~/research_shared/03_pap_analyses/02_across_topic_data/baseline_covars_4_lasso.dta") %>%
  select(rid, in_bl_texas = texas_bl) %>%

  # Add module variables (using those from Section AB).
  # NOTE: Intentionally joining on only `rid` (not `by = c("rid", "survey_name")`).
  left_join(
    section_ab %>% select(all_of(module_vars)),
    by = "rid"
  ) %>%

  # Checks
  expect_col_dummy(in_bl_texas) %>%
  expect_valid_long() %>%
  display_expectations(c("fail", "caution"))

#### Checks --------------------------------------------------------------------
skim_survey(final_location)

## SECTION H/K #################################################################

# NOTE ES 24/05/16: This was just added to the script, so I'm going to leave these
# variables as is for now and just update the old variables.

# ER added on 5/14/24 for baseline covars and post-PAP analyses
# 1. emp_n_mnths_uncond
# 2. jobs_last_1yr_uncd
# 3. jobs_last_2yr_uncd
# 4. jobs_last_3yr_uncd

### Create Outcomes ------------------------------------------------------------

final_hk <- section_k %>%
  # Join skip logic variables `job_n_other`, `employed` from Section G
  left_join(
    section_g %>% select(rid, survey_name, employed, job_n_other),
    by = c("rid", "survey_name")
  ) %>%

  # Join `unemploy_length`, `work_ever` from Section H
  left_join(
    section_h %>% select(rid, survey_name, unemploy_length, work_ever),
    by = c("rid", "survey_name")
  ) %>%

  # Create outcomes
  mutate(
    # Update the module name
    module = "H/K",

    # Set all raw outcome to NA if DKR
    across(everything(), dkr_to_na),

    # How many months the respondent has been employed in the past year
    emp_n_mnths_uncond = case_when(
      !is.na(employed_n_months_last_yr) ~ employed_n_months_last_yr,
      work_ever == 0 ~ 0,
      unemploy_length >= 3 ~ 0,
      TRUE ~ NA_real_
    ),

    # Number of jobs in the past year
    jobs_last_1yr_uncd = case_when(
      # IF unemployed, THEN use past_jobs_n_last_yr if not missing
      employed == 0 & !is.na(past_jobs_n_last_yr) ~ past_jobs_n_last_yr,

      # IF employed, add main job + number of other jobs + past_jobs_n_last_yr
      employed == 1 ~ 1 + job_n_other + past_jobs_n_last_yr,

      # IF never worked or unemployed longer than a year, set to 0
      work_ever == 0 ~ 0,

      # Unemployed for longer than a year
      unemploy_length >= 3 ~ 0,
      TRUE ~ NA_real_
    ),

    # Number of jobs in the past 2 years (midline and endline only)
    jobs_last_2yr_uncd = case_when(
      !is.na(total_jobs_n_last_2yr) ~ total_jobs_n_last_2yr,
      work_ever == 0 ~ 0,
      survey_name != "SRC Baseline" & unemploy_length >= 4 ~ 0,
      TRUE ~ NA_real_
    ),

    # Number of jobs in the past 3 years (baseline only)
    jobs_last_3yr_uncd = case_when(
      !is.na(total_jobs_n_last_3yr) ~ total_jobs_n_last_3yr,
      work_ever == 0 ~ 0,
      survey_name == "SRC Baseline" & unemploy_length == 5 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%

  # Fix cases where number of jobs in the past year = 0 but they were employed during the year
  mutate(
    # Only comes up if currently unemployed
    jobs_last_1yr_uncd = case_when(
      (jobs_last_1yr_uncd == 0 & emp_n_mnths_uncond > 0 & employed == 0) ~ 1,
      TRUE ~ jobs_last_1yr_uncd
    )
  ) %>%

  # Select required outcome and module variables
  select(
    all_of(module_vars),
    emp_n_mnths_uncond,
    jobs_last_1yr_uncd,
    jobs_last_2yr_uncd,
    jobs_last_3yr_uncd
  ) %>%

  # Checks
  # NOTE ES 24/05/22: Can ignore `jobs_last_3yr_uncd` missing at EL and `jobs_last_2yr_uncd`
  #                   missing at BL, ML. These questions aren't asked in those
  #                   periods and are removed after aggregation.
  expect_valid_long() %>%
  display_expectations(levels = c("fail", "caution"))
# display_expectations()

# Fixing the answer which was outside of SRC bounds
final_hk <- final_hk %>%
  mutate(
    jobs_last_1yr_uncd = case_when(
      jobs_last_1yr_uncd > 50 ~ 50,
      # OTHERWISE, use number other jobs
      TRUE ~ dkr_to_na(jobs_last_1yr_uncd)
    )
  )

################################################################################
# 3. Create Master long  (RID x Survey) panel dataset for family ---------------

# List of module/section outcome dataframes
all_long_surveys <- list(
  final_roster,
  final_g,
  final_m,
  final_h,
  final_ij,
  final_ab,
  final_workqual,
  final_workhrs,
  final_intratd,
  final_location,
  final_hk
)

# Long output dataframe
output_data_long <- bind_rows(all_long_surveys)

# Checks -----------------------------------------------------------------------

# Verify which new outcomes were created
outcome_var_names <-
  setdiff(
    names(output_data_long),
    unique(unlist(lapply(list(section_ab, section_g, section_i, section_j, section_m, workqual, workhrs, intratd, so), function(x) names(x))))
  )

# Print outcomes for labeling
paste0('"', outcome_var_names, '" = ""') %>% cat(sep = ",\n")

### Variable Labels ------------------------------------------------------------

# Label Map
outcome_label_map <- list(
  # Module variables
  "rid" = "Research ID",
  "treat" = "Treatment indicator",
  "survey_name" = "Survey name",
  "module" = "Survey module (Qualtrics) or sections (SRC)",
  "source" = "Survey source",

  # Outcome variables
  "work_hrs_wk_min" = "Minimum number of hours worked in a typical week",
  "work_hrs_wk_max" = "Maximum number of hours worked in a typical week",
  "employed" = "Whether the respondent is employed",
  "inc_ind_tot_rep" = "Total individual income before taxes over the last 12 months",
  "inc_hh_total_rep_win" = "Total household income before taxes over the last 12 months",
  "inc_ind_transfers" = "Total individual income from government transfers over the last 12 months",
  "inc_ind_gifts" = "Total individual income  from gifts over the last 12 months",
  "inc_ind_passive" = "Total passive individual income before taxes over the last 12 months",
  "inc_spouse" = "Total income of spouse before taxes over the last 12 months",
  "unemp_unavail" = "Not working due to suitable work being unavailable",
  "unemp_lack_skill" = "Not working due to lack in necessary skills",
  "unemp_transport" = "Not working due to lack in transportation to/from work",
  "unemp_ill" = "Not working due to illness",
  "unemp_childcare" = "Not working due to inability to find child care",
  "unemp_elderly" = "Not working due to caring for elderly",
  "unemp_edu" = "Not working due to attending school",
  "unemp_prefer_sah" = "Not working due to preferring to stay at home",
  "unemp_pers_resp" = "Not working due to personal or family responsibilities",
  "unemp_gave_up" = "Not working due to have given up looking for work",
  "unemp_other" = "Not working due to other reasons",
  "work_hrs_wk" = "Number of hours worked in a typical week",
  "work_hrs_wk_imp" = "Number of hours worked in a typical week (missing values imputed).",
  "spouse_in_hh" = "Whether participant lives with a romantic partner",
  "partner_works" = "Respondent’s romantic partner is currently employed",
  "partner_work_hrs" = "Number of hours spouse/partner works per week",
  "is_breadwinner" = "Whether participant reported being the primary breadwinner",
  "has_job2" = "Whether the participant has a second job",
  "has_job3" = "Whether the participant has a third job",
  "has_job4" = "Whether the participant has a fourth job",
  # ES 24/05/21: Replaced `work_hrs_wk_main` with equivalent `job1_hours_per_week`
  "job1_hours_per_week" = "Number of hours worked in a typical week at main job.",
  "job2_hours_per_week" = "Number of hours worked in a typical week of the second job",
  "job3_hours_per_week" = "Number of hours worked in a typical week of the third job",
  "job4_hours_per_week" = "Number of hours worked in a typical week of the fourth job",

  # Roster Outcomes
  "spouse_in_hh" = "Spouse lives in household",
  "partner_gf_bf_in_hh" = "Partner, Girlfriend, or Boyfriend lives in household",
  "partner_in_hh" = "Spouse, Partner, Girlfriend, or Boyfriend lives in household",
  "eitc_child_in_hh" = "Child eligible for being an EITC dependant lives in household",
  "eitc_child_hh_n" = "Number of children eligible for being an EITC dependant living in household",
  "head_of_hh" = "Whether participant is a single head of household with dependants",
  "married" = "Whether participant is married",
  "adult_hh_n" = "Number of other adults in household",
  "hh_size_n" = "Size of household (including participant)",
  "dual_inc" = "Whether the participant lives in a dual-income household",
  "dual_bl_inc" = "Whether the participant lived in a dual-income household at Baseline",
  # ES 24/03/20: Adding post-PAP outcomes
  "partner_work_hrs_cd" = "Number of hours spouse/partner works per week (conditional on having a spouse/partner)",
  "adult_works_n" = "Number of other household members which are employed",
  "adult_works_n_cnd" = "Number of other household members which are employed (conditional on having a spouse/partner)",
  "joint_work_hrs" = "Total number of hours participant and spouse/partner works per week",
  "joint_work_hrs_cnd" = "Total number of hours participant and spouse/partner works per week (conditional on having a spouse/partner)",
  # ES 24/03/28: Adding post-PAP outcomes
  "non_rom_works" = "Whether any other adult household members (excluding the participant's partner) work",
  "non_rom_work_hrs" = "Total number of hours other adult household members (excluding the participant's partner) work per week",
  "hh_work_hrs" = "Total number of hours all household members (including the participant) work per week",
  "parent_works" = "Whether any of the participant's parents in household work",
  "parent_work_hrs" = "Total number of hours participant's parents in household work per week",
  "adult_kid_works" = "Whether any of the participant's adult children in household work",
  "adult_kid_work_hrs" = "Total number of hours participant's adult children in household work per week",

  # Location Outcomes
  "in_bl_texas" = "Whether the participant was in Texas at Baseline.",

  # ER added on 5/14/24 for baseline covars and post-PAP analyses
  "emp_n_mnths_uncond" = "Number of months employed in the past year (not conditional on employment)",
  "jobs_last_1yr_uncd" = "Number of jobs held in the past year (not conditional on employment)",
  # FLAG: only collected at midline and endline
  "jobs_last_2yr_uncd" = "Total number of jobs held in the past two years (not conditional on employment) ML/EL only",
  # FLAG: only collected at baseline
  "jobs_last_3yr_uncd" = "Total number of jobs help in the past three years (not conditional on employment) BL only"
)

# Set variable labels
output_data_long <- output_data_long %>%
  set_variable_labels(.labels = outcome_label_map, .strict = FALSE)

## Check for any missing variable labels
long_missing_var_labels <-
  as_tibble(look_for(output_data_long)) %>%
  dplyr::filter(is.na(label))

nrow(long_missing_var_labels) == 0

### Value Labels ---------------------------------------------------------------

## Check for any missing value labels
long_missing_value_labels <-
  as_tibble(look_for(output_data_long)) %>%
  dplyr::filter(sapply(value_labels, is.null))

# NOTE: Non-Dummies / Factors are left
nrow(long_missing_value_labels) == 0

## Set missing value labels
output_data_long <- output_data_long %>%
  add_dummy_val_labels(
    partner_works, is_breadwinner, spouse_in_hh, partner_in_hh, partner_gf_bf_in_hh,
    dual_inc, dual_bl_inc, eitc_child_in_hh, eitc_child_in_hh, head_of_hh,
    non_rom_works, parent_works, adult_kid_works
  )

### Final Labeling Check -------------------------------------------------------

# Review this table for any missing/incorrect variable labels or value labels
all_long_labels <- look_for(output_data_long)

################################################################################
# 4. Aggregation & Transformation to final wide Analysis Dataset ---------------

# AGGREGATION STEPS ------------------------------------------------------------
# 1. Rename long variables to match the final outcome variable name (where required)
# 2. Define aggregation functions not already defined in analysis_dataset_creation_functions
# 3. Create aggregation_map
# 4. Use the aggregate_all function

# NOTES:
# - for (1), this is mostly a problem with _6mo, _12mo suffixes, doesn't appear in all PAP's
# - for (2), add common functions to analysis_dataset_creation_functions for standardization

## 1. Rename for outcome preparation -------------------------------------------

# List of final surveys
all_final_surveys <- list(
  final_roster,
  final_g,
  final_m,
  final_h,
  final_ij,
  final_ab,
  final_workqual,
  final_workhrs,
  final_intratd,
  final_location,
  final_hk
)

# FLAG ES 24/02/28: Removing post-treatment monthly surveys, since they cause `aggregate_all` error
all_final_surveys <- all_final_surveys %>%
  map(
    \(x) {
      x %>%
        filter(
          survey_name %notin% paste0("Study Monthly ", 37:50),
          survey_name %notin% paste0("Q", 1:4, " 2024")
        )
    }
  )

# NOTE: ES 24/05/08: `employed` appears in `G` and `workqual`
all_final_surveys %>% keep(\(x) "employed" %in% names(x))

# NOTE: `workqual` is asked MB2, MM2, ME2 as expected
surveys_asked(final_workqual)

## 2. Define Aggregation Functions ---------------------------------------------

## 3. Fill in Aggregation Map --------------------------------------------------

# Create aggregation map
aggregation_map <-
  tribble(
    ~outcome, ~agg_function, ~qualtrics_baseline, ~merge_function,

    ### Income and Employment Outcomes ----
    "employed", mean_nm, list(), non_missing_src,
    "work_hrs_wk_min", mean_nm, list(), NULL,
    "work_hrs_wk_max", mean_nm, list(), NULL,
    "inc_ind_tot_rep",  mean_nm, list(), NULL,
    "inc_hh_total_rep_win",  mean_nm, list(), NULL,
    "inc_ind_transfers",  mean_nm, list(), NULL,
    "inc_ind_gifts",  mean_nm, list(), NULL,
    "inc_ind_passive",  mean_nm, list(), NULL,
    "inc_spouse",  mean_nm, list(), NULL,
    "work_hrs_wk",  mean_nm, list(), NULL,
    "work_hrs_wk_imp", mean_nm, list(), NULL,
    "unemp_unavail", mean_nm, list(), NULL,
    "unemp_lack_skill", mean_nm, list(), NULL,
    "unemp_transport", mean_nm, list(), NULL,
    "unemp_ill", mean_nm, list(), NULL,
    "unemp_childcare", mean_nm, list(), NULL,
    "unemp_elderly", mean_nm, list(), NULL,
    "unemp_edu", mean_nm, list(), NULL,
    "unemp_prefer_sah", mean_nm, list(), NULL,
    "unemp_pers_resp", mean_nm, list(), NULL,
    "unemp_gave_up", mean_nm, list(), NULL,
    "unemp_other", mean_nm, list(), NULL,
    # ES 24/05/14: Adding post-PAP outcomes `has_job[2-4]`, `job[1-4]_hours_per_week`
    "has_job2", mean_nm, list(), NULL,
    "has_job3", mean_nm, list(), NULL,
    "has_job4", mean_nm, list(), NULL,
    "job1_hours_per_week", mean_nm, list(), NULL,
    "job2_hours_per_week", mean_nm, list(), NULL,
    "job3_hours_per_week", mean_nm, list(), NULL,
    "job4_hours_per_week", mean_nm, list(), NULL,
    # ES 24/05/14: Adding post-PAP outcomes `job[1-4]_hrs_per_wk_cnd`, which are
    # conditional versions of `job[1-4]_hours_per_week` (conditional on having ith job)
    "job1_hrs_per_wk_cnd", mean_nm, list(), NULL,
    "job2_hrs_per_wk_cnd", mean_nm, list(), NULL,
    "job3_hrs_per_wk_cnd", mean_nm, list(), NULL,
    "job4_hrs_per_wk_cnd", mean_nm, list(), NULL,
    # Past employment variables
    "emp_n_mnths_uncond", mean_nm, list(), NULL,
    "jobs_last_1yr_uncd", mean_nm, list(), NULL,
    "jobs_last_2yr_uncd", mean_nm, list(), NULL,
    "jobs_last_3yr_uncd", mean_nm, list(), NULL,

    ### Roster Outcomes ----
    "spouse_in_hh", mean_nm, list(), NULL,
    "partner_gf_bf_in_hh", mean_nm, list(), NULL,
    "eitc_child_in_hh", mean_nm, list(), NULL,
    "eitc_child_hh_n", mean_nm, list(), NULL,
    "head_of_hh", mean_nm, list(), NULL,
    "married", mean_nm, list(), NULL,
    "adult_hh_n", mean_nm, list(), NULL,
    "hh_size_n", mean_nm, list(), NULL,
    "dual_inc", mean_nm, list(), NULL,
    "dual_bl_inc", mean_nm, list(), NULL,
    "partner_in_hh", mean_nm, list(), non_missing_src,
    "partner_works", mean_nm, list(), non_missing_src,
    "partner_work_hrs", mean_nm, list(), non_missing_src,
    "is_breadwinner", mean_nm, list(), NULL,
    # ES 24/03/20: Adding post-PAP outcomes
    "partner_work_hrs_cd", mean_nm, list(), NULL,
    "adult_works_n", mean_nm, list(), NULL,
    "adult_works_n_cnd", mean_nm, list(), NULL,
    "joint_work_hrs", mean_nm, list(), NULL,
    "joint_work_hrs_cnd", mean_nm, list(), NULL,
    "hh_work_hrs", mean_nm, list(), NULL,
    "non_rom_works", mean_nm, list(), NULL,
    "non_rom_work_hrs", mean_nm, list(), NULL,
    "parent_works", mean_nm, list(), NULL,
    "parent_work_hrs", mean_nm, list(), NULL,
    "adult_kid_works", mean_nm, list(), NULL,
    "adult_kid_work_hrs", mean_nm, list(), NULL,

    # Location Outcomes
    "in_bl_texas", mean_nm, list(), NULL
  )

## 4. Aggregate ----------------------------------------------------------------

# Run a test aggregation
test_final_surveys <- lapply(all_final_surveys, function(x) dplyr::filter(x, rid < 120000))
test_agg <- aggregate_all(aggregation_map, test_final_surveys)
skim(test_agg, -matches("(_ms|_n_nm)$"))
skim(test_agg, matches("_n_nm$")) %>% arrange(desc(numeric.p100))

# Check if any names exceed 22 characters prior to full aggregation
target_names <- names(test_agg)[!grepl("_(ms|n_nm)$", names(test_agg))]
target_names[nchar(target_names) > 22]

# Aggregate
output_data_wide <- aggregate_all(aggregation_map, all_final_surveys)

# FLAG ER 5/14/24: Removing vars that are only asked during specific time periods
output_data_wide <- output_data_wide %>%
  select(
    -contains("jobs_last_2yr_uncd_bl"),
    -contains("jobs_last_3yr_uncd_ml"),
    -contains("jobs_last_3yr_uncd_el")
  )

## Checks ----------------------------------------------------------------------

# Skim
skim(output_data_wide %>% select(-matches("_(ms|n_nm)$")))
skim(output_data_wide %>% select(ends_with("_ms")))

# Confirm 100% completion rate
all(!is_missing(output_data_wide))

# Add Labels ------------------------------------------------------------------

# Generate the wide label map
wide_labels <- generate_wide_labels(output_data_wide, output_data_long)

# Set the wide labels
output_data_wide <- set_wide_labels(wide_data = output_data_wide, wide_labels = wide_labels)

### Check for missing variable/value labels
wide_labels_check <- as_tibble(look_for(output_data_wide))

# Confirm: No missing variable labels
wide_labels_check[is.na(wide_labels_check$label), ]

# Confirm: No missing dummy/factor value labels (excluding _n_nm variables)
wide_labels_check[sapply(wide_labels_check$value_labels, is.null) & !grepl("_n_nm$", wide_labels_check$variable), ]

################################################################################################################################################################
# 5. Variable Map Checks & Save Output -----------------------------------------

## Define Partial Variable Map -------------------------------------------------

# Print a template `partial_map` from the existing map (only works if variable map already exists)
partial_map_from_variable_map(
  variable_map_path = "employment/02_data/employment_variable_map.xlsx",
  family_name_stata = "labor_elasticity"
)

# Fill in the partial variable map
partial_map <- tribble(
  ~"item_stata", ~"item_human", ~"component_stata", ~"component_human", ~"priority_numeric", ~"reverse", ~"exclude",

  ### Ordered Outcomes ----

  # NOTES!! please preserve the order of the items for the first block of code!!!!!!!!!*****
  "employed", "Whether the respondent is employed", "labor_elasticity", "Labor Supply Elasticity", 1, 0, 0,
  "has_job2", "Whether the respondent has a second job", "labor_elasticity", "Labor Supply Elasticity", 99, 0, 0,
  "has_job3", "Whether the respondent has a third job", "labor_elasticity", "Labor Supply Elasticity", 99, 0, 0,
  "has_job4", "Whether the respondent has a fourth job", "labor_elasticity", "Labor Supply Elasticity", 99, 0, 0,
  "work_hrs_wk_imp", "Hours worked per week", "labor_elasticity", "Labor Supply Elasticity", 1, 0, 0, #PK 6/27/24 changed missing imputed version of variable to primary
  "job1_hours_per_week", "Hours per week worked at 1st job", "labor_elasticity", "Labor Supply Elasticity", 99, 0, 0,
  "job2_hours_per_week", "Hours per week worked at 2nd job", "labor_elasticity", "Labor Supply Elasticity", 99, 0, 0,
  "job3_hours_per_week", "Hours per week worked at 3rd job", "labor_elasticity", "Labor Supply Elasticity", 99, 0, 0,
  "job4_hours_per_week", "Hours per week worked at 4th job", "labor_elasticity", "Labor Supply Elasticity", 99, 0, 0,
  "job1_hrs_per_wk_cnd", "Hours per week worked at 1st job (conditional on having 1st job)", "labor_elasticity", "Labor Supply Elasticity", 99, 0, 0,
  "job2_hrs_per_wk_cnd", "Hours per week worked at 2nd job (conditional on having 2nd job)", "labor_elasticity", "Labor Supply Elasticity", 99, 0, 0,
  "job3_hrs_per_wk_cnd", "Hours per week worked at 3rd job (conditional on having 3rd job)", "labor_elasticity", "Labor Supply Elasticity", 99, 0, 0,
  "job4_hrs_per_wk_cnd", "Hours per week worked at 4th job (conditional on having 4th job)", "labor_elasticity", "Labor Supply Elasticity", 99, 0, 0,
  "adult_works_n", "Number of other household members which are employed", "labor_elasticity", "Labor Supply Elasticity", 99, 0, 0,
  "joint_work_hrs", "Total number of hours participant and spouse/partner works per week", "labor_elasticity", "Labor Supply Elasticity", 99, 0, 0,
  "hh_work_hrs", "Total number of hours all household members (including the participant) work per week", "labor_elasticity", "Labor Supply Elasticity", 99, 0, 0,
  "parent_work_hrs", "Total number of hours participant's parents in household work per week", "labor_elasticity", "Labor Supply Elasticity", 99, 0, 0,
  "adult_kid_work_hrs", "Total number of hours participant's adult children in household work per week", "labor_elasticity", "Labor Supply Elasticity", 99, 0, 0,
  # NOTES: until here!!!!!!

  ### Un-ordered Outcomes ----

  "work_hrs_wk_max", "Maximum number of hours worked in a typical week", "labor_elasticity", "Labor Supply Elasticity", 2, 0, 0,
  "work_hrs_wk_min", "Minimum number of hours worked in a typical week", "labor_elasticity", "Labor Supply Elasticity", 2, 0, 0,
  "unemp_childcare", "Not working due to inability to find child care", "labor_elasticity", "Labor Supply Elasticity", 2, 0, 0,
  "unemp_edu", "Not working due to attending school", "labor_elasticity", "Labor Supply Elasticity", 2, 0, 0,
  "unemp_elderly", "Not working due to caring for elderly", "labor_elasticity", "Labor Supply Elasticity", 2, 0, 0,
  "unemp_gave_up", "Not working due to have given up looking for work", "labor_elasticity", "Labor Supply Elasticity", 2, 0, 0,
  "unemp_ill", "Not working due to illness", "labor_elasticity", "Labor Supply Elasticity", 2, 0, 0,
  "unemp_lack_skill", "Not working due to lack in necessary skills", "labor_elasticity", "Labor Supply Elasticity", 2, 0, 0,
  "unemp_other", "Not working due to other reasons", "labor_elasticity", "Labor Supply Elasticity", 2, 0, 0,
  "unemp_pers_resp", "Not working due to personal or family responsibilities", "labor_elasticity", "Labor Supply Elasticity", 2, 0, 0,
  "unemp_prefer_sah", "Not working due to preferring to stay at home", "labor_elasticity", "Labor Supply Elasticity", 2, 0, 0,
  "unemp_transport", "Not working due to lack in transportation to/from work", "labor_elasticity", "Labor Supply Elasticity", 2, 0, 0,
  "unemp_unavail", "Not working due to suitable work being unavailable", "labor_elasticity", "Labor Supply Elasticity", 2, 0, 0,

  ### Excluded ----
  # NOTE ES 2024/06/28: Per EV, excluding `work_hrs_wk`, setting `work_hrs_wk_imp` as a primary
  #PK 6/27/24 changed strict missing version of variable to secondary
  "work_hrs_wk", "Hours worked per week (no imputation of missing values)", "labor_elasticity", "Labor Supply Elasticity", 2, 0, 1,
  "spouse_in_hh", "Whether Spouse Lives in House", "roster", "Roster", 2, 0, 1,
  "partner_gf_bf_in_hh", "Partner, Girlfriend, or Boyfriend lives in household", "roster", "Roster", 2, 0, 1,
  "partner_in_hh", "Spouse, Partner, Girlfriend, or Boyfriend lives in household", "roster", "Roster", 2, 0, 1,
  "eitc_child_hh_n", "Number of children eligible for being an EITC dependant lives in household", "roster", "Roster", 2, 0, 1,
  "eitc_child_in_hh", "Child eligible for being an EITC dependant lives in household", "roster", "Roster", 2, 0, 1,
  "head_of_hh", "Whether participant is a single head of household with dependants", "roster", "Roster", 2, 0, 1,
  "married", "Whether respondent is married", "roster", "Roster", 2, 0, 1,
  "adult_hh_n", "Number of other adults in household", "roster", "Roster", 2, 0, 1,
  "hh_size_n", "Size of household", "roster", "Roster", 2, 0, 1,
  "dual_inc", "Whether respondent lives in a dual income household", "roster", "Roster", 2, 0, 1,
  "dual_bl_inc", "Whether respondent lived in a dual income household at baseline", "roster", "Roster", 2, 0, 1,
  "in_bl_texas", "Whether Respondent Lives in TX at Baseline", "roster", "Roster", 2, 0, 1,
  "inc_hh_total_rep_win", "Household Income Total", "roster", "Roster", 2, 0, 1,
  "inc_ind_gifts", "Individual Income in Gifts", "roster", "Roster", 2, 0, 1,
  "inc_spouse", "Total income of spouse before taxes over the last 12 months", "roster", "Roster", 2, 0, 1,
  "inc_ind_transfers", "Individual Income in Transfers", "roster", "Roster", 2, 0, 1,
  "inc_ind_passive", "Individual Income in Passive Income", "roster", "Roster", 2, 0, 1,
  "inc_ind_tot_rep", "Reported Individual Income", "roster", "Roster", 2, 0, 1,
  "is_breadwinner", "Whether respondent is the breadwinner", "roster", "Roster", 2, 0, 1,
  "partner_works", "Whether respondent’s partner is employed", "roster", "Roster", 2, 0, 1,
  "partner_work_hrs", "Hours worked weekly by respondent’s partner", "roster", "Roster", 2, 0, 1,
  # ES 24/03/20:
  # - Adding post-PAP outcomes `part_wkr_hrs_cnd`, `adult_works_n`, `adult_works_n_cnd`
  # - Adding post-PAP outcomes `joint_work_hrs`, `joint_work_hrs_cnd`
  "partner_work_hrs_cd", "Number of hours spouse/partner works per week (conditional on having a partner/spouse)", "roster", "Roster", 99, 0, 1,
  "adult_works_n_cnd", "Number of other household members which are employed (conditional on having a partner/spouse)", "roster", "Roster", 99, 0, 1,
  "joint_work_hrs_cnd", "Total number of hours participant and spouse/partner works per week (conditional on having a spouse/partner)", "roster", "Roster", 99, 0, 1,
  # ER adding past employment variables, excluding from variable map (5/14/24)
  "emp_n_mnths_uncond", "Number of months worked in the past year (not conditioned on employment)", "past_employ", "Past Employment", 2, 0, 1,
  "jobs_last_1yr_uncd", "Number of jobs held in the past year (not conditioned on employment)", "past_employ", "Past Employment", 2, 0, 1,
  "jobs_last_2yr_uncd", "Number of jobs held in the past two years (not conditioned on employment)", "past_employ", "Past Employment", 2, 0, 1,
  "jobs_last_3yr_uncd", "Number of jobs held in the past three years (not conditioned on employment)", "past_employ", "Past Employment", 2, 0, 1,
  # NOTE ES 2024/06/28: Added other items which appear in the wide dataset
  "non_rom_works", "Whether any other adult household members (excluding the participant's partner) work", "roster", "Roster", 2, 0, 1,
  "non_rom_work_hrs", "Total number of hours other adult household members (excluding the participant's partner) work per week", "roster", "Roster", 2, 0, 1,
  "parent_works", "Whether any of the participant's parents in household work", "roster", "Roster", 2, 0, 1,
  "adult_kid_works", "Whether any of the participant's adult children in household work", "roster", "Roster", 2, 0, 1
)

# Save the WIDE dataset --------------------------------------------------------

save_analysis_wide(
  wide_output = output_data_wide,
  pap = "employment",
  partial_variable_map = partial_map,
  family_name_stata = "labor_elasticity",
  family_name_human = "Labor Supply Elasticity",
  rearrange_items = 0 # if rearrange_items set to 1, variable map will be sorted by item_stata alphabetically
)

# ES 24/05/27: Saving excluded variables to their own "variable map"
save_excluded_variables(
  pap = "employment",
  partial_variable_map = partial_map,
  family_name_stata = "labor_elasticity",
  family_name_human = "Labor Supply Elasticity",
  path = "employment/04_bulk/excluded_analysis_wide_variables.xlsx"
)

## Save the LONG dataset -------------------------------------------------------

# Long Data
write_rds(output_data_long, "employment/02_data/labor_elasticity_rid_x_survey_long.Rds")
write_dta(output_data_long, "employment/02_data/labor_elasticity_rid_x_survey_long.dta")

## Open the React Table  -------------------------------------------------------

if (FALSE) read_rds('employment/04_bulk/labor_elasticity_wide_dashboard.rds')
