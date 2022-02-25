.volatiles <- new.env(parent=emptyenv())

.volatiles$tbl <- NULL
.volatiles$ol <- list(
  study_type = c("Interventional", "Observational",
                 "Observational [Patient Registry]", "Expanded Access"),
  allocation = c("Randomized", "N/A", "Non-Randomized"),
  intervention_model = c("Parallel Assignment", "Single Group Assignment",
                         "Crossover Assignment", "Sequential Assignment",
                         "Factorial Assignment"),
  observational_model = c("Cohort", "Case-Control", "Case-Only", "Other",
                          "Ecologic or Community", "Case-Crossover",
                          "Defined Population", "Family-Based"),
  primary_purpose =  c("Treatment", "Prevention", "Basic Science",
                       "Supportive Care", "Other", "Diagnostic",
                       "Health Services Research", "Screening",
                       "Device Feasibility", "Educational/Counseling/Training"),
  time_perspective = c("Prospective", "Retrospective",
                       "Cross-Sectional", "Other"),
  masking_description = c("None (Open Label)", "Single", "Double", "Triple",
                          "Quadruple"),
  sampling_method = c("", "Non-Probability Sample", "Probability Sample"),
  phase = c("N/A", "Early Phase 1", "Phase 1", "Phase 1/Phase 2", "Phase 2",
            "Phase 2/Phase 3", "Phase 3", "Phase 4"),
  gender = c("All", "Female", "Male"),
  sponsor_type = c("Industry", "NIH", "U.S. Fed", "Other")
)
