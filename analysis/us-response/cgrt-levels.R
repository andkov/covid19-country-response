
# ---- c -------------
# C1_School closing
levels_c1 <- c(
  "0" =  "0 - no measures"
  ,"1" = "1 - recommend closing"
  ,"2" = "2 - require closing (only some levels or categories, eg just high school, or just public schools)"
  ,"3" = "3 - require closing all levels"
)
# C2_Workplace closing
levels_c2 <- c(
  "0" =  "0 - no measures"
  ,"1" = "1 - recommend closing (or recommend work from home)"
  ,"2" = "2 - require closing (or work from home) for some sectors or categories of workers"
  ,"3" = "3 - require closing (or work from home) for all-but-essential workplaces (eg grocery stores, doctors)"
)
# C3_Cancel public events
levels_c3 <- c(
  "0" =  "0 - no measures"
  ,"1" = "1 - recommend cancelling"
  ,"2" = "2 - require cancelling"
)
# C4_Restrictions on gatherings
levels_c4 <- c(
  "0" =  "0 - no restrictions"
  ,"1" = "1 - restrictions on gatherings for 1,000+ people "
  ,"2" = "2 - restrictions on gatherings for 101 - 1,000 people"
  ,"3" = "3 - restrictions on gatherings for 11 - 100 people"
  ,"4" = "4 - restrictions on gatherings for 10 or less people"
)
# C5_Close public transport
levels_c5 <- c(
  "0" =  "0 - no measures"
  ,"1" = "1 - recommend closing (or significantly reduce volume/route/means of transport available)"
  ,"2" = "2 - require closing (or prohibit most citizens from using it)"
)
# C6_Stay at home requirements
levels_c6 <- c(
  "0" =  "0 - no measures"
  ,"1" = "1 - recommend not leaving house"
  ,"2" = "2 -  require not leaving house with exceptions for daily exercise, grocery shopping, and 'essential' trips"
  ,"3" = "3 - require not leaving house with minimal exceptions (eg allowed to leave once a week, or only one person can leave at a time, etc)"
)
# C7_Restrictions on internal movement
levels_c7 <- c(
  "0" =  "0 - no measures"
  ,"1" = "1 - recommend not to travel between regions/cities"
  ,"2" = "2 - internal movement restrictions in place"
)
# C8_International travel controls
levels_c8 <- c(
  "0" =  "0 - no restrictions"
  ,"1" = "1 - screening arrivals"
  ,"2" = "2 - quarantine arrivals from some or all regions"
  ,"3" = "3 - ban arrivals from some regions"
  ,"4" = "4 -  ban on all regions or total border closure"
)

# ----- e ---------------
# E1_Income support (for households)
levels_e1 <- c(
  "0" =  "0 - no income support"
  ,"1" = "1 - government is replacing less than 50% of lost salary (or if a flat sum, it is less than 50% median salary)"
  ,"2" = "2 - government is replacing 50% or more of lost salary (or if a flat sum, it is greater than 50% median salary)"
)
# E2_Debt/contract relief (for households)
levels_e2 <- c(
  "0" =  "0 - no debt/contract relief"
  ,"1" = "1 - narrow relief, specific to one kind of contract"
  ,"2" = "2 - broad debt/contract relief"
)

# E3_Fiscal measures - monetary value in USD of fiscal stimuli, includes any spending or tax cuts NOT included in E4, H4 or H5

# E4_International support - monetary value in USD

# ------ h ------------------
# H1_Public information campaigns
levels_h1 <- c(
  "0" =  "0 - no Covid-19 public information campaign"
  ,"1" = "1 - public officials urging caution about Covid-19"
  ,"2" = "2 - coordinated public information campaign (eg across traditional and social media)"
)
# H2_Testing policy
levels_h2 <- c(
  "0" =  "0 - no testing policy"
  ,"1" = "1 - only those who both (a) have symptoms AND (b) meet specific criteria (eg key workers, admitted to hospital, came into contact with a known case, returned from overseas)"
  ,"2" = "2 - testing of anyone showing Covid-19 symptoms"
  ,"3" = "3 - pen public testing (eg 'drive through' testing available to asymptomatic people)"
)
# H3_Contact tracing
levels_h3 <- c(
  "0" =  "0 - no contact tracing"
  ,"1" = "1 - limited contact tracing; not done for all cases"
  ,"2" = "2 - comprehensive contact tracing; done for all identified cases"
)

# H4_Emergency investment in healthcare - monetary value in USD
# H5_Investment in vaccines - monetary value in USD




# ----- l -----------
cgrt_levels <- list(
  "c1"  = levels_c1
  ,"c2" = levels_c2
  ,"c3" = levels_c3
  ,"c4" = levels_c4
  ,"c5" = levels_c5
  ,"c6" = levels_c6
  ,"c7" = levels_c7
  ,"c8" = levels_c8
  ,"e1" = levels_e1
  ,"e2" = levels_e2
  ,"h1" = levels_h1
  ,"h2" = levels_h2
  ,"h3" = levels_h3
)
