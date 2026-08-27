
#---------26 abril 2019------
#---------Julio César Iturra Sanhueza - jciturra@uc.cl -----

library(haven)
library(sjPlot)
library(dplyr)
library(sjlabelled)

exp_pob02 <- read_sav("exp_pob02.sav")


view_df(exp_pob02)
names(exp_pob02)

# First Click: How many seconds pass before the respondent clicks the first time.
# Last Click: How many seconds pass before the respondent clicks the last time (not including clicking the Next button).
# Page Submit: How many seconds pass before the respondent clicks the Next button (i.e., how long in total the respondent spends on the page).
# Click Count: How many times the respondent clicks on the page.

browseURL("https://www.qualtrics.com/support/survey-platform/survey-module/editing-questions/question-types-guide/advanced/timing/")

# Base completa -----------------------------------------------------------

exp_pob02$t1_time_total <- (
  exp_pob02$time_t1a_Page_Submit +
    exp_pob02$time_t1b_Page_Submit +
    exp_pob02$time_t1c_Page_Submit +
    exp_pob02$time_t1d_Page_Submit
)
exp_pob02$t2_time_total <- (exp_pob02$time_t2a_Page_Submit +
                              exp_pob02$time_t2b_Page_Submit)

exp_pob02$ctrl_time_total <- (
  exp_pob02$time_ctrla_Page_Submit +
    exp_pob02$time_ctrlb_Page_Submit +
    exp_pob02$time_ctrlc_Page_Submit +
    exp_pob02$time_ctrld_Page_Submit
)

exp_pob02$t1_time_total <-
  set_label(exp_pob02$t1_time_total, label = "Tiempo total (segundos) en Tratamiento Pobreza")
exp_pob02$t2_time_total <-
  set_label(exp_pob02$t2_time_total, label = "Tiempo total(segundos) en Tratamiento Desigualdad")
exp_pob02$ctrl_time_total <-
  set_label(exp_pob02$ctrl_time_total, label = "Tiempo total (segundos) en Control Cigarrillos")


mean(exp_pob02$t1_time_total, na.rm = TRUE)
mean(exp_pob02$t2_time_total, na.rm = TRUE)
mean(exp_pob02$ctrl_time_total, na.rm = TRUE)

write_sav(data =exp_pob02,path = "exp_pob02_times_total.sav")

read_sav("exp_pob02_times_total.sav")

