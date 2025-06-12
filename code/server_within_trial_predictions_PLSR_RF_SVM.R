# Within-trial predictions with PLSR, RF, and SVM

# Jenna Hershberger
# jmh579@cornell.edu


library(tidyverse)
library(magrittr)
library(waves)
library(pls)
library(randomForest)
library(kernlab)
library(caret)

niterations <- 50
plots.aggregated <- read.csv("output/full_filtered_plots.csv", stringsAsFactors = F)


start_PLSR <- proc.time()
waves_results_nested_PLSR <- plots.aggregated %>%
  rename(reference = dry.matter.content.percentage.CO_334.0000092,
         unique.id = observationUnitName) %>%
  dplyr::select(studyName, unique.id, reference, starts_with("X")) %>%
  drop_na() %>%
  distinct() %>%
  group_by(studyName) %>%
  nest() %>%
  mutate(waves_results = map(data,
                             function(df) TestModelPerformance(train.data = df,
                                                               num.iterations = niterations,
                                                               test.data = NULL,
                                                               preprocessing = T,
                                                               wavelengths = 740:1070,
                                                               tune.length = 15,
                                                               model.method = "pls",
                                                               output.summary = F)))
waves_results_flat_PLSR <- waves_results_nested_PLSR %>%
  unnest(cols = c(waves_results)) %>%
  dplyr::select(-data)

time_PLSR <- proc.time() - start_PLSR

write.csv(waves_results_flat_PLSR, "output/within_trial_waves_PLSR.csv", row.names = F)

print("--------------- Finished with PLSR ----------------")

start_RF <- proc.time()
waves_results_nested_RF <- plots.aggregated %>%
  rename(reference = dry.matter.content.percentage.CO_334.0000092,
         unique.id = observationUnitName) %>%
  dplyr::select(studyName, unique.id, reference, starts_with("X")) %>%
  drop_na() %>%
  distinct() %>%
  group_by(studyName) %>%
  nest() %>%
  mutate(waves_results = map(data,
                             function(df) TestModelPerformance(train.data = df,
                                                               num.iterations = niterations,
                                                               test.data = NULL,
                                                               preprocessing = T,
                                                               wavelengths = 740:1070,
                                                               tune.length = 5,
                                                               model.method = "rf",
                                                               output.summary = F)))
waves_results_flat_RF <- waves_results_nested_RF %>%
  unnest(cols = c(waves_results)) %>%
  dplyr::select(-data)
time_RF <- proc.time() - start_RF

write.csv(waves_results_flat_RF, "output/within_trial_waves_RF.csv", row.names = F)

print("--------------- Finished with RF ----------------")

start_SVM <- proc.time()
waves_results_nested_SVM <- plots.aggregated %>%
  rename(reference = dry.matter.content.percentage.CO_334.0000092,
         unique.id = observationUnitName) %>%
  dplyr::select(studyName, unique.id, reference, starts_with("X")) %>%
  drop_na() %>%
  distinct() %>%
  group_by(studyName) %>%
  nest() %>%
  mutate(waves_results = map(data,
                             function(df) TestModelPerformance(train.data = df,
                                                               num.iterations = niterations,
                                                               test.data = NULL,
                                                               preprocessing = T,
                                                               wavelengths = 740:1070,
                                                               tune.length = 15,
                                                               model.method = "svmLinear",
                                                               output.summary = F)))
waves_results_flat_SVM <- waves_results_nested_SVM %>%
  unnest(cols = c(waves_results)) %>%
  dplyr::select(-data)
time_SVM <- proc.time() - start_SVM

write.csv(waves_results_flat_SVM, "output/within_trial_waves_SVM.csv", row.names = F)

print("--------------- Finished with SVM ----------------")


algorithm <- c("PLSR", "RF", "SVM")
times.df <- rbind(time_PLSR, time_RF, time_SVM) %>%
  cbind(algorithm) %>% as.data.frame() %>% dplyr::select(algorithm, everything())
write.csv(times.df, "output/algorithm_runtimes.csv", rownames = F)
