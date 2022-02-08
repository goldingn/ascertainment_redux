#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author dhduncan
#' @export
get_abm_data <- function() {

  path_to_file = "~/Documents/covid/simulate-infections/outputs/sim_output.csv"
  
  # keep this simple for now, it might be good to get this up to replication stage, run the model on a sequence of simulations, they could have varying parameters.
  
  # for now - only one sim output
  # hide the undetected cases
  
  abm_data <- read_csv(path_to_file) %>% 
    filter(simulation == 'sim_1') %>% 
    select(-c(simulation)) %>% 
    mutate(sim_days = infection_day - min(infection_day-1),
           date_num = sim_days / max(sim_days)) %>% 
           #contact_test_prob = random_trend(sim_days, prob = 0.9),
           #symptomatic_test_prob = random_trend(sim_days, 0.8, variance = 1),
           #screening_test_prob = random_trend(sim_days, 1 - ((1 - 5/7) ^ 34))) %>%
    relocate(date_num) %>% 
    select(-c(infection_day))
    
}
