<<<<<<< HEAD
decode <-function(alg_input, .envir = parent.frame()) {
  # Function: This function converts a solution vector of real values into a integer vector for use in simulation.
  
  # Inputs:
  #   alg_input - (Numeric) - a vector of real values
  
  # Returns:
  #   new_alloc - (Integer) - vector of integers representing servers at each queue.
=======
decode <-
  function(alg_input,
           .envir = parent.frame()) {
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
    if (!.envir$use_test_bench) {
      alg_input <-
        unique(copy(.envir$siteInfo)[, list(Facility_name, Bed_Group, total_beds)]
               )[, inputs := alg_input
                 ][, `:=`(Sums = sum(inputs), 
                          fac_beds = sum(total_beds)),
                   by = Facility_name
<<<<<<< HEAD
<<<<<<< HEAD
                   ][, bed_counts := {Vectorize(smart_round)}(na.replace(inputs / Sums, 1) * fac_beds)]
=======
                   ][, bed_counts := {Vectorize(smart.round)}(na.replace(inputs / Sums, 1) * fac_beds)]
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
                   ][, bed_counts := {Vectorize(smart_round)}(na.replace(inputs / Sums, 1) * fac_beds)]
>>>>>>> 8c8946d (Fixed some custom functions.)
      return(alg_input$bed_counts)
    } else {
      new_alloc <- alg_input %>%
        norm_vec()
      new_alloc <-
<<<<<<< HEAD
<<<<<<< HEAD
        smart_round(x = new_alloc * .envir$total_servers)
=======
        smart.round(x = new_alloc * .envir$total_servers)
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
        smart_round(x = new_alloc * .envir$total_servers)
>>>>>>> 8c8946d (Fixed some custom functions.)
      return(new_alloc)
    }
  }
