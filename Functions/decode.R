decode <-
  function(alg_input,
           .envir = parent.frame()) {
    if (!.envir$use_test_bench) {
      alg_input <-
        unique(copy(.envir$siteInfo)[, list(Facility_name, Bed_Group, total_beds)]
               )[, inputs := alg_input
                 ][, `:=`(Sums = sum(inputs), 
                          fac_beds = sum(total_beds)),
                   by = Facility_name
                   ][, bed_counts := {Vectorize(smart_round)}(na.replace(inputs / Sums, 1) * fac_beds)]
      return(alg_input$bed_counts)
    } else {
      new_alloc <- alg_input %>%
        norm_vec()
      new_alloc <-
        smart_round(x = new_alloc * .envir$total_servers)
      return(new_alloc)
    }
  }
