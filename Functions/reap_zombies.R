reap_zombies <-
  function() {
    # Kill left over R processes after parallel computing
    
    if (Sys.info()['sysname'] == 'Darwin') {
      ids <-
        ps() %>% setDT() %>% {
          function(input)
            input[name == 'rsession', pid] %>% as.integer()
        }()
    } else if (Sys.info()['sysname'] == 'Windows') {
      ids <-
        ps() %>% setDT() %>% {
          function(input)
            input[name == 'Rscript.exe', pid] %>% as.integer()
        }()
      
      if (length(ids) > 1) {
        id_list <- ps()[['pid']] %>% unlist()
        for (i in seq_along(ids)) {
          tryCatch({
            ps_kill(ps_handle(ids[i]))
          }, error = function(e) {
            cat("ERROR :", conditionMessage(e), "\n")
          })
        }
      }
    }
  }