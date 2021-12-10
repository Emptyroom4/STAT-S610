#Q3 adaptive_rejection_sampling _ main
# Hyeseon

adaptive_rejection_sampling <- function(n, h, interval, x,...){
  #input checks
  #start an environment for the envelope
  envelope_env = new.env()
  initialize_envelope_info(envelope_env, h, interval, x, ...)
  out = rep(NA, n)
  i = 0
  while(TRUE){
    ##draw proposal from outer envelope
    x_val = upper_envelope_sample(envelope_env)
    #draw a uniform(0,1)
    u = rnuif(1)
    #test the proposal with the squeeze test (scaled)
    upper_y_val = upper_envelope_evaluate(x_val, envelope_env, log=TRUE)
    lower_y_val = lower_envelope_evaulate(x_val, envelope_env, log = TRUE)
    if(log(u) < lower_y_val - upper_y_val) { # see if ite is below the inner
      #envelope(scaled) 
      # if proposal is accepted, increment i and insert proposal into output
      i = i + 1
      out[i] = x_val
    } else { #if fail to accept on th squeez test, then test with h directly
      h_y_val = h(x_val,...)
        if(log(u) < h_y_val - upper_y_val){
          i = i + 1
          out[i] = x_val
        }
      update_environment_info(envelope_env, x_val, h_y_val,...)
        # if proposal is accepted, increment i and insert proposal into output
    }
    if (i == n) break
  }
  return(out)
}
###########


adaptive_rejection_sampling = function(n, h, interval, x, ...) {
  # input checks
  
  # Start an environment for the envelope
  envelope_env = new.env()
  initialize_envelope_info(envelope_env, h, interval, x, ...) 
  
  out = rep(NA, n)
  i = 0
  while (TRUE) {
    # draw proposal from outer envelope
    x_val = upper_envelope_sample(envelope_env)
    # draw a uniform((0.1))
    v = runif(1)
    # test the proposal with the squeeze test
    upper_y_val = upper_envelope_evaluate(x_val, envelope_env, log=TRUE)
    lower_y_val = lower_envelope_evaluate(x_val, envelope_env, log=TRUE)
    if(log(v) < lower_y_val - upper_y_val) {## see if it is below the inner envolope (scaled)
      # if proposal is accepted, increment i and insert proposal into output
      i = i + 1
      out[i] = x_val
    }else{# if fail to accept on the squeeze test, then test with h directly
      h_y_val = h(x_val,...)
      if(log(v) < h_y_val-upper_y_val) {
        # if proposal is accepted, increment i and insert proposal into output
        i = i + 1
        out[i] = x_val
      } 
      ## add the proposed x value to the envelope
      update_environment_info(envelope_env, h, x_val, h_y_val, ...)
    }
    if (i == n) break
  }
  return(out)
}