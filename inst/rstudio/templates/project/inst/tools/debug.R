ravecore::load_scripts(rlang::quo({
  # debug.R runs before comp.R and main.R, 
  # providing some basic tools that will be available during the run-time
  
  
  # Session data: run-time based. Should store the reactive user's inputs
  # session_data will be different for each tabs
  session_data = ravecore::getDefaultSessionData()
  
  # Module data: shared across sessions (multiple web tabs), but stays within 
  # a package
  package_data = ravecore::getDefaultPackageData()
  
  
  # Global data: an environment that is shared across packages. stores some
  # RAVE specific data (power, phase, etc), rarely used by rave modules to 
  # store data (read-only)
  global_data = ravecore::getDefaultDataRepository()
  
  
  
  # Reactives
  input = ravecore::getDefaultReactiveInput()
  
  output = ravecore::getDefaultReactiveOutput()
  
  reactive_data = shiny::reactiveValues()
  
  session = shiny::getDefaultReactiveDomain()
  session %?<-% ravecore::fake_session()
  
  ns <- session$ns
  
}))

