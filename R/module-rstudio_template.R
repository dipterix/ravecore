# Function to create a RAVE module template
create_template <- function(path, ...){
  this_call = match.call()
  args = list(...)

  # step 1: get package name
  # path = normalizePath(path)

  rave_info('Creating RAVE Module - {path}')

  PACKAGE = utils::tail(strsplit(path, '\\\\|/')[[1]],1)
  MODULEID = args[['module_id']]

  # ensure path exists
  dir_create(path)
  dir_create(file.path(path, 'inst', 'tools'))
  dir_create(file.path(path, 'inst', 'modules', MODULEID))
  dir_create(file.path(path, 'R'))


  # check MODULEID, must starts with 'a-zA-z' and only contains 'a-zA-Z0-9_'
  MODULEID = gsub('[^a-zA-Z_]', '', MODULEID)
  MODULEID = gsub('[_]{2,}', '_', MODULEID)
  if(MODULEID == ''){
    MODULEID = 'module_id'
  }
  rave_info('Module ID - {MODULEID}')

  MODULELABEL = args[['module_label']]
  MODULELABEL = gsub('(^[\\ ]*)|([\\ ]$)', '', MODULELABEL)
  if(MODULELABEL == ''){
    MODULELABEL = 'Missing Label'
  }
  rave_info('Display Label - {MODULELABEL}')

  # migrate template
  template_dir = system.file('rstudio/templates/project', package = 'ravecore')
  # template_dir = './inst/template'
  fs = c(
    'DESCRIPTION',
    'NAMESPACE',
    'R/aaa.R',
    'inst/rave2.yaml',
    'inst/modules/first_example/comp.R',
    'inst/modules/first_example/main.R',
    'inst/tools/debug.R',
    'inst/tools/input_widgets.R',
    'inst/tools/output_widgets.R'
  )

  for(f in fs){
    s = readLines(file.path(template_dir, f))
    s = paste(s, collapse = '\n')
    s = glue(s, .open = "${{", .close = "}}")
    f = stringr::str_replace(f, 'first_example', MODULEID)
    writeLines(s, con = file.path(path, f))
  }

  # Write .Rbuildignore
  s = c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^hello\\.R$", "^readme\\.md$",  "adhoc/")
  writeLines(s, con = file.path(path, '.Rbuildignore'))

  # Rename template.Rproj

}





# rstudioapi::createProjectTemplate(
#   binding = 'create_template',
#   package = '.',
#   title = 'RAVE Module',
#   open_files = c('R/inputs.R', 'R/outputs.R'),
#
# )
