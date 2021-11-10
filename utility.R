download_acs = function(x, ...) {
  option_args = list(...)
  acs_args = c(do.call('acs_options', option_args), list(variables = unlist(unname(x))))
  variable_lookup = setNames(names(x), unlist(x))
  do.call('get_acs', acs_args) %>% 
    as.data.table %>%
    .[, .(fips = GEOID, variable = variable_lookup[variable], value = estimate)]
}

download_acs_categorical = function(x, ...) {
  acs_args = list(...)
  do.call('download_acs', c(list(x = x), acs_args)) %>%
    dcast(fips ~ variable, value.var = 'value') %>%
    melt(id.vars = c('fips', 'total'), variable.name = 'response')
}

acs_options = function(...) {
  input = list(...)
  default = list(
    geography = 'block group', 
    year = 2019, 
    state = '06',
    county = '029')
  default_values = setdiff(names(default), names(input))
  c(default[default_values], input)
}

