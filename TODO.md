# TODO

- [] hide function bodies when you run the bare function name in the console
- [] you shouldn't change to output from data.frame to tibble silently. Perhaps use an option to output as nicer tibble (tibble = TRUE).
- [] consider add a function to convert fahrenheit to celsius, for overseas users? Or auto detect the location of input data and prompt this conversion?
- [] In sample_df, ensure set seed does not change user state
- [] Add code for how models were fitted to the package so that it can be re-generated at any time 
- [] Access gridded climate data for Australia to predict development at any location
- [] Remove global variables https://stackoverflow.com/questions/12598242/global-variables-in-packages-in-r # generally a no-go
- [] check R landscape https://rdrr.io/cran/chillR/man/make_hourly_temps.html
- [] Gridded climate data for Australia with actual temperatures
- [] Predict seasonal phenology of DBM anywhere in Australia. Use it to predict risk?
- [] Model Helicoverpa risk
- [] Add a function to visualise fitted temperature-development functions for lifestages 
- [] Remove extraneous files/folders from the root directory and .Rbuildignore
- [] Fix commented-out tests in test-hourly.R

## Predict development
- [] Allow multiple locations in one function?
- [] User should be able to query the output to find out a timepoint when a particular event happened ("egg lay", or completion of pupal development (eclosion).
- [] Perhaps make this "end_stage" arg. Separate function to query the output?
- [] Add a time delay value for moth/egg lay days. Default to one. Provides a user specified gap between generations.
 


