## Summary

## Test Environments

## R CMD check results

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Robert Hickman <robwhickman@gmail.com>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  ggplot (4:45)

* checking top-level files ... NOTE
Non-standard file/directory found at top level:
  'figure'

** running examples for arch 'i386' ... ERROR
Running examples in 'ggparliament-Ex.R' failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: draw_majoritythreshold
> ### Title: Draw majority threshold
> ### Aliases: draw_majoritythreshold
> 
> ### ** Examples
> 
> data <- election_data[election_data$country == "USA" & 
+ election_data$house == "Representatives" & 
+ election_data$year == "2016",]
> usa_data <- parliament_data(election_data = data, 
+ type = "semicircle", 
+ party_seats = data$seats, 
+ parl_rows = 8)
> ggplot(usa_data, aes(x, y, color=party_long)) + 
+ geom_parliament_seats() + 
+ draw_majoritythreshold(n = 218, 
+ label = TRUE, 
+ type = 'semicircle') + 
+ theme_ggparliament()
Error in ggplot(usa_data, aes(x, y, color = party_long)) : 
  could not find function "ggplot"
Execution halted

** running examples for arch 'x64' ... ERROR
Running examples in 'ggparliament-Ex.R' failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: draw_majoritythreshold
> ### Title: Draw majority threshold
> ### Aliases: draw_majoritythreshold
> 
> ### ** Examples
> 
> data <- election_data[election_data$country == "USA" & 
+ election_data$house == "Representatives" & 
+ election_data$year == "2016",]
> usa_data <- parliament_data(election_data = data, 
+ type = "semicircle", 
+ party_seats = data$seats, 
+ parl_rows = 8)
> ggplot(usa_data, aes(x, y, color=party_long)) + 
+ geom_parliament_seats() + 
+ draw_majoritythreshold(n = 218, 
+ label = TRUE, 
+ type = 'semicircle') + 
+ theme_ggparliament()
Error in ggplot(usa_data, aes(x, y, color = party_long)) : 
  could not find function "ggplot"
Execution halted

