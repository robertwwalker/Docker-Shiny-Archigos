# Shiny app in a docker container


I started with a fork of a repo accompanies an article [here](https://blog.sellorm.com/2021/04/25/shiny-app-in-docker/).


# 'image' directory

This contains the entirety of the image files.

- Dockerfile
- `shiny-server.conf`
- `shiny-server.sh`
- the *app* directory
  + `app.R` the single app file.
  + the *data* folder
    + `archigos.rda` containing the key data files.


The *app* directory contains only those things which are needed for the app to run.
In our case that's the `app.R` file and the rda file we wrote out earlier.

The .rda file comes from the following R code.

```
library(tidyverse)
library(haven)
library(magrittr)
library(utf8)
library(lubridate)
Archigos <- read_dta(url("http://www.rochester.edu/college/faculty/hgoemans/Archigos_4.1_stata14.dta"))
Archigos %<>% mutate(leader = utf8_encode(leader))
Archigos %<>% mutate(tenure = as.duration(eindate %--% eoutdate)) %>% # Create duration for each spell  
  mutate(tenureY = tenure / dyears(1))    # Measure duration in years.
```


## Dockerfile and project layout

The Dockerfile here has everything that we need to build the Docker container.  There is [a blog post on the entirety of this exercise here.](https://robertwwalker.github.io/posts/Docker-Shiny/)


## `shiny-server.conf`

The configuration files for the shiny server.

## `shiny-server.sh`

The logs are configured here.
