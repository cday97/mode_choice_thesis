#' Download the source data from BOX
#' 
get_data_from_box <- function(){
  data_zip <- "data/data.zip"
  url <- "https://byu.box.com/shared/static/tzuv88o389qly9zi7mw6jlk6bhg9jgkt.zip"
  
  if(!file.exists(data_zip)){
    download.file(url, destfile = data_zip, 
                  method = "auto",
                  timeout = max(10000, getOption("timeout"))
                  )
  } else {
    message("File already exists")
  }
  
  data_zip
}

#' Extract a file from the ZIP archive
#' 
#' @param file path to the desired file
#' @param data_zip path to the zip archive
unzip_data <- function(file, data_zip){
  
  # see if 7z is installed
  pz7 <- system2("which", args = "7z", stdout = TRUE)
  if (!is.null(attr(pz7,"status")) ){
    stop("Could not find 7z. Please install and add to your system path")
  }
  
  # make directory to stash file (if it doesn't exist)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  
  # extract requested file (will deposit at requested place)
  system2(pz7, args = c( "x", data_zip, file, "-aoa" ))
  
  # targets needs us to return the file path.
  return(file)
}

#' sets column types by name
get_coltypes <- function(event_cols){
  coltypes <- read_csv(event_cols, col_names = F)
  coltypes <- set_names(pull(coltypes, 2), pull(coltypes ,1))
  coltypes
}


#' join tibbles
all_join <- function(events_list, func, join_col, col_1_name, ...){
  
  events <- future_map(events_list, func, ...)
  
  full <- events[[1]]
  
  for(i in 2:length(events_list)){
    full <- full %>% 
      full_join(events[[i]], by = join_col)
  }
  
  full %>% 
    `colnames<-`(c(col_1_name, names(events_list)))
}

rbind_join <- function(events_list, func){
  full <-list()
  for(i in 1:length(events_list)){
    full[[i]] <- func(events_list[[i]]) %>% 
      mutate(ScenarioName = names(events_list)[i])
  }
  
  bind_rows(full, .id = "id") %>%
    select(-id)
}

