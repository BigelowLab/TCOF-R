#' List guppy files 
#' 
#' @param path str, the root path to the guppy data
#' @param pattern str, regular expression of the file pattern to locate
#'   Use \code{glob2rx()} to convert glob patterns to regular expressions
#' @param recursive logical, if TRUE search subdirectories recursively
#' @return filenames (fully qualified by path) 
list_guppy = function(path = here::here("data", "guppy"),
                        pattern = glob2rx("*.gup"),
                        recursive = TRUE){
  list.files(path, pattern = pattern, recursive= TRUE, full.names = TRUE)
}


#' Read metadata
#' 
#' @param filename str, a single metadata filename
#' @return list of metadata
read_meta = function(filename){
  
  # does it exist?
  if (!file.exists(filename)){
    stop("file not found:", filename)
  }
  
  yaml::read_yaml(filename)
}

#' Read on or more guppy data files
#' 
#' @param filename str, one or more filenames
#' @param form str, one of "table" (default) or "sf" 
#' @return table or sf POINT table
read_guppy = function(filename = list_guppy(), form = c("table", "sf")[1]){
  
  # multiple files?
  if (length(filename) > 1) {
    
    # read them all as tables, then bind
    xx = lapply(filename, read_guppy, form  = "table") |>
      dplyr::bind_rows()
    
    # convert to sf?
    if (tolower(form[1]) == "sf"){
      xx = sf::st_as_sf(xx, coords = c("x", "y"), crs = "EPSG:26919")
    }
    return(xx)
  }
  
  # does it exist?
  if (!file.exists(filename)){
    stop("file not found:", filename)
  }
  
  # read metadat
  meta_file = sub(".gup", ".yaml", filename, fixed = TRUE)
  meta = read_meta(meta_file)
  
  # read all the lines of text 
  s = readLines(filename)
  n = length(s)
  # find the locations of lines the start with "#"
  ix = grep("##", s, fixed = TRUE)
  # read bits from the header (which happens to be YAML format!)
  hdr = yaml::read_yaml(text = paste(s[(ix[1]+1):(ix[2]-1)], collapse = "\n"))
  # read the rest as a table
  # then append bits and pieces from the header and the yaml
  x = readr::read_csv(paste(s[(ix[2]+1):n], collapse = "\n"), 
                      col_names = TRUE,
                      show_col_types = FALSE) |>
    dplyr::mutate(site_id = hdr$site_id, 
                  x = hdr$x_coord,
                  y = hdr$y_coord,
                  time = hdr$time,
                  researcher = meta$researcher,
                  shade = meta$shade,
                  gps_codes = paste(meta$codes, collapse = "-"),
                  .before = 1)
  if (tolower(form[1]) == 'sf'){
    x = sf:st_as_as(x, coords = c("x", "y"), crs = "EPSG:26919")
  }
  return(x)
}



#' Retrieve the guppy box in UTM Zone 19 coords
#' 
#' @param crs char one of "geographic" or "UTM"
#' @return fc polygon object
guppy_polygon = function(crs = c("geographic", "UTM")){
  xy = dplyr::tribble(
    ~y, ~x,
    43.871483554566936, -69.59710209044029,
    43.871109211770445, -69.59662502232892,
    43.87131316827151, -69.59632232561755,
    43.871559799418776, -69.59667394604624,
    43.871483554566936, -69.59710209044029) |>
    dplyr::select(x, y)
  
  poly = list(as.matrix(xy)) |>
    sf::st_polygon() |>
    sf::st_sfc(crs = 4326)
  
  if (tolower(crs[1]) == "utm") poly =  sf::st_transform(poly, crs = "EPSG:26919")
  poly
}

#' Generate one or more guppy files
#'
#' For each guppy file we create a subdirectory, and within two files
#' "<name>.gup" (header and table file) and <name>.meta (metadata yaml file) 
#' 
#' @param n num, the number of guppy files to generate
#' @param path str, the destination path
#' @return NULL invisibly
generate_guppy = function(n = 1, path = here::here("data", "guppy")){
 
  poly = guppy_polygon(crs = "UTM")
  locs = sf::st_sample(poly, n)
  xy = sf::st_coordinates(locs) |>
    dplyr::as_tibble()
  time = as.POSIXct("2020-05-16T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
  times = (time + 3600*6 + sample(3600*3, n)) |>
    format("%Y-%m-%dT%H:%M:%S", usetz = TRUE)
  N = sample(seq(from = 10, to = 30), n, replace = TRUE)

  researchers = sample(c("LDT", "NLS", "JPO"), n, replace = TRUE)
 
  shade = sample(seq(from = 0, to = 100, by = 10), n, replace = TRUE)
  
  site_ids = sprintf("site_%0.2i", seq_len(n))

  site_path = file.path(path, site_ids)
  ok = sapply(site_path, dir.create, recursive = TRUE, showWarnings = FALSE)
  gup_files = file.path(site_path, paste0(site_ids, ".gup"))
  meta_files = file.path(site_path, paste0(site_ids, ".yaml"))
  
  write_meta = function(filename, researcher, site_id, time, shade){
    list(site_id = site_id,
        researcher = researcher,
        time = time, 
        shade = shade,
        gps_codes = sample(letters, 3, replace = TRUE)) |>
      yaml::write_yaml(filename)
  }
  
  
  for (i in seq_len(n)){
    
    write_meta(meta_files[i], researchers[i], site_ids[1], times[i], shade[i])
    
    fileconn = file(gup_files[i], open = 'wt')
    cat("## Guppy studies\n", file = fileconn)
    list(site_id = site_ids[i],
         x_coord = xy$X[i],
         y_coord = xy$Y[i],
         time = times[i]) |>
      yaml::write_yaml(fileconn)
    cat("## data\n", file = fileconn)
    close(fileconn)
    tbl = dplyr::tibble(id = seq_len(N[i]),
                        treatment = sample(LETTERS[1:5], N[i], replace = TRUE),
                        count = sample(seq(from = 25, to = 75), N[i], replace = TRUE),
                        dose = round(sample(seq(from = 0, to = 1, by = 0.1), N[i], replace = TRUE), 2)) |>
      readr::write_csv(gup_files[i], col_names = TRUE, append = TRUE)
    
  }
  
  invisible(NULL)
}