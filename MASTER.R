#' MASTER.R
#'
#' Master file for the project Land_Use_Disadvantages_Main_Analysis.Rmd
#'
#' Author:                Anonymous
#' Created:               2021-03-23
#' Version:               3.0
#' R-Version:             R version 4.0.4 (2021-02-15)
#' CPU:                   x86-64
#' Loaded Base Packages:  stats, graphics, grDevices, utils, datasets, methods, base
#' Loaded Other Packages: tibble (3.0.6), sdcR (0.1.0), sf (0.9-7), RStata (1.1.1), readr (1.4.0), raster (3.4-5), sp (1.4-5), plyr (1.8.6), patchwork (1.1.1), osmdata (0.1.4), magrittr (2.0.1), huxtable (5.2.0), httr (1.4.2), htmlTable (2.1.0), haven (2.3.1), ggplot2 (3.3.3), furrr (0.2.2), future (1.21.0), dplyr (1.0.4), car (3.0-10), carData (3.0-4), easypackages (0.1.0)
#'
#' Details: Reproduces all data preparation and analysis steps
#'


#' ini_user_toggles.R
default_buffer_size <- 500


#' ini-load-packages.R
# load the easypackages package
if (!require(easypackages)) { install.packages("easypackages") } 
library(easypackages)

easypackages::packages(
  "car",
  "dplyr",
  "furrr",
  "ggplot2",
  "haven",
  "htmlTable",
  "httr",
  "huxtable",
  "magrittr",
  "osmdata",
  "patchwork",
  "plyr",
  "raster",
  "readr",
  "RStata",
  "sf",
  "sp",
  "StefanJuenger/sdcR",
  "tibble",
  prompt = FALSE
)


#' ini-stata_setup_real.R
# options("RStata.StataPath" = "G:/Stata-SE-15/StataSE-64")
options("RStata.StataPath" = "C:/Users/mueller2/Desktop/Stata-SE-15/StataSE-64")
options("RStata.StataVersion" = 15)

stata_data_path = "./data_stata/" 


#' ini-stata_setup_fake.R
## options("RStata.StataPath" = "/path/to/stata/Stata-SE-15/StataSE-64")
## options("RStata.StataVersion" = 15)
## 
## stata_data_path = "./data_stata/"


#' fun_color_gradient.R
color_gradient <- function (x, palette) {
  
  get_palette <- suppressWarnings(
    colorRampPalette(RColorBrewer::brewer.pal(20, 
                                              palette))
  ) 
  
  get_palette(length(x))[order(order(x))]
}


#' fun_censor_variable.R
censor_variable <- function (data, variable, descending = FALSE) {
  
  variable <- rlang::enquo(variable)
  
  if (isFALSE(descending)) {
    data %>% 
      dplyr::arrange(!! variable) %>% 
      dplyr::slice(20) %>%
      dplyr::select(!! variable) %>%
      trunc(0)
  } else {
    data %>% 
      dplyr::arrange(dplyr::desc(!! variable)) %>% 
      dplyr::slice(20) %>%
      dplyr::select(!! variable) %>%
      trunc(0)
  }
}


#' fun-r_to_stata_formula.R
r_to_stata_formula <- function (r_formula, data_to_check, gsem = FALSE) {
  
  # stata_variable_type_checker
  stata_variable_type_checker <- function (variable) {
    if (is.numeric(data_to_check[[variable]])) {
      paste0("c.", variable)
    } else if (is.factor(data_to_check[[variable]])) {
      paste0("i.", variable)
    }
  }
  
  # basic formatting
  stata_formula <-
    r_formula %>% 
    gsub("[~]", "", .) %>% 
    gsub("[+]", "", .) %>% 
    gsub("[:]", "#", .) %>% 
    
    # seperate elements for further inspection
    strsplit(., split = "  ") %>% 
    unlist()
  
  # convert quadratic terms
  stata_formula <- 
    sapply(stata_formula, function (term) {
      if (stringr::str_detect(term, "[I(]")) {
        sub(
          "([I][(])(.+)([\\^][2][)])(.*)",
          "\\2#\\2\\4",
          term
        )
      } else {
        term
      }
    }, 
    USE.NAMES = FALSE
    )
  
  # define variable types based on data_to_check
  stata_formula <- 
    stata_formula[1] %>% # exclude dependent variable
    c(.,
      sapply(stata_formula[-1], function (term) {
        if (!stringr::str_detect(term, "#")) {
          stata_variable_type_checker(term)
        } else {
          terms <-
            strsplit(term, split = "#") %>%
            unlist()
          
          sapply(terms, function (term) {
            stata_variable_type_checker(term)
          }, 
          USE.NAMES = FALSE) %>%
            paste(collapse = "#")
        }
      }, 
      USE.NAMES = FALSE)
    )  %>%
    paste(collapse = " ")
  
  # add <- for gsem
  if (isTRUE(gsem)) {
    stata_formula <- 
      glue::glue(
        "{stringr::word(stata_formula, 1)} <- ", 
        "{stringr::word(stata_formula, 2, -1)}"
      )
  }
  
  return(stata_formula)
}


#' fun_create_tidy_override.R
create_tidy_override <- function (estimations, glance_add = NULL) {
  hux_obj <- 
    estimations[["estimation_results"]] %>% 
    huxtable::tidy_override(
      .,
      glance = 
        append(
          list(
            nobs = .$N[1],
            r.squared = .$r2[1]
          ),
          glance_add
        ),
      extend = TRUE
    ) 
  
  hux_obj$tidy_cols <- hux_obj$model
  
  hux_obj
}


#' fun_create_tidy_override_mlincom.R
create_tidy_override_mlincom <- function (estimations, glance_add = NULL) {
  hux_obj <- 
    estimations[["mlincom_results"]] %>% 
    huxtable::tidy_override(
      .,
      glance =  
        append(
          list(),
          glance_add
        ),
      extend = TRUE
    ) 
  
  hux_obj$tidy_cols <- hux_obj$model
  
  hux_obj
}


#' fun_extract_coefficient.R
extract_coefficient <- function (estimations, row_number, p_stars = TRUE) {
  coefficient <- estimations$estimate[row_number]
  p_value <- estimations$p.value[row_number]
  
  if (isTRUE(p_stars)) {
    stars <- dplyr::case_when(
      p_value <= .001 ~ " ***",
      p_value > .001 &  p_value <= .01 ~ " **",
      p_value > .01 &  p_value <= .05 ~ " *",
      p_value > .05 &  p_value <= .1 ~ " +"
    )
  } else {
    stars <- NULL
  }
  
  paste0(coefficient, stars)
}


#' fun_rotate_data.R
rotate_data <- function(data, x_add = 0, y_add = 0) {
  
  shear_matrix <- function (x) { 
    matrix(c(2, 1.2, 0, 1), 2, 2) 
  }
  
  rotate_matrix <- function(x) { 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  
  data %>% 
    dplyr::mutate(
      geometry = 
        .$geometry * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
    )
}


#' fun_bkg_geocode_single_address.R
bkg_geocode_single_address <- function (
  street,
  house_number,
  zip_code,
  place,
  epsg = "3035"
) {
  
  # create POST string for WFS service
  POST_request_string <- paste0(
    '<?xml version="1.0" encoding="UTF-8" ?>
<wfs:GetFeature version="1.1.0" service="WFS" maxFeatures="1"
        xmlns:wfs="http://www.opengis.net/wfs"
        xmlns:ogc="http://www.opengis.net/ogc"
        xmlns:gdz="http://www.geodatenzentrum.de/ortsangabe"
        xmlns:gml="http://www.opengis.net/gml"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <wfs:Query typeName="gdz:Ortsangabe" srsName="EPSG:', epsg, '">
    <ogc:Filter>
      <ogc:And>
        <ogc:PropertyIsLike escapeChar="\" wildCard="*" singleChar="?">
          <ogc:PropertyName>strasse</ogc:PropertyName>
          <ogc:Literal>', street, '</ogc:Literal>
        </ogc:PropertyIsLike>
        <ogc:PropertyIsLike escapeChar="\" wildCard="*" singleChar="?">
          <ogc:PropertyName>haus</ogc:PropertyName>
          <ogc:Literal>', house_number, '</ogc:Literal>
        </ogc:PropertyIsLike>
        <ogc:PropertyIsLike escapeChar="\" wildCard="*" singleChar="?">
          <ogc:PropertyName>plz</ogc:PropertyName>
          <ogc:Literal>', zip_code, '</ogc:Literal>
        </ogc:PropertyIsLike>
        <ogc:PropertyIsLike escapeChar="\" wildCard="*" singleChar="?">
          <ogc:PropertyName>ort</ogc:PropertyName>
          <ogc:Literal>', place, '</ogc:Literal>
        </ogc:PropertyIsLike>
      </ogc:And>
    </ogc:Filter>
  </wfs:Query>
</wfs:GetFeature>'
  )
  
  # conduct actual request
  POST_request <- httr::POST(
    'https://sg.geodatenzentrum.de/wfs_geokodierung_bund?outputformat=json',
    body = POST_request_string
  )
  POST_sf <-
    sf::read_sf(POST_request) %>%
    dplyr::select(-bbox)
  
  POST_sf$gc_x <-
    POST_sf %>%
    sf::st_coordinates() %>%
    .[1]
  
  POST_sf$gc_y <-
    POST_sf %>%
    sf::st_coordinates() %>%
    .[2]
  
  POST_sf <-
    POST_sf %>%
    dplyr::mutate_at(dplyr::vars(-geometry), as.character)
  
  # return object
  return(POST_sf)
}

bkg_geocode <- function (
  data = NULL,
  street,
  house_number,
  zip_code,
  place,
  epsg = "3035"
) {
  
  if (is.null(data)) {
    bkg_geocode_single_address(
      street = street,
      house_number = house_number,
      zip_code = zip_code,
      place = place,
      epsg = epsg
    )
  } else {
    lapply(1:nrow(data), function (i) {
      
      message(paste0("Turning to row ", i, " from ", nrow(data)))
      
      bkg_geocode_single_address(
        street = data[[street]][i],
        house_number = data[[house_number]][i],
        zip_code = data[[zip_code]][i],
        place = data[[place]][i],
        epsg = epsg
      )
      
    }) %>%
      dplyr::bind_rows(.) %>%
      dplyr::bind_cols(
        data,
        .
      ) %>%
      sf::st_sf(crs = eval(parse(text = epsg)), sf_column_name = "geometry")
  }
}


#' fun_spt_create_inspire_ids.R
spt_create_inspire_ids <- function(
  data,
  type = c("1km", "100m"),
  column_name = "Gitter_ID_",
  combine = FALSE
) {
  
  if (sf::st_crs(data)$epsg != 3035) {
    data <- data %>% sf::st_transform(3035)
  }
  
  coordinate_pairs <-
    data %>%
    sf::st_coordinates() %>%
    tibble::as_tibble()
  
  id_name <-
    glue::glue("{column_name}{type}")
  
  loop_to_evaluate <-
    dplyr::case_when(
      type == "1km" ~
        glue::glue(
          "1kmN{substr(coordinate_pairs$Y %>% as.character(), 1, 4)}",
          "E{substr(coordinate_pairs$X %>% as.character(), 1, 4)}"
        ) %>%
        as.character,
      type == "100m" ~
        glue::glue(
          "100mN{substr(coordinate_pairs$Y %>% as.character(), 1, 5)}",
          "E{substr(coordinate_pairs$X %>% as.character(), 1, 5)}"
        ) %>%
        as.character()
    )
  
  expression_to_evaluate <-
    rlang::expr(!!rlang::sym(id_name) <- loop_to_evaluate)
  
  eval(expression_to_evaluate)
  
  if (isTRUE(combine)) {
    return(
      dplyr::bind_cols(data, !!id_name := get(id_name))
    )
  } else {
    return(get(id_name))
  }
}


#' fun_spt_extract_inspire_coordinates.R
spt_extract_inspire_x <- function (inspire_ids) {
  
  if (stringr::str_detect(inspire_ids[1], "1km")) {
    inspire_x <-
      substr(inspire_ids, 10, 13) %>%
      paste0(., "500") %>%
      as.numeric()
  }
  
  if (stringr::str_detect(inspire_ids[1], "100m")) {
    inspire_x <-
      substr(inspire_ids, 12, 16) %>%
      paste0(., "50") %>%
      as.numeric()
  }
  
  return(inspire_x)
}


spt_extract_inspire_y <- function (inspire_ids) {
  
  if (stringr::str_detect(inspire_ids[1], "1km")) {
    inspire_y <-
      substr(inspire_ids, 5, 8) %>%
      paste0(., "500") %>%
      as.numeric()
  }
  
  if (stringr::str_detect(inspire_ids[1], "100m")) {
    inspire_y <-
      substr(inspire_ids, 6, 10) %>%
      paste0(., "50") %>%
      as.numeric()
  }
  
  return(inspire_y)
}


#' fun_spl_simple_join.R
spl_simple_join <- function(data, shape_file, attribute) {
  data %>%
    sf::st_join(., shape_file, join = st_intersects) %>%
    sf::st_drop_geometry() %>%
    .[attribute]
}


#' fun_spl_buffer.R
spl_buffer <-
  function(data,
           raster_file,
           buffer_size) {
    
    message(
      paste0(
        "Extracting buffer values of size ", 
        buffer_size, 
        " from ", 
        names(raster_file)
      )
    )
    
    # target_value <-
    raster::extract(
      raster_file,
      data,
      fun = mean,
      buffer = buffer_size,
      na.rm = TRUE,
      small = FALSE
    )
    
    # return value
    # return(target_value)
  }


#' fun-stata_regress_robust.R
stata_regress_robust <- function (
  dependent_variable,
  controls, 
  interaction_effects = NULL,
  data,
  clustervar = NULL,
  stata_names = NULL,
  vif = TRUE,
  stata_path = "./data_stata",
  echo = FALSE,
  stata_save = c("estimates", "coefficients")
) {
  
  if (length(controls) == 1) {
    variables <- controls[[1]]
    
    # specify possible interaction effect
    if (!is.null(interaction_effects)) {
      variables <- c(variables, interaction_effects %>% unlist())
    }
    
    # create model formula
    model_formula <- 
      as.formula(
        paste(
          dependent_variable, 
          paste(variables, collapse = " + "), 
          sep = " ~ "
        )
      )
    
    # run the model
    
    vif_call <-
      ifelse(isTRUE(vif), " \n vif", "")
    
    regression_call <- paste0(
      "regress ",
      r_to_stata_formula(model_formula, data_to_check = data),
      ", robust cluster(", clustervar, ")", 
      vif_call
    )
    
    savings <- 
      if(!is.null(stata_save)) {
        sapply(stata_save, function (to_save) {
          if (to_save == "estimates") {
            paste0(
              "estimates save ", stata_data_path, "estimates_", 
              stata_names, 
              ", replace \n"
            )
          } else if (to_save == "coefficients") {
            paste0(
              "regsave using ", stata_data_path, "coefficients_", 
              stata_names, 
              ", tstat pval ci replace"
            )
          }
        },
        USE.NAMES = FALSE)
      } else {
        ""
      }
    
    stata_src <- c(
      regression_call,
      savings
    )
    
  } else {
    
    dependent_variables_call <- 
      lapply(1:length(controls), function (i) {
        paste0(
          "clonevar ", dependent_variable, i, " = ", dependent_variable
        )
      })
    
    dependent_variables <- 
      lapply(1:length(controls), function (i) {
        glue::glue(
          "{dependent_variable}{i}"
        )
      })
    
    
    model_formulas <- 
      lapply(1:length(controls), function (i) {
        
        variables <- controls[[i]]
        
        # specify possible interaction effect
        if (!is.null(interaction_effects)) {
          variables <- c(variables, paste0(interaction_effects[[i]]))
        }
        
        # create model formula
        model_formula <-
          as.formula(
            paste(
              dependent_variables[[i]],
              paste(variables, collapse = " + "),
              sep = " ~ "
            )
          )
      })
    
    regression_call <- 
      lapply(1:length(controls), function (i) {
        glue::glue(
          "({r_to_stata_formula(model_formulas[[i]], data, gsem = TRUE)}) ///"
        )
      }) %>% 
      unlist() %>% 
      paste(collapse = "\n") %>% 
      paste0(
        "gsem ",. , "\n, nocapslatent vce(cluster ", clustervar, ")"
      )
    
    savings <- 
      if(!is.null(stata_save)) {
        sapply(stata_save, function (to_save) {
          if (to_save == "estimates") {
            paste0(
              "estimates save ", stata_data_path, "estimates_", 
              stata_names, 
              ", replace \n"
            )
          } else if (to_save == "coefficients") {
            paste0(
              "regsave using ", stata_data_path, "coefficients_", 
              stata_names, 
              ", tstat pval ci replace"
            )
          }
        },
        USE.NAMES = FALSE)
      } else {
        ""
      }
    
    stata_src <- c(
      dependent_variables_call,
      regression_call,
      savings
    ) %>% 
      unlist()
  }
  
  RStata::stata(
    stata_src,
    data.in = data,
    stata.echo = echo
  )
  
  # compile everything
  list(
    dependent_variable =
      dependent_variable,
    stata_paths = 
      c(
        paste0(stata_data_path, "estimates_", stata_names, ".ster"),
        paste0(stata_data_path, "coefficients_", stata_names, ".dta")
      ),
    estimation_results =
      if (length(controls) == 1) {
        haven::read_dta(
          paste0(
            stata_data_path, "coefficients_", stata_names, ".dta"
          )
        ) %>% 
          dplyr::transmute(
            term = var,
            estimate = coef,
            std.error = stderr,
            statistic = tstat,
            p.value = pval,
            conf.low = ci_lower,
            conf.high = ci_upper,
            N,
            r2
          )
      } else {
        haven::read_dta(
          paste0(
            stata_data_path, "coefficients_", stata_names, ".dta"
          )
        ) %>%
          dplyr::transmute(
            term = var,
            estimate = coef,
            std.error = stderr,
            statistic = tstat,
            p.value = pval,
            conf.low = ci_lower,
            conf.high = ci_upper,
            N
          )
      }
  )
}


#' fun-stata_margins.R
stata_margins <- function (
  estimates,
  variables = NULL,
  dydx = NULL,
  at = NULL,
  over = NULL,
  additional_options = NULL,
  mlincom = NULL,
  matsize = 600,
  echo = FALSE,
  stata_names = NULL,
  data
) {
  matsize_call <-
    paste0("set matsize ", matsize)
  
  use_estimates <- paste0(
    "estimates use ", 
    estimates$stata_paths[1]
  )
  
  set_esample <- "estimates esample:"
  
  dydx_call <-
    ifelse(!is.null(dydx), paste0(" dydx(", dydx, ")"), "")
  
  at_call <-
    ifelse(!is.null(at), paste0(" ", at), "")
  
  over_call <-
    ifelse(!is.null(over), paste0(" over(", over, ")"), "")
  
  additional_options_call <-
    ifelse(!is.null(additional_options), paste0(" ", additional_options), "")
  
  mtable <- paste0(
    "mtable,",
    dydx_call, 
    at_call, 
    over_call,
    additional_options_call
    , " stat(est se p ci) post"
  )
  
  regsave_mtable <- 
    paste0(
      'regsave using "', stata_data_path, 
      stata_names, 
      '_mtable", tstat pval ci replace'
    )
  
  mlincom_call <-
    ifelse(
      is.null(mlincom),
      "",
      paste0(
        "tempname hdle\n",
        "postfile `hdle' est se z p lb ub using ", stata_data_path,  
        stata_names, "_mlincom.dta, replace\n",
        lapply(mlincom, function (i) { 
          paste0(
            "mlincom ", i, ", stats(all)\n",
            "post `hdle' (r(est)) (r(se)) (r(z)) (r(p)) (r(lb)) (r(ub))\n"
          )
        }) %>%
          do.call(rbind, .) %>%
          paste(collapse = "\n"),
        "postclose `hdle'" 
      )
    )
  
  stata_src <- paste(
    matsize_call,
    use_estimates, 
    set_esample,
    mtable,
    regsave_mtable,
    mlincom_call,
    sep = " \n "
  )
  
  RStata::stata(stata_src, data.in = data, stata.echo = echo)
  
  # compile everything
  list(
    dydx = dydx,
    over = over,
    at = at,
    stata_path_mtable = 
      regsave_mtable %>% 
      gsub(".*[\"](.+)[\"].*", "\\1", .) %>% 
      paste0(., ".dta"),
    stata_path_mlincom =
      if (is.null(mlincom)) {
        mlincom
      } else {
        paste0(stata_data_path, stata_names, "_mlincom.dta")
      },
    estimation_results = 
      haven::read_dta(
        regsave_mtable %>% 
          gsub(".*[\"](.+)[\"].*", "\\1", .) %>% 
          paste0(., ".dta")
      ) %>% 
      dplyr::transmute(
        term = var,
        estimate = coef,
        std.error = stderr,
        statistic = tstat,
        p.value = pval,
        conf.low = ci_lower,
        conf.high = ci_upper
      ),
    mlincom_results =
      if (is.null(mlincom)) {
        mlincom
      } else {
        haven::read_dta(
          paste0(stata_data_path, stata_names, "_mlincom.dta")
        ) %>% 
          dplyr::transmute(
            term = mlincom,
            estimate = est,
            std.error = se,
            statistic = z,
            p.value = p,
            conf.low = lb,
            conf.high = ub
          )
      }
  )
}


#' ini-load_destatis_data.R
municipalties <-
  readr::read_csv2(
    "./data/31122017_Auszug_GV_manipulated.csv",
    col_types = cols(
      zip_code = col_character()
    )) %>% 
  dplyr::select(-contains("X")) %>% 
  dplyr::filter(!is.na(gem)) %>% 
  dplyr::mutate(
    RS = paste0(land, rb, kreis, vb, gem)
  )

municipalties_shapes <-
  sf::read_sf("./data/VG250_GEM.shp") %>% 
  sf::st_transform(3035) %>% 
  dplyr::left_join(
    municipalties %>% 
      dplyr::select(RS, zip_code),
    by = "RS"
  ) %>% 
  dplyr::mutate(
    RS_short = stringr::str_sub(RS, 1, 2) %>% as.numeric(),
    east = dplyr::if_else(RS_short <= 10, 0, 1) 
  )


#' ini-load_ioer_data.R
sealing_2015_100m <-
  raster::raster("./data/S40RG_2015_100m.tif") %>% 
  raster::readAll()

green_2016_100m <-
  raster::raster("./data/F01RG_2016_100m.tif") +
  raster::raster("./data/S08RG_2016_100m.tif") %>% 
  raster::readAll()

green_2018_100m <-
  raster::raster("./data/F01RG_2018_100m.tif") +
  raster::raster("./data/S08RG_2018_100m.tif") %>% 
  raster::readAll()

water_2016_100m <-
  raster::raster("./data/F11RG_2016_100m.tif") %>% 
  raster::readAll()

water_2018_100m <-
  raster::raster("./data/F11RG_2018_100m.tif") %>% 
  raster::readAll()

urban_green_2013_100m <- 
  raster::raster("./data/O01RG_2013_100m.tif") %>% 
  raster::readAll()

street_minus_ecotone_2016_100m <-
  raster::raster("./data/V02RG_2016_100m.tif") -
  raster::raster("./data/U30DG_2016_100m.tif")

street_minus_ecotone_2016_100m[street_minus_ecotone_2016_100m < 0] <- 0

street_minus_ecotone_2018_100m <-
  raster::raster("./data/V02RG_2018_100m.tif") -
  raster::raster("./data/U30DG_2018_100m.tif") %>% 
  raster::readAll()

street_minus_ecotone_2018_100m[street_minus_ecotone_2018_100m < 0] <- 0


#' ini-create_map_data.R
dresden_shape <-
  sf::read_sf("./data/VG250_GEM.shp") %>% 
  sf::st_transform(3035) %>% 
  dplyr::filter(GEN == "Dresden")

poi <-
  bkg_geocode(
    street = "Palaisplatz",
    house_number = "1",
    zip_code = "01097",
    place = "Dresden"
  ) %>% 
  dplyr::mutate(
    inspire_100m = spt_create_inspire_ids(data = ., type = "100m"),
    inspire_x = spt_extract_inspire_x(inspire_100m),
    inspire_y = spt_extract_inspire_y(inspire_100m)
  ) %>% 
  sf::st_drop_geometry() %>% 
  sf::st_as_sf(
    coords = c("inspire_x", "inspire_y"),
    crs = 3035
  ) %>% 
  dplyr::select(id, geometry)

poi_buffer <-
  poi %>% 
  sf::st_buffer(500)

poi_bbox <-
  sf::st_bbox(
    c(
      xmin = sf::st_coordinates(poi)[1] - 1000,
      xmax = sf::st_coordinates(poi)[1] + 1000,
      ymax = sf::st_coordinates(poi)[2] + 1000,
      ymin = sf::st_coordinates(poi)[2] - 1000
    ),
    crs = sf::st_crs(3035)
  )

poi_roads <-
  osmdata::getbb("Dresden") %>% 
  osmdata::opq(timeout = 25*100) %>%
  osmdata::add_osm_feature(
    "highway", 
    c("trunk", "primary", "secondary", "tertiary")
  )%>% 
  osmdata::osmdata_sf() %>% 
  .$osm_lines %>% 
  dplyr::select(osm_id) %>% 
  sf::st_transform(3035) %>%
  sf::st_crop(poi_bbox)

poi_elbe <-
  osmdata::getbb("Dresden") %>% 
  osmdata::opq(timeout = 25*100) %>%
  osmdata::add_osm_feature("waterway", "river") %>% 
  osmdata::osmdata_sf() %>% 
  .$osm_lines %>% 
  dplyr::select(osm_id) %>% 
  sf::st_transform(3035) %>%
  sf::st_crop(poi_bbox) %>% 
  sf::st_buffer(50) %>% 
  sf::st_crop(poi_bbox) 

poi_buildings <-
  osmdata::getbb("Dresden") %>% 
  osmdata::opq(timeout = 25*100) %>%
  osmdata::add_osm_feature(
    "building", 
    c(
      "apartments", "commcerical", "office", "cathredral", "church", "retail",
      "industrial", "warehouse", "hotel", "house", "civic", "government", 
      "public", "parking", "garages", "carport", "transportation", 
      "semidetached_house", "school",
      "conservatory"
    )
  ) %>% 
  osmdata::osmdata_sf() %>% 
  .$osm_polygons %>%
  dplyr::select(osm_id) %>% 
  sf::st_transform(3035) %>%
  sf::st_crop(poi_bbox)

poi_sealing <-
  raster::raster("./data/S40RG_2015_100m.tif") %>% 
  raster::crop(., dresden_shape) %>% 
  raster::rasterToPolygons() %>%
  as(., "sf") %>% 
  sf::st_transform(3035) %>% 
  sf::st_crop(., poi_bbox) %>% 
  dplyr::mutate(S40RG_2015_100m = ifelse(S40RG_2015_100m > 100, 100, S40RG_2015_100m))

poi_green <-
  (
    raster::raster("./data/F01RG_2018_100m.tif") +
      raster::raster("./data/S08RG_2018_100m.tif")
  ) %>% 
  raster::crop(., dresden_shape) %>% 
  raster::rasterToPolygons() %>%
  as(., "sf") %>% 
  sf::st_transform(3035) %>% 
  sf::st_crop(., poi_bbox) %>% 
  dplyr::mutate(layer = ifelse(layer > 100, 100, layer))

poi_scalebar_data <- 
  (poi_bbox + c(1000, 0, 0, -1950)) %>% 
  sf::st_as_sfc() %>%
  sf::st_sf()

poi_scalebar_data <-
  rbind(
    poi_scalebar_data %>% 
      {sf::st_bbox(.) + c(750, 0, -0, 0)} %>% 
      sf::st_as_sfc() %>%
      sf::st_sf(),
    poi_scalebar_data %>% 
      {sf::st_bbox(.) + c(500, 0, -250, 0)} %>% 
      sf::st_as_sfc() %>%
      sf::st_sf(),
    poi_scalebar_data %>% 
      {sf::st_bbox(.) + c(250, 0, -500, 0)} %>% 
      sf::st_as_sfc() %>%
      sf::st_sf(),
    poi_scalebar_data %>% 
      {sf::st_bbox(.) + c(0, 0, -750, 0)} %>%
      sf::st_as_sfc() %>%
      sf::st_sf()
  )

poi_compass_data <- 
  (poi_bbox + c(2000, 1000, 0, 0)) %>% 
  sf::st_as_sfc() %>%
  sf::st_sf()


#' ini-load_ggss_geodata.R
ggss_geodata <-
  haven::read_dta("Y:/allbus_georef_tmp.dta") %>% 
  sjlabelled::remove_all_labels() %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(year == 2016 | year == 2018) %>% 
  dplyr::select(
    respid, year, inspidha
  ) %>% 
  dplyr::mutate(
    x = spt_extract_inspire_x(inspidha),
    y = spt_extract_inspire_y(inspidha)
  ) %>% 
  sf::st_as_sf(., coords = c("x", "y"), crs = 3035)


#' ini-load_ggss_survey_data.R
common_ggss_variables <- 
  c("respid", "year", "age", "sex", "educ", "hhinc", "dh04")

ggss_2016_2018 <-
  rbind(
    haven::read_sav("./data/ZA5250_v2-1-0.sav") %>% 
      dplyr::mutate(year = 2016, sample_point = paste0(xs11, "_16")) %>% 
      dplyr::select(
        common_ggss_variables,
        dn01 = dn01a, fdm01 = fdm01a, mdm01 = mdm01a,
        sample_point
      ) %>% 
      sjlabelled::remove_all_labels(),
    haven::read_sav("./data/ZA5270_v2-0-0.sav") %>% 
      dplyr::mutate(year = 2018, sample_point = paste0(xs11, "_18")) %>% 
      dplyr::select(
        dplyr::all_of(common_ggss_variables), dn01, fdm01, mdm01, sample_point
      ) %>% 
      sjlabelled::remove_all_labels()
  ) %>% 
  dplyr::transmute(
    respid,
    year,
    age  = age %>%
      as.numeric(),
    gender = 
      sex %>% 
      car::recode('1 = 0; 2 = 1') %>% 
      as.factor(),
    edu  =
      educ %>%
      car::recode('1:2 = 1; 3 = 2; 4:5 = 3; 6:7 = NA') %>%
      as.factor() %>% 
      forcats::fct_recode(low = "1", middle = "2", high = "3"),
    hhsize  = 
      dh04 %>%
      as.numeric(),
    hh_eq_income = (hhinc / sqrt(hhsize)) / 1000,
    sample_point,
    citizenship = dn01,
    country_father = fdm01,
    country_mother = mdm01
  )


#' ini-migration_variable.R
ggss_2016_2018 <-
  ggss_2016_2018 %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(
    migration_background_indicator =
      any(
        citizenship != 0,
        (country_father != 0 & country_father != 996),
        (country_mother != 0  & country_mother != 996)
      )
  ) %>%
  dplyr::mutate(
    migration_background =
      ifelse(
        isFALSE(migration_background_indicator),
        ifelse(
          citizenship == 0 &
            (country_father == 0 | country_father == 996) &
            (country_mother == 0 | country_mother == 996),
          "German",
          NA
        ),
        "Migrant"
      )
  ) %>% 
  dplyr::select(
    -citizenship, -country_father, -country_mother, 
    -migration_background_indicator
  ) %>% 
  ungroup()

ggss_2016_2018$migration_background <-
  ggss_2016_2018$migration_background %>%
  as.factor()


#' ini-spatial_linking.R
ggss_geodata_linked <-
  ggss_geodata %>% 
  dplyr::bind_cols(
    RS = { spl_simple_join(., municipalties_shapes, "RS") },
    east = { spl_simple_join(., municipalties_shapes, "east") },
    sealing_100 = { spl_buffer(., sealing_2015_100m, 25) },
    sealing_250 = { spl_buffer(., sealing_2015_100m, 250) },
    sealing_500 = { spl_buffer(., sealing_2015_100m, 500) },
    sealing_750 = { spl_buffer(., sealing_2015_100m, 750) },
    sealing_1000 = { spl_buffer(., sealing_2015_100m, 1000) },
    green_100 = {
      ifelse(
        .$year == "2016",
        spl_buffer(., green_2016_100m, 25),
        spl_buffer(., green_2018_100m, 25)
      )
    },
    green_250 = {
      ifelse(
        .$year == "2016",
        spl_buffer(., green_2016_100m, 250),
        spl_buffer(., green_2018_100m, 250)
      )
    },
    green_500 = {
      ifelse(
        .$year == "2016",
        spl_buffer(., green_2016_100m, 500),
        spl_buffer(., green_2018_100m, 500)
      )
    },
    green_750 = {
      ifelse(
        .$year == "2016",
        spl_buffer(., green_2016_100m, 750),
        spl_buffer(., green_2018_100m, 750)
      )
    },
    green_1000 = {
      ifelse(
        .$year == "2016",
        spl_buffer(., green_2016_100m, 1000),
        spl_buffer(., green_2018_100m, 1000)
      )
    },
    blue_500 = {
      ifelse(
        .$year == "2016",
        spl_buffer(., water_2016_100m, 500),
        spl_buffer(., water_2018_100m, 500)
      )
    },
    street_minus_ecotone_500 = {
      ifelse(
        .$year == "2016",
        spl_buffer(., street_minus_ecotone_2016_100m, 500),
        spl_buffer(., street_minus_ecotone_2018_100m, 500)
      )
    },
    urban_green_500 = { spl_buffer(., urban_green_2013_100m, 500) }
  ) %>% 
  dplyr::left_join(
    municipalties_shapes %>% 
      sf::st_drop_geometry() %>% 
      dplyr::select(RS, EWZ, zip_code),
    by = "RS"
  ) %>%
  
  dplyr::filter(EWZ != 0) %>% 
  
  # complete spatial linking
  tibble::as_tibble()

eval(
  parse(
    text = paste0(
      "ggss_geodata_linked$sealing <- ggss_geodata_linked$sealing_",
      default_buffer_size
    )
  )
)

eval(
  parse(
    text = paste0(
      "ggss_geodata_linked$green <- ggss_geodata_linked$green_",
      default_buffer_size
    )
  )
)


#' ini_city_centers.R
# requires access to services of bkg
zip_geocodes <-
  dplyr::left_join(
    ggss_geodata_linked %>% 
      dplyr::select(RS),
    municipalties_shapes %>% 
      sf::st_drop_geometry() %>% 
      dplyr::select(RS, zip_code, GEN),
    by = "RS"
  ) %>% 
  dplyr::distinct() %>% 
  bkg_geocode(
    data = .,
    street = "XXX",
    house_number = "XXX",
    zip_code = "zip_code",
    place = "GEN"
  ) %>% 
  dplyr::select(RS, GEN, geometry)

gem_distances <-
  dplyr::left_join(
    ggss_geodata_linked,
    zip_geocodes %>% 
      dplyr::rename(geometry_zip = geometry),
    by = "RS"
  ) %>% 
  dplyr::mutate(
    gem_distance = 
      sf::st_distance(
        geometry, geometry_zip, by_element = TRUE
      ) %>% 
      as.numeric()
  ) #%>% 
# dplyr::filter(gem_distance < 100000)

ggss_geodata_linked <-
  ggss_geodata_linked %>% 
  dplyr::left_join(
    gem_distances %>% 
      dplyr::select(respid, year, RS, gem_distance),
    by = c("respid", "year", "RS")
  ) %>% 
  dplyr::select(-inspidha, -geometry, -RS, -zip_code)


#' ini-link_spatial_survey.R
ggss_2016_2018_linked <-
  dplyr::left_join(
    ggss_2016_2018,
    ggss_geodata_linked %>% 
      dplyr::select(-urban_green_500), # remove urban green
    by = c("respid", "year")
  )

ggss_2016_2018_linked_urban_green <-
  dplyr::left_join(
    ggss_2016_2018,
    ggss_geodata_linked,
    by = c("respid", "year")
  )


#' ini-finalizing_sample_and_variables.R
dat <-
  ggss_2016_2018_linked %>% 
  dplyr::mutate(
    urban =       
      EWZ %>%
      car::recode(
        '0:99999 = 0; 100000:Inf = 1'
      )
  ) %>% 
  dplyr::select(-respid) %>% 
  na.omit()

dat$year <- as.factor(dat$year)
dat$east <- as.factor(dat$east)
dat$urban <- as.factor(dat$urban)
dat$sample_point <- as.factor(dat$sample_point)

# urban green subsample
dat_urban_green <-
  ggss_2016_2018_linked_urban_green %>% 
  dplyr::mutate(
    urban =       
      EWZ %>%
      car::recode(
        '0:99999 = 0; 100000:Inf = 1'
      )
  ) %>% 
  dplyr::select(-respid) %>% 
  na.omit()

dat_urban_green$year <- as.factor(dat_urban_green$year)
dat_urban_green$east <- as.factor(dat_urban_green$east)
dat_urban_green$urban <- as.factor(dat_urban_green$urban)
dat_urban_green$sample_point <- as.factor(dat_urban_green$sample_point)


#' ini-delete_sensitive_object.R
## rm(
##   list =
##     setdiff(
##       ls(),
##       c(
##         "dat", "dat_urban_green", "create_tidy_override", "r_to_stata_formula",
##         "stata_data_path", "stata_margins", "stata_regress_robust"
##       )
##     )
## )


#' ini_parameters.R
dependent_variables <- list(green = "green", sealing = "sealing")

common_control_variables <- 
  c(
    "age", "gender", "edu", "hhsize", "east", "year", "gem_distance", "EWZ",
    "migration_background"
  )

common_interaction_effects <- c("east:year")

control_variables =
  list(
    without_income = 
      common_control_variables,
    with_income =
      c(common_control_variables, "hh_eq_income", "I(hh_eq_income^2)"),
    with_income_interaction = 
      c(common_control_variables, "hh_eq_income", "I(hh_eq_income^2)")
  )

interaction_effects = 
  list(
    without_income =
      common_interaction_effects,
    with_income = 
      common_interaction_effects,
    with_income_interaction =
      c(
        common_interaction_effects,
        "migration_background:hh_eq_income",
        "migration_background:I(hh_eq_income^2)"
      )
  )

cluster_variable = "sample_point"

at_range <- seq(min(dat$hh_eq_income), max(dat$hh_eq_income), .1)

at_income <- 
  glue::glue(
    "at(hh_eq_income=({min(dat$hh_eq_income)}(.1){max(dat$hh_eq_income)}))"
  )


#' ana_estimations_sealing.R
estimation_sealing_without_income <- 
  stata_regress_robust(
    dependent_variable = dependent_variables$sealing,
    controls = list(control_variables$without_income),
    interaction_effects = list(interaction_effects$without_income),
    stata_names = "estimation_sealing_without_income",
    clustervar = cluster_variable,
    data = dat
  )

estimation_sealing_with_income <- 
  stata_regress_robust(
    dependent_variable = dependent_variables$sealing,
    controls = list(control_variables$with_income),
    interaction_effects = list(interaction_effects$with_income),
    stata_names = "estimation_sealing_with_income",
    clustervar = cluster_variable,
    data = dat
  )

estimation_sealing_with_income_interaction <- 
  stata_regress_robust(
    dependent_variable = dependent_variables$sealing,
    controls = list(control_variables$with_income_interaction),
    interaction_effects =
      list(interaction_effects$with_income_interaction),
    stata_names = "estimation_sealing_with_income_interaction",
    clustervar = cluster_variable,
    data = dat
  )

estimations_sealing_joined <-
  stata_regress_robust(
    dependent_variable = dependent_variables$sealing,
    controls = control_variables,
    interaction_effects = interaction_effects,
    stata_names = "estimations_sealing_joined",
    clustervar = cluster_variable,
    data = dat,
    echo = FALSE
  )


#' ana_predictions_sealing_migration_background.R
predictions_sealing_migration_background <-
  dplyr::bind_rows(
    stata_margins(
      estimates = estimation_sealing_without_income,
      over = "migration_background", 
      stata_names = "tmp",
      data = dat
    ) %>% 
      .$estimation_results %>% 
      dplyr::mutate(
        model = "sealing_without_income",
        whom = c("German", "Migrant")
      ),
    stata_margins(
      estimates = estimation_sealing_with_income,
      over = "migration_background", 
      stata_names = "tmp",
      data = dat
    ) %>% 
      .$estimation_results %>% 
      dplyr::mutate(
        model = "sealing_with_income",
        whom = c("German", "Migrant")
      ),
    stata_margins(
      estimates = estimation_sealing_with_income_interaction,
      over = "migration_background", 
      stata_names = "tmp",
      data = dat
    ) %>% 
      .$estimation_results %>% 
      dplyr::mutate(
        model = "sealing_with_income_interaction",
        whom = c("German", "Migrant")
      )
  )


#' ana_margins_sealing_migration_background.R
margins_sealing_migration_background <- 
  stata_margins(
    estimates = estimations_sealing_joined, 
    dydx = "migration_background", 
    matsize = 2400,
    stata_names = "tmp",
    mlincom = c("(2 - 1)", "(3 - 2)", "(3 - 1)"),
    data = dat %>% 
      dplyr::mutate(
        sealing1 = sealing, # sealing_without_income
        sealing2 = sealing, # sealing_with_income
        sealing3 = sealing, # sealing_with_income_interaction
      )
  ) 


#' ana_prediction_sealing_income_nonlinear_interaction.R
prediction_sealing_with_income_interaction  <-
  stata_margins(
    estimates = estimation_sealing_with_income_interaction,  
    over = "migration_background", 
    at = at_income,
    matsize = 2400,
    stata_names = "prediction_sealing_with_income_interaction",
    data = dat
  ) %>% 
  .$estimation_results %>% 
  dplyr::mutate(
    x = rep(at_range, each = 2),
    whom = rep(c("German", "Migrant"), times = length(at_range))
  ) %>% 
  dplyr::arrange(whom)

# add German data
prediction_sealing_with_income_interaction <-
  dplyr::bind_cols(
    prediction_sealing_with_income_interaction,
    coef_germ =
      c(rep(NA, length(at_range)),
        rep(
          prediction_sealing_with_income_interaction %>%
            dplyr::filter(whom == "German") %>%
            .$estimate, 1)
      ),
    lower_germ =
      c(rep(NA, length(at_range)),
        rep(
          prediction_sealing_with_income_interaction %>%
            dplyr::filter(whom == "German") %>%
            .$conf.low, 1)
      ),
    upper_germ =
      c(rep(NA, length(at_range)),
        rep(
          prediction_sealing_with_income_interaction %>%
            dplyr::filter(whom == "German") %>%
            .$conf.high, 1)
      )
  )


#' ana_AME_sealing_with_income_nonlinear_interaction.R
AME_sealing_with_income_interaction <- 
  stata_margins(
    estimates = estimation_sealing_with_income_interaction, 
    dydx = "migration_background", 
    at = at_income,
    matsize = 2400,
    stata_names = "AME_sealing_with_income_interaction",
    data = dat
  ) %>% 
  .$estimation_results %>% 
  dplyr::slice(-c(1:length(at_range))) %>% 
  dplyr::mutate(x = at_range)


#' ana-second_differences_sealing.R
second_differences_sealing <- 
  stata_margins(
    estimates = estimation_sealing_with_income_interaction, 
    over = "migration_background",
    at =  "at(hh_eq_income=(1)) at(hh_eq_income=(4))",
    mlincom = c("(3 - 1)", "(4 - 2)", "(3 - 1) - (4 - 2)"),
    matsize = 2400,
    stata_names = "second_differences_sealing",
    echo = FALSE,
    data = dat
  )

second_differences_dydx_sealing <- 
  stata_margins(
    estimates = estimation_sealing_with_income_interaction, 
    dydx = "hh_eq_income",
    over = "migration_background",
    mlincom = c("(2 - 1)"),
    matsize = 2400,
    stata_names = "second_differences_sealing",
    echo = FALSE,
    data = dat
  )


#' ana_estimations_green.R
estimation_green_without_income <- 
  stata_regress_robust(
    dependent_variable = dependent_variables$green,
    controls = list(control_variables$without_income),
    interaction_effects = list(interaction_effects$without_income),
    stata_names = "estimation_green_without_income",
    clustervar = cluster_variable,
    data = dat
  )

estimation_green_with_income <- 
  stata_regress_robust(
    dependent_variable = dependent_variables$green,
    controls = list(control_variables$with_income),
    interaction_effects = list(interaction_effects$with_income),
    stata_names = "estimation_green_with_income",
    clustervar = cluster_variable,
    data = dat
  )

estimation_green_with_income_interaction <- 
  stata_regress_robust(
    dependent_variable = dependent_variables$green,
    controls = list(control_variables$with_income_interaction),
    interaction_effects =
      list(interaction_effects$with_income_interaction),
    stata_names = "estimation_green_with_income_interaction",
    clustervar = cluster_variable,
    data = dat
  )

estimations_green_joined <-
  stata_regress_robust(
    dependent_variable = dependent_variables$green,
    controls = control_variables,
    interaction_effects = interaction_effects,
    stata_names = "estimations_green_joined",
    clustervar = cluster_variable,
    data = dat,
    echo = FALSE
  )


#' ana_predictions_green_migration_background.R
predictions_green_migration_background <-
  dplyr::bind_rows(
    stata_margins(
      estimates = estimation_green_without_income,
      over = "migration_background", 
      stata_names = "tmp",
      data = dat
    ) %>% 
      .$estimation_results %>% 
      dplyr::mutate(
        model = "green_without_income",
        whom = c("German", "Migrant")
      ),
    stata_margins(
      estimates = estimation_green_with_income,
      over = "migration_background", 
      stata_names = "tmp",
      data = dat
    ) %>% 
      .$estimation_results %>% 
      dplyr::mutate(
        model = "green_with_income",
        whom = c("German", "Migrant")
      ),
    stata_margins(
      estimates = estimation_green_with_income_interaction,
      over = "migration_background", 
      stata_names = "tmp",
      data = dat
    ) %>% 
      .$estimation_results %>% 
      dplyr::mutate(
        model = "green_with_income_interaction",
        whom = c("German", "Migrant")
      )
  )


#' ana_margins_green_migration_background.R
margins_green_migration_background <- 
  stata_margins(
    estimates = estimations_green_joined, 
    dydx = "migration_background", 
    matsize = 2400,
    stata_names = "tmp",
    mlincom = c("(2 - 1)", "(3 - 2)", "(3 - 1)"),
    data = dat %>% 
      dplyr::mutate(
        green1 = green, # green_without_income
        green2 = green, # green_with_income
        green3 = green, # green_with_income_interaction
      )
  ) 


#' ana_prediction_green_income_nonlinear_interaction.R
prediction_green_with_income_interaction  <-
  stata_margins(
    estimates = estimation_green_with_income_interaction, 
    over = "migration_background", 
    at = at_income,
    matsize = 2400,
    stata_names = "prediction_green_with_income_interaction",
    data = dat
  ) %>% 
  .$estimation_results %>% 
  dplyr::mutate(
    x = rep(at_range, each = 2),
    whom = rep(c("German", "Migrant"), times = length(at_range))
  ) %>% 
  dplyr::arrange(whom)

# add German data
prediction_green_with_income_interaction <-
  dplyr::bind_cols(
    prediction_green_with_income_interaction,
    coef_germ =
      c(rep(NA, length(at_range)),
        rep(
          prediction_green_with_income_interaction %>%
            dplyr::filter(whom == "German") %>%
            .$estimate, 1)
      ),
    lower_germ =
      c(rep(NA, length(at_range)),
        rep(
          prediction_green_with_income_interaction %>%
            dplyr::filter(whom == "German") %>%
            .$conf.low, 1)
      ),
    upper_germ =
      c(rep(NA, length(at_range)),
        rep(
          prediction_green_with_income_interaction %>%
            dplyr::filter(whom == "German") %>%
            .$conf.high, 1)
      )
  )


#' ana_AME_green_with_income_nonlinear_interaction.R
AME_green_with_income_interaction <- 
  stata_margins(
    estimates = estimation_green_with_income_interaction, 
    dydx = "migration_background", 
    at = at_income,
    matsize = 2400,
    stata_names = "AME_green_with_income_interaction",
    echo = FALSE,
    data = dat
  ) %>% 
  .$estimation_results %>% 
  dplyr::slice(-c(1:length(at_range))) %>% 
  dplyr::mutate(x = at_range)


#' ana-second_differences_green.R
second_differences_green <- 
  stata_margins(
    estimates = estimation_green_with_income_interaction, 
    over = "migration_background",
    at =  "at(hh_eq_income=(1)) at(hh_eq_income=(4))",
    mlincom = c("(3 - 1)", "(4 - 2)", "(3 - 1) - (4 - 2)"),
    matsize = 2400,
    stata_names = "second_differences_green",
    echo = FALSE,
    data = dat
  )

second_differences_dydx_green <- 
  stata_margins(
    estimates = estimation_green_with_income_interaction, 
    dydx = "hh_eq_income",
    over = "migration_background",
    mlincom = c("(2 - 1)"),
    matsize = 2400,
    stata_names = "second_differences_green",
    echo = FALSE,
    data = dat
  )


#' ana_estimation_urban_sealing.R
estimation_urban_sealing <- 
  stata_regress_robust(
    dependent_variable = dependent_variables$sealing,
    controls = list(
      c(
        control_variables$with_income_interaction,
        "urban"
      )
    ),
    interaction_effects = list(
      c(
        "east:year", 
        "urban:migration_background",
        "urban:hh_eq_income",
        "urban:I(hh_eq_income^2)",
        "migration_background:hh_eq_income",
        "migration_background:I(hh_eq_income^2)",
        "urban:migration_background:hh_eq_income",
        "urban:migration_background:I(hh_eq_income^2)"
      )
    ),
    stata_names = "estimation_urban_sealing",
    clustervar = "sample_point",
    echo = FALSE,
    data = dat
  )


#' ana_estimation_urban_no_interaction_sealing.R
estimation_urban_no_interaction_sealing <- 
  stata_regress_robust(
    dependent_variable = dependent_variables$sealing,
    controls = list(
      c(
        control_variables$with_income_interaction,
        "urban"
      )
    ),
    interaction_effects = list(
      c(
        "east:year", 
        "urban:migration_background"
      )
    ),
    stata_names = "estimation_urban_no_interaction_sealing",
    clustervar = "sample_point",
    echo = FALSE,
    data = dat
  )


#' ana_prediction_urban_sealing.R
prediction_urban_sealing <- 
  stata_margins(
    estimates = estimation_urban_sealing, 
    over = "migration_background", 
    at = 
      glue::glue(
        "at(hh_eq_income=({glue::glue_collapse(at_range, sep = ' ')}) urban=(1 2))"
      ),
    matsize = 4800,
    stata_names = "prediction_urban_sealing",
    echo = FALSE,
    data = dat
  ) %>% 
  .$estimation_results %>% 
  dplyr::mutate(
    x = rep(at_range, each = 4),
    whom = rep(c("German", "Migrant"), times = length(at_range) * 2),
    where = rep(c("Non-Urban", "Urban"), each = 2, times = length(at_range))
  ) %>% 
  dplyr::arrange(whom, where)

# add German data
prediction_urban_sealing <-
  dplyr::bind_cols(
    prediction_urban_sealing,
    coef_germ =
      c(rep(NA, length(at_range) * 2),
        rep(
          prediction_urban_sealing %>%
            dplyr::filter(whom == "German") %>%
            .$estimate, 1)
      ),
    lower_germ =
      c(rep(NA, length(at_range) * 2),
        rep(
          prediction_urban_sealing %>%
            dplyr::filter(whom == "German") %>%
            .$conf.low, 1)
      ),
    upper_germ =
      c(rep(NA, length(at_range) * 2),
        rep(
          prediction_urban_sealing %>%
            dplyr::filter(whom == "German") %>%
            .$conf.high, 1)
      )
  )


#' ana_AME_urban_sealing.R
AME_urban_sealing <- 
  stata_margins(
    estimates = estimation_urban_sealing, 
    dydx = "migration_background", 
    at = 
      glue::glue(
        "at(hh_eq_income=({glue::glue_collapse(at_range, sep = ' ')}) urban=(1 2))"
      ),
    matsize = 4800,
    stata_names = "estimation_urban_sealing",
    data = dat
  ) %>% 
  .$estimation_results %>% 
  dplyr::slice(-c(1:(length(at_range) * 2))) %>%
  dplyr::mutate(
    x = rep(at_range, each = 2),
    where = rep(c("Non-Urban", "Urban"), times = length(at_range))
  )


#' ana_AME_urban_no_interaction_sealing.R
AME_urban_no_interaction_sealing <- 
  stata_margins(
    estimates = estimation_urban_no_interaction_sealing, 
    dydx = "migration_background", 
    at = 
      glue::glue(
        "at(urban=(1 2))"
      ),
    matsize = 4800,
    stata_names = "estimation_urban_no_interaction_sealing",
    data = dat
  ) %>% 
  .$estimation_results %>% 
  dplyr::slice(3:4) %>%
  dplyr::mutate(where = c("Non-Urban", "Urban"))


#' ana_second_differences_dydx_urban_sealing.R
second_differences_dydx_urban_sealing <- 
  stata_margins(
    estimates = estimation_urban_sealing, 
    # dydx = "migration_background", 
    dydx = "hh_eq_income",
    over = "migration_background",
    at =
      "at(urban=(1 2))",
    mlincom = c("(2 - 1)", "(4 - 3)", "(4 - 3) - (2 - 1)"),
    echo = FALSE,
    matsize = 2400,
    stata_names = "second_differences_urban_sealing",
    data = dat
  )

second_differences_dydx_urban_sealing_default <- 
  stata_margins(
    estimates = estimation_urban_sealing, 
    dydx = "hh_eq_income",
    over = "migration_background",
    mlincom = c("(2 - 1)"),
    matsize = 2400,
    stata_names = "second_differences_sealing_default",
    echo = FALSE,
    data = dat
  )


#' ana-second-differences-dydx-urban-no-interaction-sealing.R
second_differences_dydx_urban_sealing_no_interaction <- 
  stata_margins(
    estimates = estimation_urban_no_interaction_sealing, 
    dydx = "migration_background",
    # over = "migration_background",
    mlincom = c("(2 - 1)"),
    matsize = 2400,
    stata_names = "second_differences_sealing_no_interaction",
    echo = FALSE,
    data = dat
  )


#' ana_estimation_urban_green.R
estimation_urban_green <- 
  stata_regress_robust(
    dependent_variable = dependent_variables$green,
    controls = list(
      c(
        control_variables$with_income_interaction,
        "urban"
      )
    ),
    interaction_effects = list(
      c(
        "east:year", 
        "urban:migration_background",
        "urban:hh_eq_income",
        "urban:I(hh_eq_income^2)",
        "migration_background:hh_eq_income",
        "migration_background:I(hh_eq_income^2)",
        "urban:migration_background:hh_eq_income",
        "urban:migration_background:I(hh_eq_income^2)"
      )
    ),
    stata_names = "estimation_urban_green",
    clustervar = "sample_point",
    echo = FALSE,
    data = dat
  )


#' ana_estimation_urban_no_interaction_green.R
estimation_urban_no_interaction_green <- 
  stata_regress_robust(
    dependent_variable = dependent_variables$green,
    controls = list(
      c(
        control_variables$with_income_interaction,
        "urban"
      )
    ),
    interaction_effects = list(
      c(
        "east:year", 
        "urban:migration_background"
      )
    ),
    stata_names = "estimation_urban_no_interaction_green",
    clustervar = "sample_point",
    echo = FALSE,
    data = dat
  )


#' ana_prediction_urban_green.R
prediction_urban_green <- 
  stata_margins(
    estimates = estimation_urban_green, 
    over = "migration_background", 
    at = 
      glue::glue(
        "at(hh_eq_income=({glue::glue_collapse(at_range, sep = ' ')}) urban=(1 2))"
      ),
    matsize = 4800,
    stata_names = "prediction_urban_green",
    data = dat
  ) %>% 
  .$estimation_results %>% 
  dplyr::mutate(
    x = rep(at_range, each = 4),
    whom = rep(c("German", "Migrant"), times = length(at_range) * 2),
    where = rep(c("Non-Urban", "Urban"), each = 2, times = length(at_range))
  ) %>% 
  dplyr::arrange(whom, where)

# add German data
prediction_urban_green <-
  dplyr::bind_cols(
    prediction_urban_green,
    coef_germ =
      c(rep(NA, length(at_range) * 2),
        rep(
          prediction_urban_green %>%
            dplyr::filter(whom == "German") %>%
            .$estimate, 1)
      ),
    lower_germ =
      c(rep(NA, length(at_range) * 2),
        rep(
          prediction_urban_green %>%
            dplyr::filter(whom == "German") %>%
            .$conf.low, 1)
      ),
    upper_germ =
      c(rep(NA, length(at_range) * 2),
        rep(
          prediction_urban_green %>%
            dplyr::filter(whom == "German") %>%
            .$conf.high, 1)
      )
  )


#' ana_AME_urban_green.R
AME_urban_green <- 
  stata_margins(
    estimates = estimation_urban_green, 
    dydx = "migration_background", 
    at = 
      glue::glue(
        "at(hh_eq_income=({glue::glue_collapse(at_range, sep = ' ')}) urban=(1 2))"
      ),
    matsize = 4800,
    echo = FALSE,
    stata_names = "estimation_urban_green",
    data = dat
  ) %>% 
  .$estimation_results %>% 
  dplyr::slice(-c(1:(length(at_range) * 2))) %>% 
  dplyr::mutate(
    x = rep(at_range, each = 2),
    where = rep(c("Non-Urban", "Urban"), times = length(at_range))
  )


#' ana_AME_urban_no_interaction_green.R
AME_urban_no_interaction_green <- 
  stata_margins(
    estimates = estimation_urban_no_interaction_green, 
    dydx = "migration_background", 
    at = 
      glue::glue(
        "at(urban=(1 2))"
      ),
    matsize = 4800,
    stata_names = "estimation_urban_no_interaction_green",
    data = dat
  ) %>% 
  .$estimation_results %>% 
  dplyr::slice(3:4) %>%
  dplyr::mutate(where = c("Non-Urban", "Urban"))


#' ana_second_differences_urban_green.R
second_differences_dydx_urban_green <- 
  stata_margins(
    estimates = estimation_urban_green, 
    dydx = "hh_eq_income", 
    over = "migration_background",
    at =
      "at(urban=(1 2))",
    mlincom = c("(2 - 1)", "(4 - 3)", "(4 - 3) - (2 - 1)"),
    echo = FALSE,
    matsize = 2400,
    stata_names = "second_differences_urban_green",
    data = dat
  )

second_differences_dydx_urban_green_default <- 
  stata_margins(
    estimates = estimation_urban_green, 
    dydx = "hh_eq_income",
    over = "migration_background",
    mlincom = c("(2 - 1)"),
    matsize = 2400,
    stata_names = "second_differences_green_default",
    echo = FALSE,
    data = dat
  )


#' ana-second-differences-dydx-urban-no-interaction-green.R
second_differences_dydx_urban_green_no_interaction <- 
  stata_margins(
    estimates = estimation_urban_no_interaction_green, 
    dydx = "migration_background",
    # over = "migration_background",
    mlincom = c("(2 - 1)"),
    matsize = 2400,
    stata_names = "second_differences_green_no_interaction",
    echo = FALSE,
    data = dat
  )


#' tab_descriptives.R
dat_german <-
  dat %>% 
  dplyr::filter(migration_background == "German")

dat_migrant <-
  dat %>% 
  dplyr::filter(migration_background == "Migrant")

descriptives <- 
  rbind(
    c(
      mean(dat_german$sealing), 
      sd(dat_german$sealing), 
      censor_variable(dat_german, sealing), 
      censor_variable(dat_german, sealing, descending = TRUE)
    ),
    c(
      mean(dat_german$green), 
      sd(dat_german$green), 
      censor_variable(dat_german, green), 
      censor_variable(dat_german, green, descending = TRUE)
    ),
    c(mean(dat_german[["hh_eq_income"]]), 
      sd(dat_german[["hh_eq_income"]]), 
      min(dat_german[["hh_eq_income"]]), 
      max(dat_german[["hh_eq_income"]])),
    c(mean(dat_german[["age"]], na.rm = TRUE), 
      sd(dat_german[["age"]], na.rm = TRUE),
      min(dat_german[["age"]], na.rm = TRUE), 
      max(dat_german[["age"]], na.rm = TRUE)),
    c(
      mean(dat_german$gender %>% as.numeric()) - 1,
      sd(dat_german$gender %>% as.numeric()), 
      min(dat_german$gender %>% as.numeric()) - 1, 
      max(dat_german$gender %>% as.numeric()) - 1
    ),
    rep(NA, 4),
    cbind(
      prop.table(table(dat_german$edu) %>% 
                   as.vector(.)) * 100, 
      c(rep(NA, 3)), 
      c(rep(NA, 3)),
      c(rep(NA, 3))
    ),
    c(mean(dat_german[["hhsize"]], na.rm = TRUE), 
      sd(dat_german[["hhsize"]], na.rm = TRUE),
      min(dat_german[["hhsize"]], na.rm = TRUE), 
      max(dat_german[["hhsize"]], na.rm = TRUE)),
    c(
      mean(dat_german$east %>% as.numeric()) - 1,
      sd(dat_german$east %>% as.numeric()), 
      min(dat_german$east %>% as.numeric()) - 1, 
      max(dat_german$east %>% as.numeric()) - 1
    ),
    rep(NA, 4),
    cbind(
      prop.table(table(dat_german$year) %>% 
                   as.vector(.)) * 100, 
      c(rep(NA, 2)), 
      c(rep(NA, 2)), 
      c(rep(NA, 2))),
    c(
      mean(dat_german$EWZ), 
      sd(dat_german$EWZ), 
      1000, 
      500000
    ),
    c(
      mean(dat_german$urban %>% as.numeric()) - 1,
      sd(dat_german$urban %>% as.numeric()), 
      min(dat_german$urban %>% as.numeric()) - 1, 
      max(dat_german$urban %>% as.numeric()) - 1
    ),
    c(
      mean(dat_german[["gem_distance"]], na.rm = TRUE), 
      sd(dat_german[["gem_distance"]], na.rm = TRUE),
      censor_variable(dat_german, gem_distance), 
      10000
    ),
    c(nrow(dat_german), NA, NA, NA)
  ) %>% 
  cbind(
    rbind(
      c(
        mean(dat_migrant$sealing), 
        sd(dat_migrant$sealing), 
        censor_variable(dat_migrant, sealing), 
        censor_variable(dat_migrant, sealing, descending = TRUE)
      ),
      c(
        mean(dat_migrant$green), 
        sd(dat_migrant$green), 
        censor_variable(dat_migrant, green), 
        censor_variable(dat_migrant, green, descending = TRUE)
      ),
      c(mean(dat_migrant[["hh_eq_income"]]), 
        sd(dat_migrant[["hh_eq_income"]]), 
        min(dat_migrant[["hh_eq_income"]]), 
        max(dat_migrant[["hh_eq_income"]])),
      c(mean(dat_migrant[["age"]], na.rm = TRUE), 
        sd(dat_migrant[["age"]], na.rm = TRUE),
        min(dat_migrant[["age"]], na.rm = TRUE), 
        max(dat_migrant[["age"]], na.rm = TRUE)),
      c(
        mean(dat_migrant$gender %>% as.numeric()) - 1,
        sd(dat_migrant$gender %>% as.numeric()), 
        min(dat_migrant$gender %>% as.numeric()) - 1, 
        max(dat_migrant$gender %>% as.numeric()) - 1
      ),
      rep(NA, 4),
      cbind(
        prop.table(table(dat_migrant$edu) %>% 
                     as.vector(.)) * 100, 
        c(rep(NA, 3)), 
        c(rep(NA, 3)),
        c(rep(NA, 3))
      ),
      c(mean(dat_migrant[["hhsize"]], na.rm = TRUE), 
        sd(dat_migrant[["hhsize"]], na.rm = TRUE),
        min(dat_migrant[["hhsize"]], na.rm = TRUE), 
        max(dat_migrant[["hhsize"]], na.rm = TRUE)),
      c(
        mean(dat_migrant$east %>% as.numeric()) - 1,
        sd(dat_migrant$east %>% as.numeric()), 
        min(dat_migrant$east %>% as.numeric()) - 1, 
        max(dat_migrant$east %>% as.numeric()) - 1
      ),
      rep(NA, 4),
      cbind(
        prop.table(table(dat_migrant$year) %>% 
                     as.vector(.)) * 100, 
        c(rep(NA, 2)), 
        c(rep(NA, 2)), 
        c(rep(NA, 2))),
      c(
        mean(dat_migrant$EWZ), 
        sd(dat_migrant$EWZ), 
        1000, 
        500000
      ),
      c(
        mean(dat_migrant$urban %>% as.numeric()) - 1,
        sd(dat_migrant$urban %>% as.numeric()), 
        min(dat_migrant$urban %>% as.numeric()) - 1, 
        max(dat_migrant$urban %>% as.numeric()) - 1
      ),
      c(
        mean(dat_migrant[["gem_distance"]], na.rm = TRUE), 
        sd(dat_migrant[["gem_distance"]], na.rm = TRUE),
        censor_variable(dat_migrant, gem_distance), 
        10000
      ),
      c(nrow(dat_migrant), NA, NA, NA)
    )
  ) %>% 
  magrittr::set_rownames(
    c(
      "Soil Sealing (500m Buffer)*",
      "Green Spaces (500m Buffer)*",
      "Income",
      "Age",
      "Gender (Female)",
      "Education:",
      "Low",
      "Medium",
      "High",
      "Household Size",
      "Region (Eastern Germany)",
      "Survey Year:",
      "2014",
      "2016",
      "Municipality Inhabitants*",
      "Urbanity (Urban)",
      "Distance City Center*",
      "Number of Respondents"
    )
  ) %>%
  magrittr::set_colnames(rep(c("Mean / N / %", "SD", "Minimum", "Maximum"), 2))

descriptives_html <-
  htmlTable::htmlTable(
    htmlTable::txtRound(descriptives, 3),
    tfoot="* Minimum and maximum values censored due to data protection",
    cgroup = c("Germans", "Migrants"),
    n.cgroup = c(4, 4),
    rgroup = c("Main Variables", "Individual Controls", "Inter-Individual Controls"),
    n.rgroup = c(3, 7),
    total = TRUE
  )

htmltools::save_html(
  descriptives_html,
  "./revise_and_resubmit_2/tables/Table 1.html"
)


#' tab-regressions.R
tab_regressions <- 
  huxtable::huxreg(
    "Without Income" = 
      create_tidy_override(
        estimation_sealing_without_income,
        glance_add = c(
          "AME" = 
            extract_coefficient(
              margins_sealing_migration_background$estimation_results, 
              4
            ),
          "AME_diff" = ""
        ) 
      ),
    "With Income" = 
      create_tidy_override(
        estimation_sealing_with_income,
        glance_add = c(
          "AME" = 
            extract_coefficient(
              margins_sealing_migration_background$estimation_results, 
              5
            ),
          "AME_diff" = 
            extract_coefficient(
              margins_sealing_migration_background$mlincom_results, 
              1
            )
        )
      ),
    "Income Interaction" = 
      create_tidy_override(
        estimation_sealing_with_income_interaction,
        glance_add = c(
          "AME" = 
            extract_coefficient(
              margins_sealing_migration_background$estimation_results, 
              6
            ),
          "AME_diff" = 
            extract_coefficient(
              margins_sealing_migration_background$mlincom_results, 
              2
            )
        )
      ),
    "Without Income" = 
      create_tidy_override(
        estimation_green_without_income,
        glance_add = c(
          "AME" = 
            extract_coefficient(
              margins_green_migration_background$estimation_results, 
              4
            ),
          "AME_diff" = ""
        )
      ),
    "With Income" = 
      create_tidy_override(
        estimation_green_with_income,
        glance_add = c(
          "AME" = 
            extract_coefficient(
              margins_green_migration_background$estimation_results, 
              5
            ),
          "AME_diff" = 
            extract_coefficient(
              margins_green_migration_background$mlincom_results, 
              1
            )
        )
      ),
    "Income Interaction" = 
      create_tidy_override(
        estimation_green_with_income_interaction,
        glance_add = c(
          "AME" = 
            extract_coefficient(
              margins_green_migration_background$estimation_results, 
              6
            ),
          "AME_diff" = 
            extract_coefficient(
              margins_green_migration_background$mlincom_results, 
              2
            )
        )
      ),
    statistics = c(
      "AME Migration Background" = "AME", 
      "Difference Previous Model" = "AME_diff", 
      "N" = "nobs", 
      "R²" = "r.squared"
    ),
    stars = c(`***` = 0.001, `**` =  0.01, `*` = 0.05, `+` = 0.1),
    coefs =
      c(
        "Migration Background" = "2.migration_background",
        "Income" = "hh_eq_income",
        "Income²" = "c.hh_eq_income#c.hh_eq_income",
        "Migration Background X Income" =
          "2.migration_background#c.hh_eq_income",
        "Migration Background X Income²" =
          "2.migration_background#c.hh_eq_income#c.hh_eq_income"
      ),
    note = "{stars}; Data source: GGSS 2014 & 2016 (ZA5250, ZA5270) and IOER Monitor; unstandardized estimates are based on cluster robust standard errors (sample point); all models control for age, gender, education, household size, german region and survey year interaction, inhabitant size of municipality, and distance to municipality administration"
  ) %>% 
  rbind(c("", "Soil Sealing", "", "", "Green Spaces", "", ""), .) %>% 
  huxtable::merge_cells(1, 2:4) %>% 
  huxtable::merge_cells(1, 5:7)


huxtable::quick_html(tab_regressions, file = "./revise_and_resubmit_2/tables/Table 2.html")


#' tab_second_differences_interaction_model.R
tab_second_differences_interaction_model <- 
  huxtable::huxreg(
    "Soil Sealing" =
      create_tidy_override_mlincom(
        second_differences_sealing,
        glance_add = c(
          "AME_all" = 
            extract_coefficient(
              second_differences_dydx_sealing$mlincom_results, 1)
        )
      ),
    "Green Spaces" =
      create_tidy_override_mlincom(
        second_differences_green,
        glance_add = c(
          "AME_all" = 
            extract_coefficient(
              second_differences_dydx_green$mlincom_results, 1)
        )
      ),
    stars = c(`***` = 0.001, `**` =  0.01, `*` = 0.05, `+` = 0.1),
    statistics = c("Migrant-German Income AME" = "AME_all"),
    coefs =
      c(
        "German 1000 -> 4000 EUR" = "(3 - 1)",
        "Migrant 1000 -> 4000 EUR" = "(4 - 2)",
        "German-Migrant 1000 -> 4000 EUR" = "(3 - 1) - (4 - 2)"
      ),
    note = "{stars}; Data source: GGSS 2014 & 2016 (ZA5250, ZA5270) and IOER Monitor; estimates are based on cluster robust standard errors (sample point); all models control for age, gender, education, household size, german region and survey year interaction, inhabitant size of municipality, and distance to municipality administration"
  ) 


huxtable::quick_html(tab_second_differences_interaction_model, file = "./revise_and_resubmit_2/tables/Table 3.html")


#' tab_second_differences_urban.R
sealing_tab_4 <-
  create_tidy_override_mlincom(
    second_differences_dydx_urban_sealing
  )

sealing_tab_4$model <- 
  rbind(
    c("interaction", rep(NA, 6)),
    sealing_tab_4$model, 
    second_differences_dydx_urban_sealing_default$mlincom_results %>% 
      dplyr::mutate(term = "overall"),
    c("no_interaction", rep(NA, 6)),
    AME_urban_no_interaction_sealing %>% dplyr::select(-where),
    second_differences_dydx_urban_sealing_no_interaction$estimation_results[2,] %>% 
      dplyr::mutate(term = "diff")
  )

sealing_tab_4$tidy_cols <- 
  rbind(
    c("interaction", rep(NA, 6)),
    sealing_tab_4$tidy_cols, 
    second_differences_dydx_urban_sealing_default$mlincom_results %>% 
      dplyr::mutate(term = "overall"),
    c("no_interaction", rep(NA, 6)),
    AME_urban_no_interaction_sealing %>% dplyr::select(-where),
    second_differences_dydx_urban_sealing_no_interaction$estimation_results[2,] %>% 
      dplyr::mutate(term = "diff")
  )

green_tab_4 <-
  create_tidy_override_mlincom(
    second_differences_dydx_urban_green
  )

green_tab_4$model <- 
  rbind(
    c("interaction", rep(NA, 6)),
    green_tab_4$model, 
    second_differences_dydx_urban_green_default$mlincom_results %>% 
      dplyr::mutate(term = "overall"),
    c("no_interaction", rep(NA, 6)),
    AME_urban_no_interaction_green %>% dplyr::select(-where),
    second_differences_dydx_urban_green_no_interaction$estimation_results[2,] %>% 
      dplyr::mutate(term = "diff")
  )

green_tab_4$tidy_cols <- 
  rbind(
    c("interaction", rep(NA, 6)),
    green_tab_4$tidy_cols, 
    second_differences_dydx_urban_green_default$mlincom_results %>% 
      dplyr::mutate(term = "overall"),
    c("no_interaction", rep(NA, 6)),
    AME_urban_no_interaction_green %>% dplyr::select(-where),
    second_differences_dydx_urban_green_no_interaction$estimation_results[2,] %>% 
      dplyr::mutate(term = "diff")
  )




tab_second_differences_urban <-
  huxtable::huxreg(
    "Soil Sealing" = sealing_tab_4,
    "Green Spaces" = green_tab_4,
    statistics = c("Migrant-German Income AME" = "AME_all"),
    stars = c(`***` = 0.001, `**` =  0.01, `*` = 0.05, `+` = 0.1),
    coefs = c(
      "Income Interaction Model" = "interaction",
      "Migrant - German Income AME (Non-Urban)" = "(2 - 1)",
      "Migrant - German Income AME (Urban)" = "(4 - 3)",
      "Non-Urban - Urban Income" = "(4 - 3) - (2 - 1)",
      "Migrant - German Income AME (Overall)" = "overall",
      "No Income Interaction Model" = "no_interaction",
      "Migrant - German AME (Non-Urban)" = 
        "2.migration_background:1bn._at",
      "Migrant - German AME (Urban)" = 
        "2.migration_background:2._at",
      "Non-Urban - Urban" = "diff"
    ),
    note = "{stars}; Data source: GGSS 2014 & 2016 (ZA5250, ZA5270) and IOER Monitor; estimates are based on cluster robust standard errors (sample point); all models control for age, gender, education, household size, german region and survey year interaction, inhabitant size of municipality, and distance to municipality administration"
  ) %>% 
  huxtable::set_bold(c(2, 12), 1)


huxtable::quick_html(
  tab_second_differences_urban, 
  file = "./revise_and_resubmit_2/tables/Table 4.html"
)


#' ini-figure_options.R
green_limits <- ylim(20, 60)
sealing_limits <- ylim(20, 60)
green_minus_blue_limits <- ylim(20, 60)
blue_limits <- ylim(0, 5)
streets_minus_ecotone_limits <- ylim(0, 25)
urban_green_limits <- ylim(0, 100)
income_limits <- xlim(0, 10)
AME_limits_sealing <- ylim(-15, 75)
AME_limits_green <- ylim(-75, 15)
AME_limits_green_minus_blue <- ylim(-75, 15)
AME_limits_blue <- ylim(-5, 2.5)
AME_limits_streets_minus_ecotone <- ylim(-5, 25)
AME_limits_urban_green <- ylim(-20, 20)

# labels 
income_label <- "Equivalized Household Income in 1000 €"

caption_label <- 
  paste0(
    "Data source: GGSS 2016 & 2018; ",
    "N = ", nrow(dat), "; ",
    "95% confidence intervals based on cluster-robust ",
    "standard errors (sample point); \n",
    "all models control for age, gender, education, ",
    "household size, german region and survey year interaction, ",
    "inhabitant size of \nmunicipality, ",
    "and distance to municipality administration; ",
    "display of predictions is censored after an income of 10\n",
    "(i.e., 10,000 €), but underlying estimates are based on full income range."
  )


#' figure_1.R
fig_3d <- 
  ggplot() + 
  
  # river
  geom_sf(
    data = poi_elbe %>% rotate_data(),
    fill = "skyblue", 
    color = "skyblue"
  ) +
  
  # roads
  geom_sf(data = poi_roads %>% rotate_data(),
          aes(fill = code), fill = "gray", color = "gray") +
  
  # buildings
  geom_sf(data = poi_buildings %>% rotate_data(),
          aes(fill = code), fill = "grey", color = "grey") +
  
  # buffer
  geom_sf(
    data = poi_buffer %>% rotate_data(y_add = 0),
    color = "black",
    fill = "grey",
    linetype = "dashed",
    alpha = .5
  ) +
  
  # point
  geom_sf(data = poi %>% rotate_data(),
          color = "black", size = 3) +
  
  # frame
  geom_sf(
    data = poi_bbox %>% sf::st_as_sfc() %>% sf::st_sf() %>% rotate_data(),
    color = "black",
    fill = NA
  ) +
  
  # label
  annotate("text", x = 13226000, y = 1056500, 
           label = "Respondent's\nLocation") +
  
  # scalebar
  geom_sf(
    data = poi_scalebar_data %>% rotate_data(x_add = -150, y_add = -100),
    color = "black",
    fill = rep(c("black", "white"), 2)
  ) +
  
  annotate("text", 
           x = poi_scalebar_data %>% 
             rotate_data(x_add = -150, y_add = -100) %>% 
             sf::st_bbox() %>% 
             .$xmin, 
           y = poi_scalebar_data %>% 
             rotate_data(x_add = -150, y_add = -100) %>% 
             sf::st_bbox() %>% 
             .$ymax - 150,
           label = "0 km",
           angle = -12,
           size = 3
  ) +
  
  annotate("text", 
           x = poi_scalebar_data %>% 
             rotate_data(x_add = -150, y_add = -100) %>% 
             sf::st_bbox() %>% 
             .$xmax - 200, 
           y = poi_scalebar_data %>% 
             rotate_data(x_add = -150, y_add = -100) %>% 
             sf::st_bbox() %>% 
             .$ymin - 130,
           label = "1 km",
           angle = -12,
           size = 3
  ) +
  
  # compass
  geom_segment(
    data = poi_compass_data %>% 
      rotate_data(x_add = -250, y_add = -300) %>% 
      sf::st_bbox() %>% 
      {data.frame(xmin = .$xmin, xmax = .$xmax, ymin = .$ymin, ymax = .$ymax)},
    aes(x = xmin, y = ymin, xend = xmax, yend = ymax),
    size = 1.2,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed")
  ) +
  
  annotate("text", 
           x = poi_compass_data %>% 
             rotate_data(x_add = -250, y_add = -300) %>% 
             sf::st_bbox() %>% 
             .$xmax + 190, 
           y = poi_scalebar_data %>% 
             rotate_data(x_add = -250, y_add = -300) %>% 
             sf::st_bbox() %>% 
             .$ymax + 1400,
           label = "N",
           angle = -50,
           size = 3
  ) +
  
  # soil sealing
  geom_sf(
    data = poi_sealing %>% rotate_data(y_add = 2000),
    aes(fill = S40RG_2015_100m), 
    color = NA,
    fill = color_gradient(poi_sealing$S40RG_2015_100m, "Reds")
  ) +
  
  # buffer
  geom_sf(
    data = poi_buffer %>% rotate_data(y_add = 2000),
    color = "black",
    fill = "grey",
    linetype = "dashed",
    alpha = .5
  ) +
  
  # point
  geom_sf(data = poi %>% rotate_data(y_add = 2000),
          color = "black", size = 3) +
  
  # label
  annotate("text", x = 13226000, y = 1058500, 
           label = "Soil Sealing") +
  
  # green spaces
  geom_sf(
    data = poi_green %>% rotate_data(y_add = 4000),
    aes(fill = layer),
    color = NA,
    fill = color_gradient(poi_green$layer, "Greens")
  ) +
  
  # buffer
  geom_sf(
    data = poi_buffer %>% rotate_data(y_add = 4000),
    color = "black",
    fill = "grey",
    linetype = "dashed",
    alpha = .5
  ) +
  
  # point
  geom_sf(data = poi %>% rotate_data(y_add = 4000),
          color = "black", size = 3) +
  
  # label
  annotate("text", x = 13226000, y = 1060500, 
           label = "Green Spaces") +
  
  ggsn::blank() +
  
  labs(
    caption = 
      paste0(
        "Data Source: OpenStreetMap, ",
        "Federal Agency for Cartography and Geodesy (BKG), \n",
        "Leibniz Institute for Ecological and Regional Development (IOER)"
      )
  ) +
  theme(plot.caption = element_text(hjust = 0))


ggsave(
  "./revise_and_resubmit_2/figures/FIGURE 1.png",
  fig_3d,
  dpi = 600
)


#' figure_2_A.R
fig_prediction_sealing_with_income_interaction <-
  ggplot(
    prediction_sealing_with_income_interaction,
    aes(x = x, y = estimate, group = whom)
  ) +
  geom_line(aes(colour = "Migrant")) +
  geom_line(aes(y = conf.high), linetype = "dashed") +
  geom_line(aes(y = conf.low), linetype = "dashed") +
  
  geom_line(aes(y = coef_germ, colour = "German")) +
  geom_line(aes(y = lower_germ), linetype = "dashed", color = "gray") +
  geom_line(aes(y = upper_germ), linetype = "dashed", color = "gray") +
  scale_linetype_manual("",
                        values = c("German" = "solid",
                                   "Migrant" = "solid")) +
  scale_colour_manual("",
                      labels = c("German", "Migrant"),
                      values = c("German" = "gray",
                                 "Migrant" = "black")) +
  sealing_limits +
  income_limits +
  labs(x = income_label,
       y = "Soil Sealing in %") +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(0.35, 0.78),
        legend.title.align = 0)

fig_AME_sealing_with_income_interaction <-
  ggplot(AME_sealing_with_income_interaction, aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  geom_line(aes(y = conf.high), linetype = "dashed") +
  geom_line(aes(y = conf.low), linetype = "dashed") +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  AME_limits_sealing +
  income_limits +
  labs(x = income_label,
       y = "Average Marginal Effect:\nMigrant - German") +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title.align = 0)


#' figure_2_B.R
fig_prediction_green_with_income_interaction <-
  ggplot(
    prediction_green_with_income_interaction,
    aes(x = x, y = estimate, group = whom)
  ) +
  geom_line(aes(colour = "Migrant")) +
  geom_line(aes(y = conf.high), linetype = "dashed") +
  geom_line(aes(y = conf.low), linetype = "dashed") +
  
  geom_line(aes(y = coef_germ, colour = "German")) +
  geom_line(aes(y = lower_germ), linetype = "dashed", color = "gray") +
  geom_line(aes(y = upper_germ), linetype = "dashed", color = "gray") +
  scale_linetype_manual("",
                        values = c("German" = "solid",
                                   "Migrant" = "solid")) +
  scale_colour_manual("",
                      labels = c("German", "Migrant"),
                      values = c("German" = "gray",
                                 "Migrant" = "black")) +
  green_limits +
  income_limits +
  labs(x = income_label,
       y = "Green Spaces in %") +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(0.4, 0.2),
        legend.title.align = 0)

fig_AME_green_with_income_interaction <-
  ggplot(AME_green_with_income_interaction, aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  geom_line(aes(y = conf.high), linetype = "dashed") +
  geom_line(aes(y = conf.low), linetype = "dashed") +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  AME_limits_green +
  income_limits +
  labs(x = income_label,
       y = "Average Marginal Effect:\nMigrant - German") +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title.align = 0)


#' figure_2.R
figure_2_new <-
  (fig_prediction_sealing_with_income_interaction + 
     ggtitle("A.1 Soil Sealing") +
     fig_AME_sealing_with_income_interaction + 
     ggtitle("A.2 Soil Sealing")) / 
  ( fig_prediction_green_with_income_interaction + 
      ggtitle("B.1 Green Spaces") +
      fig_AME_green_with_income_interaction  +
      ggtitle("B.2 Green Spaces"))  +
  patchwork::plot_annotation(
    caption = 
      paste0("Data source: GGSS 2016 & 2018; ",
             "N = ", nrow(dat), "; ",
             "95% confidence intervals based on cluster-robust ",
             "standard errors (sample point); \n",
             "all models control for age, gender, education, ",
             "household size, german region and survey year interaction, ",
             "inhabitant size of \nmunicipality, ",
             "and distance to municipality administration"
      ),
    theme = theme(plot.caption = element_text(hjust = 0))
  )

ggsave(
  "./revise_and_resubmit_2/figures/FIGURE 2.png",
  figure_2_new,
  # width = 12,
  height = 7,
  dpi = 600
)


