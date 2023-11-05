
library (xml2)


apiXml <-
  local ({
    if (!fs::file_exists('cl.xml')) {
      message("Downloading CL API...")
      
      curl::curl_download(
        url = 'https://github.com/KhronosGroup/OpenCL-Docs/blob/main/xml/cl.xml',
        destfile = 'cl.xml'
      )
    }
    
    xml2::read_xml('cl.xml')
  })
  
typeMappings <-
  tibble::tibble(
    XML_NODES =
      apiXml |>
      xml2::xml_find_all(xpath = '//type[type and name]') |>
      c(),
    
    TYPE =
      XML_NODES |>
      purrr::map(selectr::querySelector, selector = 'type > type'),
    
    NAME =
      XML_NODES |>
      purrr::map(selectr::querySelector, selector = 'type > name')
  ) |>
  dplyr::transmute(
    dplyr::across(
      c(TYPE, NAME),
      ~ purrr::map_chr(., .f = xml2::xml_text)
    )
  )

enums <-
  tibble::tibble(
    XML_NODES =
      apiXml |>
      selectr::querySelectorAll('enums') |>
      c(),
    
    NAME =
      XML_NODES |>
      purrr::map_chr(xml2::xml_attr, attr = 'name') |>
      stringr::str_trim(),
    
    IS_BITMASK =
      XML_NODES |>
      purrr::map_chr(xml2::xml_attr, attr = 'type') |>
      magrittr::equals('bitmask') |>
      dplyr::coalesce(FALSE)
  ) |>
  dplyr::filter(
    NAME |>
      stringr::str_detect(
        pattern = stringr::regex('(?i)^cl_')
      ),
    
    NAME |>
      stringr::str_detect(
        pattern = stringr::regex('(?i)(intel|amd|arm)'),
        negate = TRUE
      ),
    
    NAME |>
      stringr::str_detect(
        pattern = stringr::regex('(?i)khr$'),
        negate = TRUE
      )
  ) |>
  dplyr::mutate(
    COMPILED_NAME =
      NAME |>
      stringr::str_remove(
        pattern = stringr::regex('(?i)^cl_')
      ) |>
      stringr::str_split(
        pattern = stringr::fixed('_'),
        simplify = FALSE
      ) |>
      purrr::map(stringr::str_to_title),
    
    GROUP =
      COMPILED_NAME |>
      purrr::map(head, n = 1L),
    
    COMPILED_NAME =
      COMPILED_NAME |>
      purrr::map_chr(stringr::str_c, collapse = ''),
    
    LEVELS =
      purrr::map2(
        XML_NODES,
        IS_BITMASK,
        function (xmlNodes, isBitmask) {
          xmlLevels <-
            xmlNodes |>
            selectr::querySelectorAll(selector = 'enum')
          
          if (isBitmask) {
            tibble::tibble(
              BIT_POSITION =
                xmlLevels |>
                xml2::xml_attr('bitpos'),
              
              LEVEL =
                xmlLevels |>
                xml2::xml_attr('name')
            )
          } else {
            tibble::tibble(
              VALUE =
                xmlLevels |>
                xml2::xml_attr('value'),
              
              LEVEL =
                xmlLevels |>
                xml2::xml_attr('name')
            )
          }
        }),
    
    COUNT_LEVELS =
      LEVELS |>
      purrr::map_int(nrow)
  ) |>
  dplyr::left_join(
    typeMappings,
    by = 'NAME'
  ) |>
  dplyr::filter(
    !is.na(TYPE)
  ) |>
  dplyr::arrange(
    GROUP,
    COMPILED_NAME
  )