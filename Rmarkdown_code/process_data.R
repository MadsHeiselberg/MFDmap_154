# Load data
library(dplyr)
library(stringr)
library(glue)
library(tidygeocoder)
library(tidyverse)


translate_habitat <- function(df, col = "sample_habitat_local") {
  df %>%
    mutate(sample_habitat_local_dk = case_when(
      str_detect(!!sym(col), "Beech forests Luzulo-Fagetum") ~ "Bøgeskov Luzulo-Fagetum",
      str_detect(!!sym(col), "Quaking bogs") ~ "Hængesæk",
      str_detect(!!sym(col), "Calcareous grassland") ~ "Kalkrig græsland",
      str_detect(!!sym(col), "Barley, Spring") ~ "Byg, vårbyg",
      str_detect(!!sym(col), "Lake, Rich pondweed") ~ "Sø, rig på vandaks",
      str_detect(!!sym(col), "Parks") ~ "Parker",
      str_detect(!!sym(col), "Molinia meadows") ~ "Blåtopenge",
      str_detect(!!sym(col), "Solanales, Potato, Starch") ~ "Kartoffel (stivelse)",
      str_detect(!!sym(col), "Rhynchosporion depressions") ~ "Nedbrudte tørvemos-samfund",
      str_detect(!!sym(col), "Degraded raised bog") ~ "Nedbrudt højmoser",
      str_detect(!!sym(col), "Hydrophilous tall-herb swamp") ~ "Vådområder med højstauder",
      str_detect(!!sym(col), "Temperate forests") ~ "Tempererede skove",
      str_detect(!!sym(col), "Rainwater basin, City") ~ "Regnvandsbassin, by",
      str_detect(!!sym(col), "Open sea and tidal areas") ~ "Åbent hav og tidevandsområder",
      str_detect(!!sym(col), "Grass, Clover, Rotation") ~ "Græs, kløver, omdrift",
      str_detect(!!sym(col), "Dry heath") ~ "Tør hede",
      str_detect(!!sym(col), "Decalcified Empetrum dunes") ~ "Afkalkede revling-klitter",
      str_detect(!!sym(col), "Standing freshwater") ~ "Stående ferskvand",
      str_detect(!!sym(col), "Perennial vegetation of stony banks") ~ "Flerårig vegetation på stenede brinker",
      str_detect(!!sym(col), "Sphagnum acid bogs") ~ "Sphagnum-sur mose",
      str_detect(!!sym(col), "Atlantic salt meadows") ~ "Atlantiske strandenge",
      str_detect(!!sym(col), "Fixed dunes \\(grey dunes\\)") ~ "Faste klitter (grå klitter)",
      str_detect(!!sym(col), "Lake, Mixed Najas flexilis") ~ "Sø, blandet Najas flexilis",
      str_detect(!!sym(col), "Wooded dunes") ~ "Træklitter",
      str_detect(!!sym(col), "Active raised bogs") ~ "Aktive højmoser",
      str_detect(!!sym(col), "Lake, Dystrophic") ~ "Sø, næringsfattig",
      str_detect(!!sym(col), "Beech forests Limestone") ~ "Bøgeskov på kalk",
      str_detect(!!sym(col), "Inland dunes Genista") ~ "Indlandsklitter med gyvel",
      str_detect(!!sym(col), "Other") ~ "Andet",
      str_detect(!!sym(col), "Marram dunes \\(white dunes\\)") ~ "Marehalmklitter (hvide klitter)",
      str_detect(!!sym(col), "Harbours") ~ "Havne",
      str_detect(!!sym(col), "Rainwater basin, Dried") ~ "Regnvandsbassin, udtørret",
      str_detect(!!sym(col), "Wet thicket") ~ "Våd krat (ikke habitat-type)",
      str_detect(!!sym(col), "Alluvial woodland") ~ "Fugtig elleskov",
      str_detect(!!sym(col), "Fabales, Faba bean") ~ "Hestebønner",
      str_detect(!!sym(col), "Running freshwater") ~ "Rindende ferskvand",
      str_detect(!!sym(col), "Dunes creeping willow") ~ "Klitter med krybende pil",
      str_detect(!!sym(col), "Inland dunes grass") ~ "Indlandsklitter med græs",
      str_detect(!!sym(col), "Enclosed water") ~ "Lukkede vandområder",
      str_detect(!!sym(col), "Inland dunes Empetrum nigrum") ~ "Indlandsklitter med revling",
      str_detect(!!sym(col), "Salt marshes and salt meadows") ~ "Saltmarsk og strandeng",
      str_detect(!!sym(col), "Xeric sand calcareous grasslands") ~ "Tørre sandede kalkgræsmarker",
      str_detect(!!sym(col), "Wet heath") ~ "Våd hede",
      str_detect(!!sym(col), "Sea cliffs and shingle or stony beaches") ~ "Havklinter og stenede strande",
      str_detect(!!sym(col), "Alkaline fens") ~ "Kalkrige kær",
      str_detect(!!sym(col), "Bog woodland") ~ "Mose-skov",
      str_detect(!!sym(col), "Semi-natural dry grasslands") ~ "Halvnatu­rlige tørre græsarealer",
      str_detect(!!sym(col), "Salicornia mud") ~ "Kveller-vade",
      str_detect(!!sym(col), "Humid dune slacks") ~ "Fugtige klitlavninger",
      str_detect(!!sym(col), "Siliceous rocky slopes") ~ "Kiselrige klippeskrænter",
      str_detect(!!sym(col), "Fjords") ~ "Fjorde",
      str_detect(!!sym(col), "Inland salt meadows") ~ "Indlands strandenge",
      str_detect(!!sym(col), "Poales, grass") ~ "Græs (Poaceae)",
      str_detect(!!sym(col), "Species-rich Nardus upland grassland") ~ "Artsrig nardus-bjergeng",
      str_detect(!!sym(col), "Semi-natural tall-herb humid meadows") ~ "Halvnatu­rlig højstaude-eng",
      TRUE ~ !!sym(col) # fallback: keep original if not matched
    ))
}

translate_habitat_broad <- function(df, col = "sample_habitat_broad") {
  df %>%
    mutate(sample_habitat_broad_dk = case_when(
      str_detect(!!sym(col), "Forests") ~ "Skove",
      str_detect(!!sym(col), "Bogs, mires and fens") ~ "Moser, kær og højmoser",
      str_detect(!!sym(col), "Grassland formations") ~ "Græsland",
      str_detect(!!sym(col), "Fields") ~ "Marker",
      str_detect(!!sym(col), "Freshwater") ~ "Ferskvand",
      str_detect(!!sym(col), "Greenspaces") ~ "Grønne områder",
      str_detect(!!sym(col), "Saltwater") ~ "Saltvand",
      str_detect(!!sym(col), "Temperate heath and scrub") ~ "Heder og krat",
      str_detect(!!sym(col), "Dunes") ~ "Klitter",
      str_detect(!!sym(col), "Coastal") ~ "Kystområder",
      str_detect(!!sym(col), "Rocky habitats and caves") ~ "Klipper og huler",
      is.na(!!sym(col))~"Jord",
      TRUE ~ !!sym(col) # fallback: keep original if not matched
    ))
}
translate_city <- function(x) {
  
  # Korrekte danske bynavne
  correct_city <- c(
    "Alsgarde"="Ålsgårde",
    "Sonderborg"="Sønderborg", 
    "Skorping"="Skørping",
    "Nykobing Mors"="Nykøbing Mors", 
    "Klitmoller"="Klitmøller",
    "Abybro"="Aabybro", "Harboore"="Harboøre", "Harboore"="Harboøre", 
    "Ronne"="Rønne", "Olgod"="Ølgod", "Rudkobing"="Rudkøbing", "Birkerod"="Birkerød",
    "Aabenraa"="Aabenraa"
  )
  
  
  x %>%
    ## byer correction
    str_replace_all("Alsgarde", "Ålsgårde") %>%
    str_replace_all("Sonderborg", "Sønderborg") %>%
    str_replace_all("Skorping", "Skørping") %>%
    str_replace_all("Nykobing Mors", "Nykøbing Mors") %>%
    str_replace_all("Klitmoller", "Klitmøller") %>%
    str_replace_all("Abybro", "Aabybro") %>%
    str_replace_all("Harboore", "Harboøre") %>%
    str_replace_all("Ronne", "Rønne") %>%
    str_replace_all("Olgod", "Ølgod") %>%
    str_replace_all("Rudkobing", "Rudkøbing") %>%
    str_replace_all("Birkerod", "Birkerød") %>%
    str_replace_all("Aabenraa", "Aabenraa") %>% 
    str_replace_all("Aarhus", "Aarhus") %>%
    str_replace_all("Aalborg", "Aalborg") %>% 
    str_replace_all("Noerre", "Nørre") %>%
    str_replace_all("Oester", "Øster") %>%
    str_replace_all("Soender", "Sønder") %>%
    str_replace_all("Ae", "Æ") %>%
    str_replace_all("Oe", "Ø") %>%
    str_replace_all("Aa", "Å") %>%
    str_replace_all("oe", "ø") %>%
    str_replace_all("ae", "æ") %>%
    str_replace_all("aa", "å")
    
    
}

####### This code is for processing the raw suplimentary data in DOI: https://doi.org/10.1038/s41564-025-02062-z ##################
# data <- readxl::read_xls("41564_2025_2062_MOESM3_ESM.xls", sheet = "Dataset_S5") %>%
#   translate_habitat(.) %>%  # translate habitats from English to danish
#   translate_habitat_broad(.) %>% #translate habitats from English to danish
#   mutate(
#     naming_city = ifelse(str_detect(genus_explanation, "Microbe found nearby "), str_remove(genus_explanation, 'Microbe found nearby '), "Ikke by brugt i navngivning"),
#     city_dk = sapply(naming_city, translate_city))
# 
# write_excel_csv2(data, file = "data_clean3.csv") #skriver bynavne forkert
############################################################################################################################################


#Microbe found nearby
tanslation_city <- "Bakterie slægten blev fundet førstegang tæt på"
translation_found <- "Bakterien er navngivet efter"
# Microbe named after
tanslation_name <- "Bakterie slægten er opkaldt efter"


data_shiny <- read_csv2("Data/data_clean_bynavnetjek.csv") %>% #Tilrettet bynavne og  
  mutate(
    change_name = ifelse(naming_city==city_dk, FALSE, TRUE),
  #### Make the HMTL code generating the popup messages. ############### 
  genus_nameing = case_when(str_detect(genus_explanation, "Microbe found nearby ")&genus_type == "Y"~glue("<b>Forklaring:</b> {translation_found} {city_dk} {ifelse(change_name==TRUE, paste0('(', {naming_city}, ')'), ' ')} <br/>"),
                                   #str_detect(genus_explanation, "Microbe found nearby ")~glue("{tanslation_city} {str_remove(genus_explanation, 'Microbe found nearby ')}, hvilken slægten er navgivet efter. Prøv at vælge bynavnet og se hvor denne slægt er fundet før (Orange markør)."),
                                   str_detect(genus_explanation, "Microbe named after ")~glue("<b>Forklaring:</b>{tanslation_name} {str_remove(genus_explanation, 'Microbe named after ')} <br/>"),
                                   genus_explanation=="NA"|is.na(genus_explanation)~" ",
                                   TRUE~" "),
         genus_proposed = ifelse(is.na(genus_proposed), str_remove(str_extract(taxonomy_proposed, "g__[A-z]+"), "g__"), genus_proposed),
          kingdom = case_when(str_detect(taxonomy_proposed, "Bacteria")==TRUE~"Bakteriens",
                              str_detect(taxonomy_proposed, "Archaea")==TRUE~"Arkebakterierens"),
    ## Names whether it is a new genus and type species or not. ## 
       nyslægt = ifelse(genus_type=="Y", "Ja", "Nej"),
         
    ## Generate the popup message ##
       popup_html = glue(
           "<b>{kingdom} navn:</b> {genus_proposed} {species_proposed}<br/>
           <b>Findested:</b> {sample_habitat_broad_dk} {ifelse(!is.na({sample_habitat_local_dk}), paste0('(', {sample_habitat_local_dk}, ')'), ' ')} <br/>
            <b> Ny slægt:</b> {nyslægt} <b> Ny art:</b> Ja <br/>
            {genus_nameing}
      "
         )
  )
data_shiny$latitude[data_shiny$city_dk == "Tillitse"]
data_shiny$latitude[data_shiny$city_dk == "Tillitse"][1] <- 54.71415+0.05 #Tilretter markør til land i stedet for i vandet 
data_shiny$latitude[data_shiny$city_dk == "Tillitse"]

#data_na <- data %>% filter(is.na(genus_nameing)) # checks for NA values in the text output.
# selects a varibles for saving to the data the apps loads.
save <- data_shiny %>% select(bin, genus_explanation, genus_type, nyslægt, genus_nameing,  popup_html, latitude, longitude, city_dk, taxonomy_proposed)
write.csv(save, "Data/shinyapp_data_done.csv")
