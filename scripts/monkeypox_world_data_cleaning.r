### CLEANS AND FORMATS GPW AND GADM DATA FOR ANALYSIS AND MAPPING
### Juliana Taube

library(tidyverse)
library(tidylog)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(stringi)
library(MetBrewer)

# RUN US_GPW_DATA BEFORE THIS

# skip to read in data_agg
# data <- read_csv("data/gpw-v4-admin-unit-center-points-population-estimates-rev11_global_csv 2/gpw_v4_admin_unit_center_points_population_estimates_rev11_global.csv")
# 
# # need to summarise to admin1 level, this may take a while 
# data_agg <- data %>% group_by(ISOALPHA, COUNTRYNM, NAME1) %>%
#   summarise(tot_popn_2010 = sum(UN_2010_E),
#             tot_popn_2020 = sum(UN_2020_E),
#             A00_04B = sum(A00_04B),
#             A05_09B = sum(A05_09B),
#             A10_14B = sum(A10_14B),
#             A15_19B = sum(A15_19B),
#             A20_24B = sum(A20_24B),
#             A25_29B = sum(A25_29B),
#             A30_34B = sum(A30_34B),
#             A35_39B = sum(A35_39B),
#             A40_44B = sum(A40_44B),
#             A45_49B = sum(A45_49B),
#             A50_54B = sum(A50_54B),
#             A55_59B = sum(A55_59B),
#             A60_64B = sum(A60_64B),
#             A65_69B = sum(A65_69B),
#             A70_74B = sum(A70_74B),
#             A75_79B = sum(A75_79B),
#             A80_84B = sum(A80_84B),
#             A85PLUSB = sum(A85PLUSB))
# 
# write_csv(data_agg, "data/world_agg_age_data_admin1.csv")

data_agg_orig <- read_csv("data/world_agg_age_data_admin1.csv")
clean_gpw <- function(admin1name){
  fix_1 <- gsub("[ ]region$|[ ]governorate$|[ ]administration$|[ ]gov.|^dpto.[ ]|[ ]district|[ ]territory|region[ ]de[ ]|district[ ]de[ ]|^bourough[ ]of[ ]|[ ]county$|[ ]province$|province[ ]de[ ]|province[ ]du[ ]|province[ ]de[ ]l[ ]|province[ ]de[ ]la[ ]|[ ]state$|region[ ]of[ ]|[ ]division|[ ]-[ ]metro[ ]city$|^raionul[ ]|^wilayah[ ]persekutuan[ ]|^region[ ]autonoma[ ]|^regiao[ ]autonoma[ ]da[ ]|^regiao[ ]autonoma[ ]dos[ ]|[ ]municipio$|[ ]oblast$|-ken$|'|`",
                "", admin1name)
  fix_2 <- gsub("&", "and", fix_1)
  fix_3 <- gsub("/[ ]", "-", fix_2)
  fix_4 <- gsub("^st\\.[ ]|^st[ ]", "saint ", fix_3)
  fix_5 <- gsub("'", "", fix_4)
  return(fix_5)
}

# don't sum up quite right, even to 2010 population totals, but on same order of magnitude
data_agg <- data_agg_orig %>% filter(tot_popn_2020 != 0) %>% 
  rowwise() %>% 
  mutate(COUNTRYNM = stri_trans_general(str = tolower(COUNTRYNM), id = "Latin-ASCII"),
         NAME1 = stri_trans_general(str = tolower(NAME1), id = "Latin-ASCII"),
         NAME1 = trimws(clean_gpw(NAME1))) %>% 
  # need to strip white space
  distinct() %>% 
  # deal with duplicate admin1s with different populations, e.g., ile-de-france, and sum to get total popn
  group_by(ISOALPHA, NAME1) %>% 
  summarise(COUNTRYNM = COUNTRYNM,
            NAME1 = NAME1,
            tot_popn_2010 = sum(tot_popn_2010), 
            tot_popn_2020 = sum(tot_popn_2020), 
            A00_04B = sum(A00_04B), A05_09B = sum(A05_09B), A10_14B = sum(A10_14B),
            A15_19B = sum(A15_19B), A20_24B = sum(A20_24B), A25_29B = sum(A25_29B), 
            A30_34B = sum(A30_34B), A35_39B = sum(A35_39B), A40_44B = sum(A40_44B),
            A45_49B = sum(A45_49B), A50_54B = sum(A50_54B), A55_59B = sum(A55_59B),
            A60_64B = sum(A60_64B), A65_69B = sum(A65_69B), A70_74B = sum(A70_74B),
            A75_79B = sum(A75_79B), A80_84B = sum(A80_84B), A85PLUSB = sum(A85PLUSB)) %>%
  ungroup() %>% 
  distinct() %>% 
  filter(!is.na(NAME1)) %>% 
  mutate(NAME1 = trimws(NAME1), # add version back below
         ISOALPHA = ifelse(ISOALPHA == "KOS", "XKO", ISOALPHA)) %>% # gadm has different kosovo isoalpha
  filter(NAME1 != "bikini") %>% # 1 person, old nuclear tests
  select(ISOALPHA, COUNTRYNM, NAME1, tot_popn_2010, tot_popn_2020, contains("A")) # reordering columns

# these are summing to <1 and >1, less than 1 maybe cause small rounding but over 1 cause of some
#   weird grouping

# add US data
us_data <- read_csv("data/cleaned_gpw_us_age_data.csv")
data_agg <- data_agg %>% bind_rows(us_data) %>% 
  mutate(admin_id = row_number())

# fix gpw names from MANUAL comparison to gadm
gpw_gadm_name_fix <- read_csv("data/gpw_to_gadm_country_join.csv")
data_agg_join <- data_agg %>% left_join(gpw_gadm_name_fix, by = c("ISOALPHA" = "ISOALPHA",
                                                                  "COUNTRYNM",
                                                                  "NAME1" = "original_gpw_name")) %>% 
  mutate(gadm_match = ifelse(is.na(updated_gpw_name), NAME1, updated_gpw_name)) %>%
  # need to summarise because multiple gpw now go to one gadm name, sum across all these
  group_by(ISOALPHA, COUNTRYNM, gadm_match) %>% 
  summarise(tot_popn_2010 = sum(tot_popn_2010),
            tot_popn_2020 = sum(tot_popn_2020), 
            A00_04B = sum(A00_04B), A05_09B = sum(A05_09B), A10_14B = sum(A10_14B),
            A15_19B = sum(A15_19B), A20_24B = sum(A20_24B), A25_29B = sum(A25_29B), 
            A30_34B = sum(A30_34B), A35_39B = sum(A35_39B), A40_44B = sum(A40_44B),
            A45_49B = sum(A45_49B), A50_54B = sum(A50_54B), A55_59B = sum(A55_59B),
            A60_64B = sum(A60_64B), A65_69B = sum(A65_69B), A70_74B = sum(A70_74B),
            A75_79B = sum(A75_79B), A80_84B = sum(A80_84B), A85PLUSB = sum(A85PLUSB)) %>% 
  ungroup() %>% 
  distinct() %>% 
  rename(NAME1 = gadm_match)

# convert to proportions 09/06/22, use across here!!!
data_agg_join_props <- data_agg_join %>% rowwise() %>% 
  mutate(row_sum = sum(A00_04B, A05_09B, A10_14B, A15_19B, A20_24B, A25_29B, 
                       A30_34B, A35_39B,A40_44B, A45_49B, A50_54B, A55_59B, 
                       A60_64B, A65_69B, A70_74B, A75_79B, A80_84B, A85PLUSB),
         A00_04B = A00_04B/row_sum, A05_09B = A05_09B/row_sum, A10_14B = A10_14B/row_sum,
         A15_19B = A15_19B/row_sum, A20_24B = A20_24B/row_sum, A25_29B = A25_29B/row_sum, 
         A30_34B = A30_34B/row_sum, A35_39B = A35_39B/row_sum, A40_44B = A40_44B/row_sum,
         A45_49B = A45_49B/row_sum, A50_54B = A50_54B/row_sum, A55_59B = A55_59B/row_sum,
         A60_64B = A60_64B/row_sum, A65_69B = A65_69B/row_sum, A70_74B = A70_74B/row_sum,
         A75_79B = A75_79B/row_sum, A80_84B = A80_84B/row_sum, A85PLUSB = A85PLUSB/row_sum) %>% 
  filter(row_sum != 0) # no age distribution available

write_csv(data_agg_join_props, "data/cleaned_gpw_age_data_props.csv")


####
####
#### GADM DATA #### -----------------------------------------------------------------------
####
####

orig_gadm404 <- st_read("data/gadm404-levels.gpkg",
                        layer = "level1")
gadm404 <- orig_gadm404 %>% select(ID_0, COUNTRY, ID_1, NAME_1, VARNAME_1, TYPE_1, geom) %>%
  mutate(COUNTRY = stri_trans_general(str = tolower(COUNTRY), id = "Latin-ASCII"),
         NAME_1 = stri_trans_general(str = tolower(NAME_1), id = "Latin-ASCII"),
         VARNAME_1 = stri_trans_general(str = tolower(VARNAME_1), id = "Latin-ASCII"),
         ID_1 = gsub("^...\\.", "", gsub("_1$", "", ID_1)),
         OBJECTID = row_number()) %>%
  rename(ISO = ID_0)

gadm404_nosf <- data.frame(OBJECTID = gadm404$OBJECTID,
                           ISO = gadm404$ISO, NAME_0 = gadm404$COUNTRY, 
                           ID_1 = gadm404$ID_1, NAME_1 = gadm404$NAME_1, 
                           VARNAME_1 = gadm404$VARNAME_1, TYPE_1 = gadm404$TYPE_1)
#is there a better way to perform this check without doing a giant separate...although its pretty fast
# mutate(admin1name = gsub("[ ]a\\.s\\.s\\.r\\.", "", admin1name),
#        admin1name = gsub("'", "", gsub("`", "", gsub("!", "", gsub("\\.", "", admin1name)))),
#        admin1name = gsub("[ ]-[ ]", "-", admin1name)
clean_gadm <- function(name){
  fix_1 <- gsub("'|/|`|!|\\.", "", name)
  fix_2 <- gsub("[ ]-[ ]", "-", fix_1)
  return(fix_2)
}
gadm_variations <- function(variation, letter, type){
  if(is.na(variation) | type == " "){
    return(NA)
  }else if(letter == "a"){
    return(paste0(variation, type))
  }else if(letter == "b"){
    return(paste0(variation, "-", type))
  }else if(letter == "c"){
    return(paste0(variation, " ", type))
  }else if(letter == "d"){
    return(gsub("[ ]", "", gsub(type, "", variation)))
  }else if(letter == "e"){
    return(gsub("-", " ", variation))
  }
}

# trying to consider naming variations
# need to separate VARNAME_1 column in gadm
gadm404_sep <- gadm404_nosf %>% 
  mutate(VARNAME_1 = ifelse(VARNAME_1 == " ", NA,  gsub(",", "|", VARNAME_1)),
         TYPE_1 = tolower(TYPE_1),
         NAME_1 = trimws(clean_gadm(NAME_1))) %>% 
  separate(VARNAME_1, into = c("var1", "var2", "var3", "var4", "var5", "var6", "var7", "var8", "var9", "var10"),
           sep = "\\|") %>% 
  rowwise() %>% # only want to do this if TYPE_1 != " "
  mutate(var0typea = ifelse(TYPE_1 == " " | is.na(TYPE_1), NA, paste0(NAME_1, TYPE_1)),
         var0typeb = ifelse(TYPE_1 == " " | is.na(TYPE_1), NA, paste0(NAME_1, "-", TYPE_1)),
         var0typec = ifelse(TYPE_1 == " " | is.na(TYPE_1), NA, paste0(NAME_1, " ", TYPE_1)),
         var0typed = ifelse(TYPE_1 == " " | is.na(TYPE_1), NA, gsub(paste0("[ ]", TYPE_1), "", NAME_1)),
         var0typee = ifelse(TYPE_1 == " " | is.na(TYPE_1), NA, gsub("-", " ", NAME_1)),
         var1typea = gadm_variations(var1, "a", TYPE_1),
         var1typeb = gadm_variations(var1, "b", TYPE_1),
         var1typec = gadm_variations(var1, "c", TYPE_1),
         var1typed = gadm_variations(var1, "d", TYPE_1),
         var1typee = gadm_variations(var1, "e", TYPE_1),
         var2typea = gadm_variations(var2, "a", TYPE_1),
         var2typeb = gadm_variations(var2, "b", TYPE_1),
         var2typec = gadm_variations(var2, "c", TYPE_1),
         var2typed = gadm_variations(var2, "d", TYPE_1),
         var2typee = gadm_variations(var2, "e", TYPE_1),
         var3typea = gadm_variations(var3, "a", TYPE_1),
         var3typeb = gadm_variations(var3, "b", TYPE_1),
         var3typec = gadm_variations(var3, "c", TYPE_1),
         var3typed = gadm_variations(var3, "d", TYPE_1),
         var3typee = gadm_variations(var3, "e", TYPE_1),
         var4typea = gadm_variations(var4, "a", TYPE_1),
         var4typeb = gadm_variations(var4, "b", TYPE_1),
         var4typec = gadm_variations(var4, "c", TYPE_1),
         var4typed = gadm_variations(var4, "d", TYPE_1),
         var4typee = gadm_variations(var4, "e", TYPE_1),
         var5typea = gadm_variations(var5, "a", TYPE_1),
         var5typeb = gadm_variations(var5, "b", TYPE_1),
         var5typec = gadm_variations(var5, "c", TYPE_1),
         var5typed = gadm_variations(var5, "d", TYPE_1),
         var5typee = gadm_variations(var5, "e", TYPE_1),
         var6typea = gadm_variations(var6, "a", TYPE_1),
         var6typeb = gadm_variations(var6, "b", TYPE_1),
         var6typec = gadm_variations(var6, "c", TYPE_1),
         var6typed = gadm_variations(var6, "d", TYPE_1),
         var6typee = gadm_variations(var6, "e", TYPE_1),
         var7typea = gadm_variations(var7, "a", TYPE_1),
         var7typeb = gadm_variations(var7, "b", TYPE_1),
         var7typec = gadm_variations(var7, "c", TYPE_1),
         var7typed = gadm_variations(var7, "d", TYPE_1),
         var7typee = gadm_variations(var7, "e", TYPE_1),
         var8typea = gadm_variations(var8, "a", TYPE_1),
         var8typeb = gadm_variations(var8, "b", TYPE_1),
         var8typec = gadm_variations(var8, "c", TYPE_1),
         var8typed = gadm_variations(var8, "d", TYPE_1),
         var8typee = gadm_variations(var8, "e", TYPE_1),
         var9typea = gadm_variations(var9, "a", TYPE_1),
         var9typeb = gadm_variations(var9, "b", TYPE_1),
         var9typec = gadm_variations(var9, "c", TYPE_1),
         var9typed = gadm_variations(var9, "d", TYPE_1),
         var9typee = gadm_variations(var9, "e", TYPE_1),
         var10typea = gadm_variations(var10, "a", TYPE_1),
         var10typeb = gadm_variations(var10, "b", TYPE_1),
         var10typec = gadm_variations(var10, "c", TYPE_1),
         var10typed = gadm_variations(var10, "d", TYPE_1),
         var10typee = gadm_variations(var10, "e", TYPE_1))

gadm404_long <- gadm404_sep %>% 
  pivot_longer(cols = c("NAME_1":"var10", "var0typea":"var10typee"),
               values_to = "admin1name", names_to = "version") %>% 
  mutate(admin1name = trimws(clean_gadm(admin1name))) %>% # redo clean on variations
  filter(!is.na(admin1name)) %>% 
  mutate(version = ifelse(version == "NAME_1", "original", "var")) %>% 
  filter(! is.na(ISO), ISO != "NA") %>% 
  distinct(OBJECTID, ISO, NAME_0, admin1name, .keep_all = TRUE) # accent clearing can lead to duplicate rows
  

write_csv(gadm404_long, "data/cleaned_gadm_data_noshapefile.csv")


# 
# > gadm_longjoin$NAME1[duplicated(gadm_longjoin)]
# [1] "ali sabieh"   "arta"         "dikhil"      
# [4] "djiboutii"    "obock"        "tadjoura"    
# [7] "rift valley"  "antananarivo" "antsiranana" 
# [10] "fianarantsoa" "mahajanga"    "toamasina"   
# [13] "toliary"  
