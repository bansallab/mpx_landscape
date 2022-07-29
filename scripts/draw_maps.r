### FUNCTIONS TO JOIN GPW AND GADM DATA AND THEN DRAW MAPS FOR THE WORLD
### Juliana Taube
### last updated 07/13/22

map_main <- function(estimates, do_plot_difference, var_to_plot,
                     legend_title, lower, upper, palette, show_legend,
                     wide_legend, diff_comparison_path){
  
  # use of gadm starts here since this is mapping, although some fixing of names and aggregating
  #   has already been done to the GPW data
  gadm_longjoin <- estimates %>% 
    left_join(spread_gpw, by = c("ISOALPHA", "COUNTRYNM", "NAME1" = "original_gpw_name")) %>% 
    mutate(gadm_match = ifelse(is.na(updated_gpw_name), NAME1, updated_gpw_name)) %>% 
    left_join(gadm_clean, by = c("ISOALPHA" = "ISO", "gadm_match" = "admin1name")) %>% 
    select(-c(version, TYPE_1, OBJECTID)) %>% distinct() %>% filter(!is.na(NAME_0))
  
  quant_vec <- c()
  if(do_plot_difference){
    world_estimates <- read_csv(diff_comparison_path) %>% 
      rename(perc_susceptible_world = perc_susceptible)
    gadm_longjoin <- gadm_longjoin %>% 
      left_join(world_estimates, by = c("ISOALPHA", "COUNTRYNM", "NAME1")) %>% 
      mutate(difference = perc_susceptible - perc_susceptible_world)
    max_val <- max(abs(min(gadm_longjoin$difference, na.rm = T)),
                   abs(max(gadm_longjoin$difference, na.rm = T)))
    if(is.na(lower) & is.na(upper)){lower <- -max_val; upper <- max_val}
    #quant_vec <- quantile(gadm_longjoin$difference, na.rm = T)
  }else{
    if(is.na(lower)){lower <- min(gadm_longjoin$eval(parse(text = var_to_plot)), na.rm = T)}
    if(is.na(upper)){upper <- max(gadm_longjoin$eval(parse(text = var_to_plot)), na.rm = T)}
    #quant_vec <- quantile(gadm_longjoin$perc_susceptible, na.rm = T)
  }
  
  gadm404_longjoin_sf <- gadm404 %>% left_join(gadm_longjoin, by = c("ISO" = "ISOALPHA", "ID_1"))
  
  print(upper)
  print(lower)
  # make plot
  out <- gadm404_longjoin_sf %>% 
    ggplot(aes(fill = eval(parse(text = var_to_plot)))) + 
    geom_sf(size = 0.05, color = NA) + 
    #coord_sf() +
    scale_fill_gradientn(colors = met.brewer(palette),
                         name = legend_title,
                         limits = c(lower, upper)) + #,
                         #values = scales::rescale(quant_vec, c(0, 1))) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          legend.position = ifelse(show_legend, "right", "none"),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 15),
          legend.key.width = unit(ifelse(wide_legend, 2.5, 1), "cm"),
          plot.background = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))# +
  #guides(fill = guide_colorbar(title.position = "left"))
  return(out)
  #ggsave(fig_path, height = 4, width = 8, dpi = 600)
}

# join_shape_data <- function(estimates, do_plot_difference){
#   # use of gadm starts here since this is mapping, although some fixing of names and aggregating
#   #   has already been done to the GPW data
#   gadm_longjoin <- estimates %>%
#     left_join(spread_gpw, by = c("ISOALPHA", "COUNTRYNM", "NAME1" = "original_gpw_name")) %>%
#     mutate(gadm_match = ifelse(is.na(updated_gpw_name), NAME1, updated_gpw_name)) %>%
#     left_join(gadm_clean, by = c("ISOALPHA" = "ISO", "gadm_match" = "admin1name")) %>%
#     select(-c(version, TYPE_1, OBJECTID)) %>% distinct() %>% filter(!is.na(NAME_0))
# 
#   if(do_plot_difference){
#     world_estimates <- read_csv("data/world-estimates-waning85.csv") %>%
#       rename(perc_protected_world = perc_protected)
#     gadm_longjoin <- gadm_longjoin %>%
#       left_join(world_estimates, by = c("ISOALPHA", "COUNTRYNM", "NAME1")) %>%
#       mutate(protect_diff = perc_protected - perc_protected_world)
#   }
# 
#   gadm404_longjoin_sf <- gadm404 %>% left_join(gadm_longjoin, by = c("ISO" = "ISOALPHA", "ID_1"))
# 
#   return(gadm404_longjoin_sf)
# }
#   
# 
# map_ests <- function(estimates, do_plot_difference, var_to_plot,
#                      legend_title, lower, upper, palette, show_legend){
#   
#   if(do_plot_difference){
#     max_val <- max(abs(min(estimates$protect_diff, na.rm = T)),
#                    abs(max(estimates$protect_diff, na.rm = T)))
#     if(is.na(lower) & is.na(upper)){lower <- -max_val; upper <- max_val}
#   }else{
#     if(is.na(lower)){lower <- min(estimates$eval(parse(text = var_to_plot)), na.rm = T)}
#     if(is.na(upper)){upper <- max(estimates$eval(parse(text = var_to_plot)), na.rm = T)}
#   }
#   
#   print(upper)
#   print(lower)
#   # make plot
#   out <- estimates %>% 
#     ggplot(aes(fill = eval(parse(text = var_to_plot)))) + 
#     geom_sf(size = 0.05, color = NA) + 
#     coord_sf() +
#     scale_fill_gradientn(colors=met.brewer(palette), 
#                          name = legend_title,
#                          limits = c(lower, upper))+
#     theme_bw() + 
#     theme(panel.border = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text.x = element_blank(),
#           legend.position = ifelse(show_legend, "right", "none"),
#           legend.background = element_blank(),
#           legend.box.background = element_blank(),
#           legend.title = element_text(vjust = 0),
#           plot.background = element_rect(fill = "transparent"),
#           panel.background = element_rect(fill = "transparent"))# +
#     #guides(fill = guide_colorbar(title.position = "left"))
#   return(out)
#   #ggsave(fig_path, height = 4, width = 8, dpi = 600)
# }

### US MAP
options(tigris_use_cache = TRUE)
all_pumas <- pumas(cb = T) %>% 
  mutate(PUMA = as.integer(PUMACE10),
         ST = as.integer(STATEFP10))

map_us <- function(data, do_plot_difference, var_to_plot,
                   legend_title, lower, upper, palette, show_legend){
  #data, do_plot_difference, legend_title, palette, lower, upper,
  #show_legend){
  
  joined_pumas <- all_pumas %>%
    left_join(data, by = c("PUMA","ST")) %>%
    filter(ST <= 56)
  
  if(is.na(lower) & is.na(upper)){
    lower <- min(joined_pumas$eval(parse(text = var_to_plot)), na.rm = T)
    upper <- max(joined_pumas$eval(parse(text = var_to_plot)), na.rm = T)
  }else if(is.na(upper)){
    upper <- max(joined_pumas$eval(parse(text = var_to_plot)), na.rm = T)
  }
  
  # quant_col <- ifelse(var_to_plot == "perc_susceptible",
  #                     joined_pumas$perc_susceptible,
  #                     joined_pumas$protect_diff)
  
  joined_pumas %>%
    ggplot(aes(fill = eval(parse(text = var_to_plot)))) +
    geom_sf(size = 0.01, color = NA) +
    scale_fill_gradientn(colors=met.brewer(palette), 
                         name = legend_title,
                         limits = c(lower, upper)) + # not even using this legend
                         # values = scales::rescale(quantile(joined_pumas$perc_susceptible),
                         #                          c(0, 1))) +
    theme_void() +
    theme(legend.title = element_text(size = 20),
          legend.text = element_text(size = 15),
          legend.key.width = unit(2.5, "cm")) +
    xlim(-125, -68) +
    ylim(25, 50) +
    theme(legend.position = ifelse(show_legend, "right", "none")) -> p1
  joined_pumas %>%
    ggplot(aes(fill = eval(parse(text = var_to_plot)))) +
    geom_sf(size = 0.01, color = NA) +
    scale_fill_gradientn(colors=met.brewer(palette), 
                         name = legend_title,
                         limits = c(lower, upper)) +
    theme_void() +
    xlim(-180, -130) +
    ylim(50, 75) +
    theme(legend.position = "none",
          plot.margin=grid::unit(c(0,0,0,0), "mm")) -> p2
  joined_pumas %>%
    ggplot(aes(fill = eval(parse(text = var_to_plot)))) +
    geom_sf(size = 0.01, color = NA) +
    scale_fill_gradientn(colors=met.brewer(palette), 
                         name = legend_title,
                         limits = c(lower, upper)) +
    theme_void() +
    xlim(-160, -153) +
    ylim(17, 25) +
    theme(legend.position = "none") -> p3
  
  out <- p1 + inset_element(p2, left = 0, bottom = 0, right = 0.38, top = 0.35) +
    inset_element(p3, left = 0.25, bottom = 0, right = 0.45, top = 0.3)
  return(out)
  #ggsave(fig_path, height = 4, width = 8, dpi = 600)
  
}




