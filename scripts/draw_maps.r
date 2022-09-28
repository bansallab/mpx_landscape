### FUNCTIONS TO JOIN GPW AND GADM DATA AND THEN DRAW MAPS FOR THE WORLD
### Juliana Taube

join_shape_data <- function(estimates, do_plot_difference, diff_comparison_path){
  # use of gadm starts here since this is mapping, although some fixing of names and aggregating
  #   has already been done to the GPW data
  # spread_gpw converts one GPW to multiple GADMs
  gadm_longjoin <- estimates %>% 
    left_join(spread_gpw, by = c("ISOALPHA", "COUNTRYNM", "NAME1" = "original_gpw_name")) %>% 
    mutate(gadm_match = ifelse(is.na(updated_gpw_name), NAME1, updated_gpw_name)) %>% 
    left_join(gadm_clean, by = c("ISOALPHA" = "ISO", "gadm_match" = "admin1name")) %>% 
    select(-c(version, TYPE_1, OBJECTID)) %>% 
    distinct() %>% 
    filter(!is.na(NAME_0))
  
  #quant_vec <- c()
  if(do_plot_difference){
    world_estimates <- read_csv(diff_comparison_path) %>% 
      rename(perc_vaxxed_world = perc_vaxxed)
    gadm_longjoin <- gadm_longjoin %>% 
      left_join(world_estimates, by = c("ISOALPHA", "COUNTRYNM", "NAME1")) %>% 
      mutate(difference = perc_vaxxed - perc_vaxxed_world)
  }
  
  gadm404_longjoin_sf <- gadm404 %>% left_join(gadm_longjoin, by = c("ISO" = "ISOALPHA", "ID_1"))
  return(gadm404_longjoin_sf)
}


map_ests <- function(estimates, do_plot_difference, var_to_plot,
                     legend_title, lower, upper, palette, show_legend,
                     wide_legend){

  if(do_plot_difference){
    max_val <- max(abs(min(estimates$difference, na.rm = T)),
                   abs(max(estimates$difference, na.rm = T)))
    if(is.na(lower) & is.na(upper)){lower <- -max_val; upper <- max_val}

  }
  
  print(upper)
  print(lower)
  # make plot
  out <- estimates %>% 
    ggplot(aes(fill = eval(parse(text = var_to_plot)))) + 
    geom_sf(size = 0.05, color = NA) + 
    scale_fill_gradientn(colors = met.brewer(palette),
                         name = legend_title,
                         limits = c(lower, upper)) +
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
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
  return(out)
}

### US MAP
options(tigris_use_cache = TRUE)
all_pumas <- pumas(cb = T) %>% 
  mutate(PUMA = as.integer(PUMACE10),
         ST = as.integer(STATEFP10))

map_us <- function(data, var_to_plot, legend_title, lower, upper, palette, 
                   show_legend, wide_legend){
  
  joined_pumas <- all_pumas %>%
    left_join(data, by = c("PUMA","ST")) %>%
    filter(ST <= 56)
  
  joined_pumas %>%
    ggplot(aes(fill = eval(parse(text = var_to_plot)))) +
    geom_sf(size = 0.01, color = NA) +
    scale_fill_gradientn(colors=met.brewer(palette), 
                         name = legend_title,
                         limits = c(lower, upper)) + # not even using this legend
    theme_void() +
    theme(legend.title = element_text(size = 20, hjust = 0.5),
          legend.text = element_text(size = 15),
          legend.key.width = unit(ifelse(wide_legend, 2.5, 1), "cm"),
          legend.box = "horizontal") +
    xlim(-125, -68) +
    ylim(25, 50) +
    theme(legend.position = ifelse(show_legend, "bottom", "none")) +
    guides(fill = guide_colorbar(title.position = "top")) -> p1
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




