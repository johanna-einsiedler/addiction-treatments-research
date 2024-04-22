library(grid)
library(extrafont)
#######################################
# FUNCTIONS TO DEFINE PLOTTING STYLE
# Based on bbplot (https://github.com/bbc/bbplot) & Theme 538 (https://github.com/alex23lemm/theme_fivethirtyeight/)
#######################################

# function to set base size of text
define_base_size <- function(size=16){
  return (size)
}


# function to set colors
set_colors <- function(colors=c('#6EC4B3', '#e49273','#7180ac','#a37a74', '#2b4570')){
  #colors=c("#987284",
  #"#75b9be",
  #"#3b5249",
  #"#d0d6b5",
  #"#d4b483")

  return (colors)
}


# define basic theme for plots
theme_ind2 <- function (base_size=16, base_family="") {
  font <- "Helvetica Neue"
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # base text settings
      text = element_text(family = base_family, face = "plain",
                          colour = "#222222", size = base_size,
                          hjust = 0.5, vjust = 0.5, angle = 0, 
                          lineheight = 0.9),
      plot.title = element_text(family = font, size = rel(1), face = "bold", color = "#5A5A5A",margin = margin(t=10,b=10),hjust = 0, 
                                vjust = 1.5, ), # plot title style
      plot.subtitle = ggplot2::element_text(family = font, face = "bold",size = rel(.8), color='#5A5A5A',margin = ggplot2::margin(0, 0, 9, 0),hjust=0), # subplot title style
      #plot.caption = ggplot2::element_blank(), # plot caption set to empty per default
      legend.position = "top", legend.text.align = 0, legend.background = ggplot2::element_blank(), # position of legend on the top as default
      #legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), # legend title remove
      legend.text = ggplot2::element_text(family = font, size = rel(1), color = "#5A5A5A"), # set parameters for legend text
      axis.title = ggplot2::element_blank(), # remove axis title per default
      axis.text = ggplot2::element_text(family = font, size = rel(1), color = "#5A5A5A"), 
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)), 
      axis.ticks = ggplot2::element_blank(), 
      axis.line = ggplot2::element_blank(), 
      panel.grid.minor = ggplot2::element_blank(), 
      #panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"), 
      panel.grid.major.x = ggplot2::element_blank(), 
      panel.background = ggplot2::element_blank(), 
      strip.background = ggplot2::element_rect(fill = "#FCF6F2"), 
      plot.background =element_rect(fill='#FCF6F2')
      strip.text = ggplot2::element_text(size = 22, hjust = 0),
      complete=TRUE)
}





# define basic theme for plots
theme_ind <- function (base_size=16, base_family="") {
  font <- "Helvetica Neue"
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
              # base text settings
              text = element_text(family = base_family, face = "plain",
                                             colour = "#222222", size = base_size,
                                             hjust = 0.5, vjust = 0.5, angle = 0, 
                                             lineheight = 0.9),
               plot.title = element_text(family = font, size = rel(1.5), face = "bold", color = "#222222",margin = margin(t=10),hjust = 0, 
                                         vjust = 1.5, ), # plot title style
               plot.subtitle = ggplot2::element_text(family = font, face = "bold",size = rel(1), margin = ggplot2::margin(0, 0, 9, 0),hjust=0), # subplot title style
               plot.caption = ggplot2::element_blank(), # plot caption set to empty per default
               legend.position = "top", legend.text.align = 0, legend.background = ggplot2::element_blank(), # position of legend on the top as default
               legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), # legend title remove
               legend.text = ggplot2::element_text(family = font, size = rel(1), color = "#222222"), # set parameters for legend text
               axis.title = ggplot2::element_blank(), # remove axis title per default
               axis.text = ggplot2::element_text(family = font, size = rel(1), color = "#222222"), 
               axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)), 
               axis.ticks = ggplot2::element_blank(), 
               axis.line = ggplot2::element_blank(), 
               panel.grid.minor = ggplot2::element_blank(), 
               #panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"), 
               panel.grid.major.x = ggplot2::element_blank(), 
               panel.background = ggplot2::element_blank(), 
               strip.background = ggplot2::element_rect(fill = "white"), 
               strip.text = ggplot2::element_text(size = 22, hjust = 0),
              complete=TRUE)
}


# function to save plot
save_plot <- function (plot_grid, width, height, save_filepath) {
  grid::grid.draw(plot_grid)
  #save it
  ggplot2::ggsave(filename = save_filepath,
                  plot=plot_grid, width=(width/72), height=(height/72),  bg="white")
}

#Left align text
left_align <- function(plot_name, pieces){
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  return(grob)
}


finalise_plot <- function(plot_name,
                          source_name,
                          save_filepath=file.path(Sys.getenv("TMPDIR"), "tmp-nc.png"),
                          width_pixels=640,
                          height_pixels=450,
                          logo_image_path = file.path(system.file("data", package = 'bbplot'),"placeholder.png")) {
  
  footer <- create_footer(source_name, logo_image_path)
  
  #Draw your left-aligned grid
  plot_left_aligned <- left_align(plot_name, c("subtitle", "title", "caption"))
  plot_grid <- ggpubr::ggarrange(plot_name, footer,
                                 ncol = 1, nrow = 2,
                                 heights = c(1, 0.045/(height_pixels/450)))
  ## print(paste("Saving to", save_filepath))
  save_plot(plot_grid, width_pixels, height_pixels, save_filepath)
  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
  invisible(plot_grid)
}

# add footer
create_footer <- function (source_name, logo_image_path=NA) {
  base_size <-define_base_size()
  if (nchar(source_name)>50){
    text_size = base_size*0.5
  } else {text_size=base_size*0.8}
  
  print(text_size)
  if (is.na(logo_image_path)){
  #Make the footer
  footer <- grid::grobTree(grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(1.6, "npc")),
                           grid::textGrob(source_name,
                                          x = 0.004, hjust = 0,vjust=0, gp = grid::gpar(fontsize=text_size)),
                           grid::rasterGrob(png::readPNG(logo_image_path), x = 0.944))
  } else{
    footer <- grid::grobTree(grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(1.6, "npc")),
                             grid::textGrob(source_name,
                                            x = 0.004, hjust = 0,vjust=0, gp = grid::gpar(fontsize=text_size)))
  }
  return(footer)
  
}

