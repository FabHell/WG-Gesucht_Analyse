


################################################################################
###   ####################################################################   ###
####   ###                                                            ###   ####
#####   ##                        SCHRIFTARTEN LADEN                  ##   #####
####   ###                                                            ###   ####
###   ####################################################################   ###
################################################################################


library(ggtext)
library(showtext)



## Laden verschiedener Fonts  --------------------------------------------------


font_add("domine", 
         regular = "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Daten\\Schriftarten\\Domine\\Domine-Regular.ttf",
         bold = "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Daten\\Schriftarten\\Domine\\Domine-Bold.ttf"
         )

font_add("playfair", 
         regular = "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Daten\\Schriftarten\\Playfair_Display\\PlayfairDisplay-Regular.ttf",
         bold = "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Daten\\Schriftarten\\Playfair_Display\\PlayfairDisplay-Bold.ttf"
         )

font_add("franklin", 
         regular = "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Daten\\Schriftarten\\Libre_Franklin\\LibreFranklin-Regular.ttf",
         italic = "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Daten\\Schriftarten\\Libre_Franklin\\LibreFranklin-Italic.ttf",
         bold = "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Daten\\Schriftarten\\Libre_Franklin\\LibreFranklin-Bold.ttf",
         )


showtext_opts(dpi = 300)
showtext_auto()


print("Fonts erfolgreich geladen")



## Themes laden ----------------------------------------------------------------


panel_background_color <- "#1c202a"

plot_title_family <- "playfair"
plot_title_color <- "gray70"

axis_text_family <- "domine"
axis_text_color <- "gray35"

axis_title_family <- "domine"
axis_title_color <- "gray45"

legend_text_family <- "domine"
legend_text_color <- "gray40"

axis_line_color = "gray25"
panel_grid_color <- "gray15"


theme_dunkel <- function() {
  
  theme(
    
    # Einstellungen Hintergründe ===============================================
    plot.background  = element_rect(
      fill =  panel_background_color, 
      color = panel_background_color
      ),
    panel.background  = element_rect(
      fill =  panel_background_color, 
      color = panel_background_color
      ),
    legend.background = element_rect(
      fill =  panel_background_color, 
      color = panel_background_color
      ),
    
    # Einstellungen Texte ======================================================
    plot.title = element_text(
      color = plot_title_color,
      family = plot_title_family
      ),
    axis.title = element_text(
      color = axis_title_color,
      family = axis_title_family
      ),
    axis.text = element_text(
      color = axis_text_color,
      family = axis_text_family
      ),
    legend.text = element_text(
      color = legend_text_color,
      family = legend_text_family
      ),
    
    # Einstellungen Linien =====================================================
    axis.line = element_line(
      color = axis_line_color
      ),
    panel.grid.major = element_line(
      color = panel_grid_color
      ),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank()
  )
}


print("Theme erfolgreich geladen")



