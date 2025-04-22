

#### Plots ####

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ggrepel)

anexo_anet <- read_excel("anexo_paises.xlsx")

plot_general <- anexo_anet %>% 
  filter(Poblacion == "ARG")


n   <- length(unique(plot_general$Set))
# tomo de la paleta los 7 tonos más oscuros (índices 3 a 9) y los interpolo a n
cols <- colorRampPalette(brewer.pal(9, "Greens")[5:9])(n)

grafico_ProLen <- ggplot(plot_general, aes(x = M_Val, y = M_Aro, shape = Set, color = Set)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = cols) +  # uso sólo verdes más intensos
  xlab("Valencia") +
  ylab("Activación") +
  coord_flip() +
  scale_x_continuous(breaks = 1:9, limits = c(1, 9)) +
  scale_y_continuous(breaks = 1:9, limits = c(1, 9)) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12)
  )

ggsave("grafico_1_prolen.png", 
       plot = grafico_ProLen, 
       width = 6, height = 4,
       dpi = 300, units = "in")




colores_personalizados <- colorRampPalette(
  brewer.pal(9, "Greens")[5:9]  # indices 3 a 9: los más intensos
)(2)

grafico_edad_Prolen <- ggplot(base_anova, aes(x = texto, y = mean_dom, 
                                              color = edad, group = edad)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 21, fill = "white") +
  
  # eje X en castellano
  scale_x_discrete(labels = c(
    "Implicit" = "Implícito",
    "Explicit" = "Explícito"
  )) +
  
  # colores y leyenda edad en castellano
  scale_color_manual(
    values = colores_personalizados,
    name   = "Edad",
    labels = c(
      "Menos40" = "Menor de 40",
      "Mas40"   = "Mayor de 40"
    )
  ) +
  
  # etiqueta eje Y en castellano
  labs(y = "") +
  
  theme_minimal() +
  theme(
    plot.title        = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text         = element_text(size = 15),
    axis.title        = element_text(size = 14),
    legend.position   = "bottom",
    legend.title      = element_text(size = 14),
    legend.text       = element_text(size = 12),
    panel.grid.major  = element_line(color = "gray90"),
    panel.grid.minor  = element_blank()
  )




colores_sexo <- c(
  "Hombre" = "#B8860B",  # amarillo dorado
  "Mujer"  = "#228B22"   # verde lima
)

grafico_sexoProlen <-  ggplot(base_anova, aes(x = texto, y = mean_dom, 
                                              color = sexo, group = sexo)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 21, fill = "white") +
  scale_x_discrete(labels = c(
    "Implicit" = "Implícito",
    "Explicit" = "Explícito"
  )) +
  scale_color_manual(
    values = colores_sexo,
    name   = "Género",
    labels = c("Hombre" = "Hombre", "Mujer" = "Mujer")
  ) +
  labs(y = "Dominancia") +
  theme_minimal() +
  theme(
    plot.title        = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text         = element_text(size = 15),
    axis.title        = element_text(size = 14),
    legend.position   = "bottom",
    legend.title      = element_text(size = 14),
    legend.text       = element_text(size = 12),
    panel.grid.major  = element_line(color = "gray90"),
    panel.grid.minor  = element_blank()
  )


grafico_interaccionProlen <- (grafico_sexoProlen + grafico_edad_Prolen) +
  plot_annotation(
    caption = "Tipo de Texto",
    theme = theme(
      plot.caption = element_text(size = 14, hjust = .52, vjust = 1),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
  ) & 
  theme(
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    axis.title.x = element_text(size = 14, margin = margin(r = 10))  # Centrar y ajustar "Arousal"
  )



grafico_interaccionProlen

# Guardar el gráfico combinado
ggsave("grafico_interaccionProlen.png",
       plot = grafico_interaccionProlen, 
       width = 12, height = 6, 
       dpi = 300)





base_anova <- readxl::read_excel("base_anova.xlsx")


data <- base_anova %>%
  mutate(
    texto = factor(texto, levels = c("Implícito", "Explícito"), 
                   labels = c("Implicit",
                              "Explicit")),
    edad = factor(edad, levels = c("Menos40", "Mas40"),
                  labels = c("Under 40", "Over 40"))
  )



# Número de grupos de edad
n <- length(unique(data$edad))
# Tomo los índices 3:9 de la paleta "Greens" (los más intensos) y genero n tonos
mis_verdes <- colorRampPalette(brewer.pal(9, "Greens")[4:9])(n)

aro_ProLen <- ggplot(data, aes(x = texto, y = mean_aro, fill = edad)) +
  stat_summary(fun = mean, geom = "bar",
               position = "dodge", color = "black") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
               position = position_dodge(width = 0.9), width = 0.2) +
  
  # Colores manuales y etiquetas de leyenda en español
  scale_fill_manual(
    values = mis_verdes,
    name   = "Grupo etario",
    labels = c("Menor de 40", "Mayor de 40")
  ) +
  
  # Traducción de los niveles del eje X
  scale_x_discrete(
    name   = "Tipo de texto",
    labels = c(
      "Implicit" = "Implícito",
      "Explicit" = "Explícito"
    )
  ) +
  
  # Etiqueta del eje Y
  labs(y = "Activación") +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major   = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor   = element_blank(),
    axis.text          = element_text(color = "black"),
    axis.title         = element_text(face = "bold"),
    legend.title       = element_text(face = "bold"),
    legend.position    = "bottom",
    legend.box.spacing = unit(0.4, "cm")
  )

ggsave(
  filename = "arousal_proLen.png",  # Change to .pdf or .tiff for other formats
  plot = aro_ProLen,
  width = 8,                     # Width in inches
  height = 6,                    # Height in inches
  dpi = 300                      # Resolution in dots per inch (300 is standard for journals)
)