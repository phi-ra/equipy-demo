library(tidyverse)
library(ggpattern)
library(wesanderson)
library(latex2exp)

extrafont::loadfonts(quiet = T)

font_main = 'Helvetica Neue'
font_title = 'Helvetica Neue Medium'
face_text='plain'
face_title='plain'
size_title = 18
size_text = 16

global_theme <- function(){
  theme_minimal() %+replace%
    theme(
      text=element_text(family=font_main, size=size_text,face=face_text),
      axis.text = element_text(size=size_text, face=face_text), 
      plot.title = element_text(family=font_title, size=size_title, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

colors_ <- wes_palette('Rushmore1')
setwd('~/Documents/Uni/PhD/UQAM/pkgs/equipy_demo/')

#### Drawing data ####

num_samps = 1000000
sample_1 <- rnorm(n=num_samps, 
                  mean = 2, 
                  sd = 3)

sample_2 <- rnorm(n=num_samps, 
                  mean = 32, 
                  sd = 7)

## create simple barycenter

bary_1 = function(x) 0.5*x + 0.5*quantile(sample_2,ecdf(sample_1)(x))
bary_2 = function(x) 0.5*quantile(sample_1,ecdf(sample_2)(x)) + 0.5*x

tibble(
  s_1 = sample_1, 
  s_2 = sample_2, 
  bary_1 = bary_1(sample_1) %>% unname(), 
  bary_2 = bary_2(sample_2 %>% unname())) %>%  
  gather() %>% 
  mutate(key = if_else(grepl('bary', key), 'barycenter', key)) %>% 
  mutate(key = factor(key, levels=c('s_1', 's_2', 'barycenter'))) %>% 
  ggplot() + 
  geom_density(aes(x=value,
                   color=key, 
                   lwd=key, 
                   lty=key), 
               key_glyph = draw_key_path,
                 position = position_dodge()) + 
  global_theme() + 
  guides(color=guide_legend(title='', 
                            override.aes = list(lwd=2, 
                                                lty=c('dashed', 'dashed', 'solid'))), 
         lwd=FALSE, 
         lty=FALSE) + 
  scale_linewidth_manual(values=c(1,1,1.75)) + 
  scale_linetype_manual(values=c('dashed', 'dashed', 'solid')) + 
  scale_color_manual(
    values = colors_[c(1,4,3)], 
    labels = c(expression(paste(f["s=0"]^{"*"})), 
               expression(paste(f["s=1"]^{"*"})), 
               expression(paste(f["barycenter"]))
               )) + 
  theme(legend.position = 'bottom',
        legend.key.size = unit(1.75, "cm"),
        legend.text = element_text(size=18)) + 
  
  ylab('Density') + 
  xlab('Prediction') #-> p1


dev.off()
p1
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="data/plots/")
dev.off()  




