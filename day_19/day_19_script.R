library(tidyverse)
library(here)
library(ggforce)
#library(showtext)
#library(extrafont)
library(ggtext)


# downloaded data from here, copied to excel, and imported from there. 
# the .csv dataset is in the GitHub repo.
# http://atmenv.envi.osakafu-u.ac.jp/aono/kyophenotemp4/

data <- read.csv(here("day_19","kyoto2.csv")) %>% janitor::clean_names()
head(data)

data <- 
  data %>%
  mutate(fu_fd = as.numeric(fu_fd)) %>%
  filter(!is.na(fu_fd)) %>%
  mutate(date = paste0(0,fu_fd,a_d)) %>%
  mutate(date2 = as.Date(date, format = "%m%d%Y"),
         day = strftime(date2, format="%j"),
         day = as.numeric(day))
  
# plot data
data %>%
  ggplot(aes(x=a_d,y=day)) +
  geom_point() +
  geom_smooth()




# test making flower ------------------------------------------------------
df <- data.frame(x=c(1,1),y=c(1,3),year=c(1,2))

df %>%
  slice(rep(1:n(), each=5)) %>% 
  mutate(flower = rep_len(1:5, length.out=n())   ) %>%
  # make two anchors for each flower pedal
  mutate(anchor_x =   x + c(-.25, .8,  .7,  -.7, -.8),
         anchor_y =   y + c( .9, .5, -.4,  -.4,  .55)) %>%
  mutate(anchor_x2 =  x + c( .25, .9,  .3,  -.3, -.9),
         anchor_y2 =  y + c( .9, .2, -.6,  -.6,  .2)) %>%
  pivot_longer(cols = c(x,anchor_x,anchor_x2), values_to = "xs", names_to = "x_type") %>%
  pivot_longer(cols = c(y,anchor_y,anchor_y2), values_to = "ys", names_to = "y_type") %>%
  filter(x_type == "x" & y_type == "y" |
           x_type == "anchor_x" & y_type == "anchor_y" |
           x_type == "anchor_x2" & y_type == "anchor_y2")  %>%
  select(-x_type,-y_type) %>%
  
  
  ggplot()+
  # geom_shape(aes(x=xs,y=ys,group = interaction(year, flower)),
  #             expand = unit(.5, 'mm'), radius = unit(3, 'mm')) +
  geom_bspline_closed(aes(x=xs,y=ys,group=interaction(year,flower), fill=ys), color="pink",
                      expand = unit(6, 'mm'), radius = unit(5, 'mm'))+
  coord_equal(clip = "off")

  
  

# make points to draw a tree trunk ----------------------------------------

# I did this in a weird order while exploring the data, 
# but I traced the long branch over a geom_smooth of the real data. 
# color column just helped to distinguish which points were which as I was plotting
trunk = tribble(
  ~x,   ~y,   ~color,
  650,   51,  'blue',
  795,   51,  'blue',
  798,   51,  'blue',
  800,   51,  'blue',
  830,   51,  'blue',
  812,   62,  'blue',
  794,   70,  'blue',
  782,   77,  'blue',
  775,   85,  'blue',
  782,   92,  'blue',
  788,   95,  'blue',
  798,   97,  'blue',
  805,   98,  'blue',
  # follow branch to right (this branch traces geom_smooth trend line)
  825,   100,  'green',
  1000,   102.5,  'green',
  1125,   103.5,  'green',
  1250,   103.5, 'green',
  1375,   103, 'green',
  1500,   104, 'green',
  1625,   105, 'green',
  1685,   105.5, 'green',
  1750,   105.5, 'green',
  1875,   104,  'green',
  2000,   96.5,  'green',
  2035,   95.5,  'green', # branch tip
  # back up the branch right to left - back along trend line
  2000,   99.5,  'green',
  1875,   105,  'green',
  1750,   107.5,  'green',
  1685,   107,  'green',
  1625,   106.5,  'green',
  1500,   105,  'green',
  1375,   105,  'green',
  1250,   105,  'green',
  1125,   105,  'green',
  1000,   104.5,  'green',
  875,   104.5, 'green',
  
  
  # corner  
  788,   103.5,  'blue',
  788,   103,  'blue',
  790,   107,  'blue',
  800,   110,  'blue',
  # second branch  'blue'
  #  802,   114,  'blue'
  810,   117,  'blue',
  900,   118,  'blue',
  950,   120,  'blue',
  1100,   124,  'blue',
  1300,   130,  'blue',
  # end of branch  (off screen)
  1300,   131,  'blue',
  1300,   131,  'blue',
  1080,   125.5,  'blue',
  1000,   123,  'blue',
  900,   121,  'blue',
  800,    119,  'blue',
  802,    120,  'blue',
  #  825,   130,  'blue',
  # top border  
  750,   130,  'blue',
  700,   130,  'blue',
  680,   130,  'blue',
  765,   117,  'blue',
  760,   110,  'blue',
  
  
  #800,   130,  'blue',
  #650,   130,  'blue',
  #745,   130,  'blue',
  #710,   130,  'blue',
  660,   97,  'blue',
  
)



# full plot ----------------------------------------------


plot <- data %>%
  
  # repeat each year 5 times
  slice(rep(1:n(), each=5)) %>% 
  
  # add column for individual pedals
  mutate(pedal = rep_len(1:5, length.out=n())   ) %>%
  
  # rename
  mutate(year = a_d) %>%
  
  # make two anchors for each flower pedal - expand center point outwards so they form a triangle
  mutate(anchor_x =   a_d + (c(-.25, .8,  .7,  -.7, -.8)*10),
         anchor_y =   day + c( .9, .5, -.4,  -.4,  .55)) %>%
  mutate(anchor_x2 =  a_d + (c( .25, .9,  .3,  -.3, -.9)*10),
         anchor_y2 =  day + c( .9, .2, -.6,  -.6,  .2)) %>%
  
  # pivot to move anchor points - probably a bad way to do this, but functional.
  pivot_longer(cols = c(a_d,anchor_x,anchor_x2), values_to = "xs", names_to = "x_type") %>%
  pivot_longer(cols = c(day,anchor_y,anchor_y2), values_to = "ys", names_to = "y_type") %>%
  filter(x_type == "a_d" & y_type == "day" |
           x_type == "anchor_x" & y_type == "anchor_y" |
           x_type == "anchor_x2" & y_type == "anchor_y2")  %>%
  select(-x_type,-y_type) %>%
  
  # start plot -------------------
ggplot()+
  
  # draw tree trunk
  geom_bspline_closed(data = trunk, aes(x=x,y=y),
                      n = 500, alpha=1)+
  # add points for tree trunk (using when creating the tibble)
  # geom_point(data = trunk, aes(x=x,y=y,color=color))+
  # add smooth to trace tree trunk over
  # geom_smooth(data = . %>% group_by(year) %>% slice(1),
  #             aes(x=xs,y=ys), formula = "y~x", method = "loess",
  #             color="grey20", fill="green",alpha=.3) +
  
  # draw flowers -------------------
# draw flower pedals
  geom_bspline_closed(aes(x=xs,y=ys,group=interaction(year,pedal), fill=ys), color="grey20", fill ="#f7e6ed",
                      size= .12,
                      expand = unit(.5, 'mm'), radius = unit(.1, 'mm') )+
  # draw flower centers
  geom_point(data = . %>% group_by(year) %>% slice(1),
             aes(x=xs,y=ys), color="grey20", shape=8, stroke=.45, size=.4) +
  geom_point(data = . %>% group_by(year) %>% slice(1),
             aes(x=xs,y=ys), color="#a84878", shape=8, stroke=.25, size=.4) +
  
  
  # add years timeline -----------------------------
  annotate(geom="segment",
           x=800, y=84.5,
           xend = 2000, yend = 84.5,
           color = "grey30",
           size = .5,
           lineend = "round") +
  # add individual year ticks
  geom_segment(data = data.frame(x = c(800, 1000, 1200, 1400, 1600, 1800, 2000),
                                 y = 83.75,
                                 xend = c(800, 1000, 1200, 1400, 1600, 1800, 2000),
                                 yend = 84.5),
               aes(x=x,y=y,xend=xend,yend=yend),
               color = "grey30", size=.45, lineend = "round")+
  # add individual year labels
  geom_text(data = data.frame(x = c(800, 1000, 1200, 1400, 1600, 1800, 2000),
                              y = 84),
            aes(x=x,y=y,label=paste(x,"AD")),
            nudge_y = -1, color = "grey30", size=2.2,
            family = "Dancing Script") +
  
  # add dates boxes timeline  --------------------------
  annotate(geom = "rect", # feb
           xmin = 2050, xmax=2075,
           ymin = 32, ymax = 59,
           color = "#D8AEDD", fill = "#D8AEDD", alpha=.75) +
  annotate(geom = "rect",  # march
           xmin = 2050, xmax=2075,
           ymin = 60, ymax = 90,
           color = "#C39EDD", fill = "#C39EDD",alpha=.75) +
  annotate(geom = "rect",   # april
           xmin = 2050, xmax=2075,
           ymin = 91, ymax = 120,
           color = "#C781BD", fill = "#C781BD",alpha=.75) +
  annotate(geom = "rect",   # may
           xmin = 2050, xmax=2075,
           ymin = 121, ymax = 151,
           color = "#D889A4", fill = "#D889A4",alpha=.75) +
  
  # label months 
  annotate(geom="text",
           angle = -90, 
           x = 2063,
           y = 75,
           label = "March",
           vjust=.5,
           family = "Dancing Script",
           color="grey20"
  )+
  annotate(geom="text",
           angle = -90, 
           x = 2063,
           y = 105.5,
           label = "April",
           vjust=.5,
           family = "Dancing Script",
           color = "grey20")+
  
  # add text ---------------------------
  annotate(geom="text",
           x = 825,
           y = 80,
           label = "Seasons are Changing",
           vjust=1,
           hjust=0,
           size=24,
           family = "Dancing Script",
           color = "grey20")+
  geom_richtext(data = data.frame(x=820, y= 69,
                                  label = "long-term trend in <span style = 'color:#a84878'>peak bloom date</span> of cherry blossom trees in Kyoto City, Japan, years 801-2021 AD"),
                aes(x=x,y=y,label=label),
                vjust=1, hjust=0,
                fill=NA, label.color=NA,
                family = "Dancing Script",
                size=6,
                color="grey25")+
  
  # theme stuff
  #-------------------------------------------
  ggthemes::theme_few() +
  coord_cartesian(ylim = c(55,128),
                  xlim = c(726,2090),
                  #clip="off",
                  expand=F) +
  
  theme(axis.text  =element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(20,20,20,20,"pt"),
        axis.title = element_blank(),
        axis.ticks.length = unit(0, "pt"),
        panel.background = element_rect(fill ="#daeaf5",colour = "black",size=2)) +
  
  # add extra rectangle on top bc text boxes overlap panel background
  annotate(geom="rect",
           ymin = 55, ymax = 128,
           xmin = 726, xmax = 2090,
           colour = "black",size=2, fill= "transparent"  )


# view plot
plot

# save
ggsave(plot, filename = here("day_19","kyotoplot.png"),
       dpi = 350,
       width = 11.3,
       height = 7.1
       )

