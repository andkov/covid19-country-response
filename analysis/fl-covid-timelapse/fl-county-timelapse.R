# covid19_data <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# reproducing the animation from
# https://www.r-bloggers.com/tracking-covid19-cases-throughout-nj-with-r/


library(dplyr) # used for data wrangling
library(ggplot2)

FL_covid19 <- covid19_data%>%
  dplyr::filter(state == "Florida",county != "Unknown") #%>%  #filters data frame
  # tibble::as_tibble()

head(FL_covid19) # Returns the first or last parts of the data frame
## # A tibble:

florida_counties_map <- ggplot2::map_data("county") %>%
  dplyr::filter(region == "florida") %>%
  dplyr::mutate_at(
    "subregion"
    , ~stringr::str_replace_all(
      .
      ,c(
        "de soto" = "desoto"
        ,"st johns" ="saint johns"
        ,"st lucie" = "saint lucie"
      )
    )
  ) #%>%
  # tibble::as_tibble()
head(florida_counties_map)

ds <- FL_covid19 %>%
  dplyr::mutate(
    county = tolower(county)
  ) %>%
  dplyr::right_join(florida_counties_map,by = c( "county" = "subregion"))



g1 <-  ds %>%
  # dplyr::filter(date == "2020-04-08") %>%
  ggplot(aes_string(x = "long", y = "lat", group = "group", fill = "deaths")) +
  geom_polygon(color = "black") +
  coord_map() +
  theme_void() +
  scale_fill_viridis_c(option = "magma", direction = -1, label = scales::label_comma()) +
  theme(
    legend.position = c(.3, .5)
  )+
  # facet_wrap(~year)+
  labs(
    # title = plot_title
    # ,fill = var_names_labels[metric]
  ) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        legend.background = element_blank(),
        legend.position=c(-0.3,0.8),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(colour="black", size=12, face="bold"),
        plot.title=element_text(size=20, face="bold",hjust =0.5),
        plot.subtitle = element_text(hjust = 0.5,size=12),
        plot.caption = element_text(size = 11,
                                    hjust = .5,
                                    color = "black",
                                    face = "bold"))
  gganimate::transition_manual(date)

gganimate::animate(g1, nframe=27,fps = 2, end_pause = 15,height = 500, width =500)


 g1


library(ggplot2) #Used for plotting
library(gganimate) #Used for animations
library(RColorBrewer) #Used for color scale

# Used to make new data frame an sf object
# Must use st_as_sf in order to use geom_sf() to plot polygons
# NJ_covid19_shapes<-st_as_sf(NJ_covid19_shapes)
# Makes plot with ggplot2 and gganimate to animate through the days
covid_map<-ggplot()+
  geom_sf(data = NJ_counties,fill = "white")+
  geom_sf(data = NJ_covid19_shapes,aes(fill=cases))+
  ggtitle("Spread of Covid19 Throughout New Jersey")+
  xlab("")+
  ylab("")+
  labs(subtitle = "Date: {current_frame}",
       caption = "Date Source: The New York Times\nAuthor: Kevin Zolea")+
  cowplot::background_grid(major = "none", minor = "none") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        legend.background = element_blank(),
        legend.position=c(-0.3,0.8),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(colour="black", size=12, face="bold"),
        plot.title=element_text(size=20, face="bold",hjust =0.5),
        plot.subtitle = element_text(hjust = 0.5,size=12),
        plot.caption = element_text(size = 11,
                                    hjust = .5,
                                    color = "black",
                                    face = "bold"))+
  scale_fill_distiller("Number of Positive Cases",
                       palette ="Reds",type = "div",
                       direction = 1)+
  transition_manual(date)

animate(covid_map, nframe=27,fps = 2, end_pause = 15,height = 500, width =500)
