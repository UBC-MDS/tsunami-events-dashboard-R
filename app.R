library(dash)
library(devtools) # contains dashBootstrapComponents
library(purrr)
library(plotly)
library(ggplot2)
library(readr)
library(dashHtmlComponents)
library(dplyr)

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")


tsunami_events <- read_csv("data/processed/tsunami-events.csv")


app$layout(
    dbcContainer(
        list(
          dccGraph(id = 'plot_bar'),
        dccRangeSlider(
        id = 'year_slider',
        min=min(tsunami_events$year),
        max=max(tsunami_events$year),
        value= list(min(tsunami_events$year), max(tsunami_events$year)),
        allowCross=FALSE,
        marks = list(
          "1802" = "1802",
          "1850" = "1850",
          "1900" = "1900",
          "1950" = "1950",
          "2000" = "2000",
          "2022" = "2022")))))

app$callback(
  output('plot_bar', 'figure'),
  list(input('year_slider', 'value')),
  function(year_value) {
    new_df <- tsunami_events %>% subset(year >= year_value[1] & year <= year_value[2])
    p <- ggplot(new_df[order(-new_df$tsunami_intensity),][1:10,], 
                aes(label_0 = country,
                    label = location_name, 
                    label2 = earthquake_magnitude,
                    label3 = year,
                    label4 = month)) +
      geom_col(aes(x = 1:10,
                     y = tsunami_intensity,
                     color = country,
                      fill = country)) +
      ylim(0, 12) +
      xlab('Tsunami Instance') +
      ylab('Tsunami Intensity') +
      ggtitle('Top 10 Most Intense Tsunamis') +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    ggplotly(p)
  }
)

app$run_server(debug = T)
