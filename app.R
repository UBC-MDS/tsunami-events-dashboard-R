library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(plotly)
library(dash)
library(dashHtmlComponents)
library(dashBootstrapComponents)


tsunami_events <- read.csv('data/processed/tsunami-events.csv')
country_codes <- read.csv("data/processed/country_codes.csv")

years <- unique(tsunami_events[['year']])
countries <- sort(unique(tsunami_events[['country']]))

app <- Dash$new(external_stylesheets = dbcThemes$QUARTZ)

create_map_plot <- function(year_start, year_end, countries) {
    if (as.integer(year_start) > as.integer(year_end)) {
        stop("Invalid value for year start and/or year end")
    }
    
    if (typeof(countries) != "list") {
        stop("Invalid value for countries")
    }
    
    if (length(countries) > 0) {
        tsunami_events <- tsunami_events %>%
            filter(country %in% countries)
    }
    
    tsunami_events <- tsunami_events %>%
        filter(year >= year_start,
               year <= year_end)
    
    counts <- tsunami_events %>%
        group_by(country) %>%
        summarise(count = n())
    
    counts <- right_join(
        counts,
        country_codes,
        by = c("country" = "name")
    ) %>%
        mutate_at(vars(count), ~replace_na(., 0))
    
    l <- list(color = toRGB("grey"), width = 0.5)
    g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'Mercator')
    )
    
    fig <- plot_geo(counts)
    
    colorscale <- data.frame(
        z =  c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.4, 0.8, 1.0),
        col = brewer.pal(9, "Blues")
    )
    
    fig <- fig %>%
        add_trace(
            z = ~count, color = ~count, colors = 'Blues',
            text = ~paste("Country:", country, "\nHits:", count),
            locations = ~alpha.3,
            marker = list(line = l),
            colorscale = colorscale,
            colorbar = list(title = "Tsunami Hits"),
            hoverinfo = "text",
            zmin = 1,
            zmax = max(counts$count)
        )  %>%
        layout(geo = g) %>%
        add_markers(data = tsunami_events, y = ~latitude, x = ~longitude,
                    size = 2, marker = list(size = 2, opacity=0.4, 
                                            color = "red"),
                    text = ~paste("Earthquake Magnitude:",
                                  earthquake_magnitude),
                    hoverinfo = "text"
        )
    
    fig
}

create_scatter_plot <- function(year_start, year_end, countries) {
    if (as.integer(year_start) > as.integer(year_end)) {
        stop("Invalid value for year start and/or year end")
    }
    
    if (typeof(countries) != "list") {
        stop("Invalid value for countries")
    }
    
    if (length(countries) == 0) {
        countries_subset <- NULL
        countries_subset <- tsunami_events |>
            arrange(desc(earthquake_magnitude)) |>
            pull(country) |>
            unique() |>
            head(10)
        tsunami_events <- tsunami_events %>%
            filter(country %in% countries_subset)
    } 
    else if (length(countries) > 10) {
        countries_subset <- NULL
        countries_subset <- tsunami_events |>
            arrange(desc(earthquake_magnitude)) |>
            filter(country %in% countries) |>
            pull(country) |>
            unique() |>
            head(10)
        tsunami_events <- tsunami_events %>%
            filter(country %in% countries)
        tsunami_events <- tsunami_events %>%
            filter(country %in% countries_subset)
    }
    else if (length(countries) > 0) {
        tsunami_events <- tsunami_events %>%
            filter(country %in% countries)
    }
    
    tsunami_events <- tsunami_events %>%
        filter(year >= year_start,
               year <= year_end)
    
    p <- ggplot(tsunami_events) +
        aes(x = earthquake_magnitude,
            y = total_deaths,
            color = country) +
        geom_point() +
        ggthemes::scale_color_tableau() +
        theme_bw() +
        scale_y_log10(
            breaks = c(1, 10, 100, 1000, 10000, 100000),
            labels = c("1", "10", "100", "1000", "10000", "100000")
            
        ) +
        labs(
            x="Earthquake Magnitude (on Richter scale)",
            y="Total Deaths Recorded per Event \n(log-transformed)"
        ) +
        xlim(5.5, 10)
    ggplotly(p)
}

create_bar_plot <- function(year_value) {
    new_df <- tsunami_events %>% subset(year >= year_value[1] & year <= year_value[2])
    p <- ggplot(new_df[order(-new_df$tsunami_intensity),][1:10,], 
                aes(x = 1:10,
                    y = tsunami_intensity,
                    color = country,
                    fill = country,
                    text = (paste("Country:", country,
                                  "<br>Location:", location_name,
                                  "<br>Tsunami Intensity:", tsunami_intensity,
                                  "<br>Earthquake Magnitude:", earthquake_magnitude,
                                  "<br>Year:", year,
                                  "<br>Month:", month)))) +
      geom_col() +
      ylim(0, 12) +
      xlab('Tsunami Instance') +
      ylab('Tsunami Intensity') +
      ggtitle('Top 10 Most Intense Tsunamis') +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    ggplotly(p, tooltip = 'text')
  }

navbar = dbcNavbar(
    dbcContainer(
      list(
        htmlA(
          dbcRow(
            list(
              dbcCol(dbcNavbarBrand('Tsunami Events Dashboard'))
            ),
            align = 'center',
            className = 'g-0'
          )
        ),
        dbcNavbarToggler(id = 'navbar-toggler', n_clicks = 0),
        dbcCollapse(
          id = 'navbar-collapse',
          is_open = FALSE,
          navbar = TRUE,
      )
    )
  ),
  color = 'dark',
  dark = TRUE
)

world_plot_card <- dbcCard(
    dbcCardBody(list(
        htmlH6('Total Tsunami Hits by Country with Origin Points'),
        dccGraph(id = 'map_plot')
    )
    )
)

scatter_plot_card <- dbcCard(
  dbcCardBody(list(
    htmlH6('Total Deaths and Earthquake Magnitude per Event'),
    dccGraph(id = 'scatter_plot')
  )
  )
)

bar_chart_card <- dbcCard(
  dbcCardBody(list(
    htmlH6('10 Most Intense Tsunamis by Country'),
    dccGraph(id = 'bar_chart')
  )
  )
)

app$layout(dbcContainer(
    list(
        navbar,
        dbcRow(list(
            dbcCol(list(
                htmlH5('Years and Countries Selection', className='form-label'),
                htmlHr(),
                htmlH6('Years of Interest (1802 - 2022)',
                       className='form-label'),
                dccRangeSlider(
                    id = 'year_slider',
                    min=min(tsunami_events$year),
                    max=max(tsunami_events$year),
                    value= list(min(tsunami_events$year),
                                max(tsunami_events$year)),
                    allowCross=FALSE,
                    marks = list(
                        "1802" = "1802",
                        "1850" = "1850",
                        "1900" = "1900",
                        "1950" = "1950",
                        "2000" = "2000",
                        "2022" = "2022"),
                ),
                htmlBr(),
                htmlBr(),
                htmlH6('Countries of Interest', className='form-label'),
                dccDropdown(
                    id = 'country_select',
                    multi = TRUE,
                    value = list(),
                    options = countries,
                    className = 'text-dark')
            ), width = 4),
            dbcCol(list(
                world_plot_card,
                htmlBr(),
                htmlBr(),
                dbcRow(list(
                    scatter_plot_card,
                    bar_chart_card
                ))
            ), width = 8)
        ))
    )
))

#App callback for world_map_plot
app$callback(
    output('map_plot', 'figure'),
    list(input('year_slider', 'value'),
         input('country_select', 'value')),
    function(years, countries) {
        create_map_plot(years[1], years[2], countries)
    }
)

# App callback for scatter_plot
app$callback(
    output('scatter_plot', 'figure'),
    list(input('year_slider', 'value'),
         input('country_select', 'value')),
    function(years, countries) {
        create_scatter_plot(years[1], years[2], countries)
    }
)

# App callback for bar_chart
app$callback(
  output('bar_chart', 'figure'),
  list(input('year_slider', 'value')),
  function(year_value){
      create_bar_plot(year_value)
  }
)

app$run_server(host = '0.0.0.0')