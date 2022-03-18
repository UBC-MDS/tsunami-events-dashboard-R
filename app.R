library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(plotly)
library(dash)
library(dashHtmlComponents)

tsunami_events <- read.csv('data/processed/tsunami-events.csv')
country_codes <- read.csv("data/processed/country_codes.csv")

years <- unique(tsunami_events[['year']])
countries <- sort(unique(tsunami_events[['country']]))

app <- Dash$new(external_stylesheets = dbcThemes$QUARTZ)

# Creation of plots

# Map Plot
create_map_plot <- function(year_start, year_end, countries,
                            magnitude_start, magnitude_end) {
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
               year <= year_end,
               earthquake_magnitude >= magnitude_start,
               earthquake_magnitude <= magnitude_end)
    
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
                                  earthquake_magnitude,
                                  "\nEvent Year:",
                                  year),
                    hoverinfo = "text"
        )
    
    fig
}

create_scatter_plot <- function(
    year_start, year_end, countries, magnitude_start, magnitude_end
) {
    if (as.integer(year_start) > as.integer(year_end)) {
        stop("Invalid value for year start and/or year end")
    }
    
    if (typeof(countries) != "list") {
        stop("Invalid value for countries")
    }
    
    if (length(countries) == 0) {
        countries_subset <- NULL
        countries_subset <- tsunami_events %>%
            arrange(desc(earthquake_magnitude)) %>%
            pull(country) %>%
            unique() %>%
            head(10)
        tsunami_events <- tsunami_events %>%
            filter(country %in% countries_subset)
    } 
    else if (length(countries) > 10) {
        countries_subset <- NULL
        countries_subset <- tsunami_events %>%
            arrange(desc(earthquake_magnitude)) %>%
            filter(country %in% countries) %>%
            pull(country) %>%
            unique() %>%
            head(10)
        tsunami_events <- tsunami_events %>%
            filter(
                country %in% countries,
                country %in% countries_subset
            )
    }
    else if (length(countries) > 0) {
        tsunami_events <- tsunami_events %>%
            filter(country %in% countries)
        
    }
    
    tsunami_events_active <- tsunami_events %>%
        filter(
            year >= year_start,
            year <= year_end,
            earthquake_magnitude >= magnitude_start,
            earthquake_magnitude <= magnitude_end
        )
    tsunami_events_inactive <- tsunami_events %>%
        filter(
            year >= year_start,
            year <= year_end,
            (earthquake_magnitude < magnitude_start) | 
                (earthquake_magnitude > magnitude_end))
    
    p <- ggplot(tsunami_events_active) +
        geom_point(aes(x = earthquake_magnitude,
                       y = total_deaths,
                       color = country,
                       text = (paste("Country:", country,
                                     "<br>Location:", location_name,
                                     "<br>Tsunami Intensity:", tsunami_intensity,
                                     "<br>Earthquake Magnitude:", earthquake_magnitude,
                                     "<br>Year:", year,
                                     "<br>Month:", month)))) +
        geom_point(
            data = tsunami_events_inactive,
            aes(x=earthquake_magnitude, y=total_deaths),
            alpha = 0.1,
            size = 1) +
        ggthemes::scale_color_tableau() +
        theme_bw() +
        scale_y_log10(
            breaks = c(1, 10, 100, 1000, 10000, 100000),
            labels = c("1", "10", "100", "1000", "10000", "100000")
        ) +
        labs(
            x="Earthquake Magnitude (on Richter scale)",
            y="Total Deaths Recorded per Event \n(Log-Scaled)"
        ) +
        xlim(5.5, 10) +
        scale_colour_discrete("Countries (Up to Top 10)")
    
    
    ggplotly(p, tooltip = 'text')
}

create_bar_plot <- function(year_value, magnitude_value) {
    tsunami_events_active <- tsunami_events %>%
        filter(
            year >= year_value[1],
            year <= year_value[2],
            earthquake_magnitude >= magnitude_value[1],
            earthquake_magnitude <= magnitude_value[2])
    p <- ggplot(tsunami_events_active[order(-tsunami_events_active$tsunami_intensity),][1:10,], 
                aes(x = 1:10,
                    y = tsunami_intensity,
                    fill = country,
                    text = (paste("Country:", country,
                                  "<br>Location:", location_name,
                                  "<br>Tsunami Intensity:", tsunami_intensity,
                                  "<br>Earthquake Magnitude:", earthquake_magnitude,
                                  "<br>Year:", year,
                                  "<br>Month:", month)))) +
        geom_bar(stat = 'identity') +
        coord_flip() +
        ylim(0, 12) +
        xlab('Tsunami Instance') +
        ylab('Tsunami Intensity') +
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    p <- p + scale_fill_brewer(palette="Blues")
    ggplotly(p, tooltip = 'text')
}

# Navigation Bar
navbar <- dbcNavbar(
    dbcContainer(
        list(
                dbcRow(
                    list(
                        dbcCol(dbcNavbarBrand("Tsunami Events Dashboard", 
                                              style = list('font' = 'Helvetica', 'font-size' = '20px',
                                                           'font-weight' = '500')))
                    ),
                    align = "center",
                    className = "g-0"
                )
        ), style = list('margin-left' = '0')
    ),
    color = "dark",
    dark = TRUE
)

# Cards
world_plot_card <- dbcCard(
    dbcCardBody(list(
        htmlH6('Total Tsunami Hits by Country with Origin Points'),
        dccGraph(id = 'map_plot')
    )
    ), style = list('padding' = '0'),
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
        htmlH6('Top 10 Most Intense Tsunamis'),
        dccGraph(id = 'bar_chart')
    )
    )
)

# Sidebar
sidebar <- dbcCol(dbcRow(
    list(
        htmlP(" "),
        htmlP(" "),
        htmlH5('Years and Countries Selection',
               style = list("font" = "Helvetica", "font-size" = "20px",
                            "text-align" = "center")),
        htmlP(" "),
        htmlHr(),
        htmlH6('Years of Interest (1802 - 2022)',
               style = list('font' = 'Helvetica', 'font-size' = '14px',
                            'text-align' = 'center')),
        dccRangeSlider(
            id = 'year_slider',
            min=min(tsunami_events$year),
            max=max(tsunami_events$year),
            value= list(min(tsunami_events$year),
                        max(tsunami_events$year)),
            allowCross=FALSE,
            marks = list(
                "1800" = list('label' = "1800", "style" = list('color' = "#FFFFFF")),
                "1850" = list('label' = "1850", "style" = list('color' = "#FFFFFF")),
                "1900" = list('label' = "1900", "style" = list('color' = "#FFFFFF")),
                "1950" = list('label' = "1950", "style" = list('color' = "#FFFFFF")),
                "2000" = list('label' = "2000", "style" = list('color' = "#FFFFFF"))),
        ),
        htmlP(" "),
        htmlP(" "),
        htmlH6('Earthquake Magnitude of Interest',
               style = list('font' = 'Helvetica', 'font-size' = '14px',
                            'text-align' = 'center')),
        dccRangeSlider(
            id = 'magnitude_slider',
            min=min(tsunami_events$earthquake_magnitude),
            max=max(tsunami_events$earthquake_magnitude),
            value= list(min(tsunami_events$earthquake_magnitude),
                        max(tsunami_events$earthquake_magnitude)),
            allowCross=FALSE,
            marks = list(
                "4" = list('label' = "4", "style" = list('color' = "#FFFFFF")),
                "5" = list('label' = "5", "style" = list('color' = "#FFFFFF")),
                "6" = list('label' = "6", "style" = list('color' = "#FFFFFF")),
                "7" = list('label' = "7", "style" = list('color' = "#FFFFFF")),
                "8" = list('label' = "8", "style" = list('color' = "#FFFFFF")),
                "9" = list('label' = "9", "style" = list('color' = "#FFFFFF")))
        ),
        htmlP(" "),
        htmlP(" "),
        htmlH6('Countries of Interest', 
               style = list('font' = 'Helvetica', 'font-size' = '14px',
                            'text-align' = 'center')),
        dccDropdown(
            id = 'country_select',
            multi = TRUE,
            value = list(),
            options = countries,
            className = 'text-dark'),
        htmlBr(),
        htmlBr(),
        htmlBr(),
        htmlHr(),
        htmlP(
            "A data visualisation app that allows viewers to observe the number and intensity of tsunamis based on years and countries",
            style = list('font' = 'Helvetica', 'font-size' = '14px',
                         'text-align' = 'center'))
    ))
    )

# Card Arrangement

cards <- list(
    dbcRow(world_plot_card,
           style = list('padding' = '12px')),
    dbcRow(
        list(
            dbcCol(scatter_plot_card, width = 6,
                   style = list('padding-right' = '6px',
                                'padding-bottom' = '6px')),
            dbcCol(bar_chart_card, width = 6,
                   style = list('padding-left' = '6px',
                                'padding-bottom' = '6px'))
        )
    )
)



app$layout(dbcContainer(
    list(
        navbar,
        dbcRow(
            list(
                dbcCol(sidebar, 
                       width=2,
                       style = list(
                           'backgroundColor' = '#484848',
                           'border-width' = '0',
                           'padding' = '20px')),
                dbcCol(cards, width=10)
            ))
    ),
    style = list("width" = "100%", 
                 "max-width" = "100%",
                 'backgroundColor' = '#191919')
))

#App callback for world_map_plot
app$callback(
    output('map_plot', 'figure'),
    list(input('year_slider', 'value'),
         input('magnitude_slider', 'value'),
         input('country_select', 'value')),
    function(years, magnitude, countries) {
        create_map_plot(year_start = years[1],
                        year_end = years[2],
                        countries = countries,
                        magnitude_start = magnitude[1],
                        magnitude_end = magnitude[2])
    }
)

# App callback for scatter_plot
app$callback(
    output('scatter_plot', 'figure'),
    list(input('year_slider', 'value'),
         input('magnitude_slider', 'value'),
         input('country_select', 'value')),
    function(years, magnitude, countries) {
        create_scatter_plot(year_start = years[1],
                            year_end = years[2],
                            countries = countries,
                            magnitude_start = magnitude[1],
                            magnitude_end = magnitude[2])
    }
)

# App callback for bar_chart
app$callback(
    output('bar_chart', 'figure'),
    list(input('year_slider', 'value'),
         input('magnitude_slider', 'value')),
    function(year_value, magnitude_value){
        create_bar_plot(year_value, magnitude_value)
    }
)

app$run_server(host = '0.0.0.0')
