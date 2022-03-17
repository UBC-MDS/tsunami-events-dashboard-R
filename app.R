library(dash)
library(dashHtmlComponents)
library(purrr)
library(plotly)
library(ggplot2)
library(dplyr)

years = unique(tsunami_df[['year']])
countries = sort(unique(tsunami_df[['country']]))


tsunami_events <- read.csv("data/processed/tsunami-events.csv")
country_codes <- read.csv("data/processed/country_codes.csv")

years = unique(tsunami_events[['year']])
countries = sort(unique(tsunami_events[['country']]))

app = Dash$new(external_stylesheets = dbcThemes$QUARTZ)

create_scatter_plot <- function(year_start = years[1], 
                                year_end = years[2], 
                                countries = countries, 
                                magnitude_start = magnitude[1], 
                                magnitude_end = magnitude[2]) {
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

create_bar_plot <- function(year_value, magnitude_value=c(8,9)) {
    new_df <- tsunami_events %>% subset(year >= year_value[1] & year <= year_value[2])
    new_df <- tsunami_events %>% subset(earthquake_magnitude >= magnitude_value[1] & earthquake_magnitude <= earthquake_magnitude[2])
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
        geom_bar(stat = 'identity') +
        coord_flip() +
        ylim(0, 12) +
        xlab('Tsunami Instance') +
        ylab('Tsunami Intensity') +
        ggtitle('Top 10 Most Intense Tsunamis') +
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    p <- p + scale_fill_brewer(palette="Blues")
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
                htmlH6('Earthquake Magnitude of Interest',
                       className='form-label'),
                dccRangeSlider(
                    id = 'magnitude_slider',
                    min=min(tsunami_events$earthquake_magnitude),
                    max=max(tsunami_events$earthquake_magnitude),
                    value= list(min(tsunami_events$earthquake_magnitude),
                                max(tsunami_events$earthquake_magnitude)),
                    allowCross=FALSE,
                    marks = list(
                        "4" = "4",
                        "5" = "5",
                        "6" = "6",
                        "7" = "7",
                        "8" = "8",
                        "9" = "9",
                        "9.5" = "9.5"),
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
        create_map_plot(year_start = years[1],
                        year_end = years[2],
                        countries = countries,
                        magnitude_start = 8,
                        magnitude_end = 9)
    }
)

# App callback for scatter_plot
app$callback(
    output('scatter_plot', 'figure'),
    list(input('year_slider', 'value'),
         input('magnitude_slider', 'value'),
         input('country_select', 'value')),
         #input('magnitude_slider', 'value')),
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
