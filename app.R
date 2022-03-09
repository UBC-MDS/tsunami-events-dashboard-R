library(dash)
library(dashHtmlComponents)
library(devtools) # contains dashBootstrapComponents
library(purrr)
library(plotly)
library(ggplot2)

tsunami_df = read.csv('data/processed/tsunami-events.csv')

years = unique(tsunami_df[['year']])
countries = unique(tsunami_df[['country']])
country_list=list(sort(countries))

app = Dash$new(external_stylesheets = dbcThemes$QUARTZ)

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
      htmlH6('Years of Interest (1802 - 2022)', className='form-label'),
      dccRangeSlider(
        id = 'year_slider',
        min=min(tsunami_df$year),
        max=max(tsunami_df$year),
        value= list(min(tsunami_df$year), max(tsunami_df$year)),
        allowCross=FALSE,
        marks = list(
          "1802" = "1802",
          "1850" = "1850",
          "1900" = "1900",
          "1950" = "1950",
          "2000" = "2000",
          "2022" = "2022")),
      htmlBr(),
      htmlBr(),
      htmlH6('Countries of Interest', className='form-label')
      # dccDropdown(
      #   id = 'country_select',
      #   multi = TRUE,
      #   value = list(),
      #   options = country_list %>%
      #               purrr::map(function(country) list(label = country, value = country)),
      #               className = 'text-dark')
    ),
    style = list(
      color = 'black',
      textAlign = 'center'
      # backgroundcolor = 'rgba(255,255,255,.25)'
    )
    ),
    dbcCol(list(
      world_plot_card,
      htmlBr(),
      htmlBr(),
      dbcRow(list(
        scatter_plot_card,
        bar_chart_card
      )
          ))))
  )))  
  )


#App callback for world_map_plot
app$callback(
  output('map_plot', 'figure'),
  list(input('year_slider', 'value'),
       input('country_select', 'value')),
  # function() {
  #   ...
  # }
)

#App callback for scatter_plot
app$callback(
  output('scatter_plot', 'figure'),
  list(input('year_slider', 'value'),
       input('country_select', 'value')),
  # function() {
  #   ...
  # }
)

#App callback for scatter_plot
app$callback(
  output('bar_chart', 'figure'),
  list(input('year_slider', 'value')),
  # function() {
  #   ...
  # }
)

app$run_server(debug=T)
