library(dash)
library(dashHtmlComponents)

app = Dash$new()

app$layout(htmlDiv('Hello world!'))

app$run_server(debug = T)