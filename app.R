#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

Sys.setlocale("LC_ALL", "es_CO.UTF-8")
library(shiny)
library(thematic)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse) 
library(effectsize)
library(faux)
library(plyr)
library(scales)

input <<- tibble(
  alts = "Group 1 ≠ Group 2",
  mean1 = 180.1, mean2 = 203.3,
  sd1 = 32.1, sd2 = 24.8,
  reps = 1000,
  sample_size = 40,
  alpha = 0.05,
  corrxy = 0.1
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),
  
  # Application title
  titlePanel(title = tags$link(rel = "icon",
                               type = "image",
                               href = "https://image.pngaaa.com/393/402393-middle.png"),
             "PowerSimulate: Prueba t pareada"),
  HTML("<center><a href='https://shiny.jdl-svr.lat/PowerSimulate'><img src='powersimulate.svg'' width='600'></a></center>"),
  tags$h3(HTML("<center>Prueba <em>t</em> pareada</center>")),
  p(HTML("<center>Código disponible en 
      <a style=color:#ff5555;  href='https://github.com/JDLeongomez/PowerSimulate_pair_t_ES'>GitHub</a>
      - Creado por
      <a style=color:#ff5555;  href='https://jdleongomez.info/es/'>Juan David Leongómez</a>, Universidad El Bosque
      · 2023 · <a style=color:#4075de;  href='https://shiny.jdl-svr.lat/PowerSimulate_pair_t_EN/'>
      English version</a> 
      · <a style=color:#ff5555;  href='https://shiny.jdl-svr.lat/PowerSimulate_corr_ES'>PowerSimulate: Correlación.</a></center>")),
  hr(),
  p(HTML("<center>Análisis de poder estadístico basado en la simulación de una población y 
         la probabilidad de obtener un resultado significativo con una muestra aleatoria de 
         un tamaño determinado.<br>Aunque existen herramientas más directas para el análisis de 
         poder en el caso de las pruebas <em>t</em>, esta aplicación se basa en simulaciones 
         para ilustrar el concepto de poder estadístico.</center>")),
  fluidRow(
    column(2,
           tags$h2("Parámetros de las condiciones"),
           tags$h4("Condición 1"),
           textInput(inputId = "label1",
                     label = "Etiqueta de la condición 1",
                     value = "Pre-test",
                     width = '300px'),
           numericInput(inputId = "mean1",
                        label = "Media",
                        min = -Inf,
                        max = Inf,
                        value = 180.1,
                        step = 0.0001,
                        width = '300px'),
           numericInput(inputId = "sd1",
                        label = "Desviación estándar",
                        min = -Inf,
                        max = Inf,
                        value = 32.1,
                        step = 0.0001,
                        width = '300px'),
           hr(),
           tags$h4("Condición 2"),
           textInput(inputId = "label2",
                     label = "Etiqueta de la condición 1",
                     value = "Post-test",
                     width = '300px'),
           numericInput(inputId = "mean2",
                        label = "Media",
                        min = -Inf,
                        max = Inf,
                        value = 203.3,
                        step = 0.0001,
                        width = '300px'),
           numericInput(inputId = "sd2",
                        label = "Desviación estándar",
                        min = -Inf,
                        max = Inf,
                        value = 32.8,
                        step = 0.0001,
                        width = '300px')
    ),
    column(4,
           tags$h1("Tamaño del efecto en la población"),
           tags$h4("Correlación entre condiciones"),
           fluidRow(
             column(6,
                    tags$h6(HTML("<b style=color:#ff5555;>NOTA:</b> En un diseño de muestras 
                        pareadas, cuanto mayor sea la correlación entre las variables (condiciones), 
                        mayor será la potencia de la prueba. Si la correlación es 0, el poder de 
                        una prueba de muestras pareadas es idéntico a la de una 
                        <a style=color:#ff5555;  href='https://github.com/JDLeongomez/PowerSimulate_ind_t_EN'>prueba <em>t</em> independiente</a> 
                        con los mismos parámetros."))),
             column(6,
                    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background:#ff5555}")),
                    sliderInput(inputId = "corrxy",
                                label = "Coeficiente de correlación (Pearson)",
                                min = -1,
                                max = 1,
                                value = 0.25,
                                step = 0.01,
                                width = 'auto'))),
           tags$h3("Si esta fuera la diferencia en la población"),
           plotOutput("effectPlot") %>% 
             withSpinner(color = "#ff5555"),
           tags$h6(HTML("<b style=color:#ff5555;>NOTA:</b> La <em>d</em> de Cohen es el 
                       tamaño del efecto más común para diferencias estandarizadas entre 
                       dos medias. Sin embargo, tiende a proporcionar estimaciones 
                       sesgadas cuando se tienen tamaños de muestra pequeños. Por este 
                       motivo, la <em>g</em> de Hedges es una alternativa valiosa.")),
    ),
    column(2,
           tags$h2("Parámetros de simulación"),
           tags$h4("Tamaño de muestra"),
           tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background:#ff5555}")),
           sliderInput(inputId = "sample_size",
                       label = "Tamaño de muestra (observaciones pareadas)",
                       min = 5,
                       max = 1000,
                       value = 30,
                       step = 1,
                       width = '300px'),
           tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background:#ff5555}")),
           sliderInput(inputId = "alpha",
                       label = HTML("Nivel de significación (típicamente &alpha; = 0.05)"),
                       min = 0,
                       max = 1,
                       value = 0.05,
                       step = 0.001,
                       width = '300px'),
           selectInput(inputId = "alts",
                       label = "Hipótesis",
                       choices = c("Condición 1 ≠ Condición 2", 
                                   "Condición 1 > Condición 2",
                                   "Condición 1 < Condición 2"
                       )),
           numericInput(inputId = "reps",
                        label = HTML("Número de simulaciones:
                                     <span style='font-weight:normal'>Por defecto se ejecutan sólo 100 simulaciones, 
                                     pero una vez que hayas comprobado todos los parámetros, te sugiero que ejecutes 
                                     1000 o más simulaciones para aumentar la precisión (entre más simulaciones hagas, 
                                     más tiempo tomará).</span>"),
                        min = 1,
                        max = 1000000,
                        value = 100,
                        step = 1,
                        width = '300px'),
           nextGenShinyApps::submitButton("runSim", text = "¿Todo listo? ¡Corre la simulación!", 
                                          icon("paper-plane"), bg.type = "danger")
    ),
    column(4,
           tags$h1("Poder estadístico"),
           tags$h3("Este es el poder estadístico que alcanzarías"),
           plotOutput("powerPlot") %>% 
             withSpinner(color = "#ff5555"),
           htmlOutput("powText")
    )
  )
)

server <- function(input, output, session) {
  
  # Simulate population
  dat <- reactive({
    datos <- rnorm_multi(
      n = 100000, 
      vars = 2, 
      r = input$corrxy, 
      mu = c(input$mean1, input$mean2), 
      sd = c(input$sd1, input$sd2), 
      varnames = c("A", "B")
    )
    return(datos)
  })
  
  # Calculate effect sizes
  cohen.d <- reactive({
    coh.d <- cohens_d(x = dat()$A, y = dat()$B,
                      pooled_sd = FALSE,
                      paired = TRUE,
                      ci = 0.95)
    return(coh.d)
  })
  hedges.g <- reactive({
    hed.g <- hedges_g(x = dat()$A, y = dat()$B,
                      pooled_sd = FALSE,
                      paired = TRUE,
                      ci = 0.95)
    return(hed.g)
  })   
  
  # Create normal distributions with input means and SDs
  dat.dist <- reactive({
    x = seq(min(dat()), max(dat()), length = 200)
    dat.distri <- data.frame(A = dnorm(x, mean = input$mean1, sd = input$sd1),
                             B = dnorm(x, mean = input$mean2, sd = input$sd2), x = x) %>%
      pivot_longer(cols = A:B, names_to = "Condición", values_to = "Value")
    return(dat.distri)
  })
  
  # Population distribution plot 
  output$effectPlot <- renderPlot({
    ggplot(data = dat.dist(), aes(x = x, fill = Condición)) +
      geom_polygon(aes(y = Value), alpha = 0.8) +
      xlab("Valor") + ylab("Densidad de probabilidad") + 
      geom_vline(aes(xintercept = input$mean1, color = "white"),
                 linetype="dashed",
                 show.legend = FALSE) +
      geom_vline(aes(xintercept = input$mean2, color = "white"),
                 linetype="dashed",
                 show.legend = FALSE) +
      scale_fill_manual(values = c("#4075de", "#ff5555"),
                        labels = c(input$label1, input$label2)) +
      annotate("text", x = min(dat.dist()$x), y = Inf, 
               hjust = 0, vjust = 2, size = 7,
               label = paste0("d de Cohen = ", round(abs(cohen.d()$Cohens_d), 2))) +
      annotate("text", x = min(dat.dist()$x), y = Inf, 
               hjust = 0, vjust = 6,
               label = paste0("g de Hedges = ", round(abs(hedges.g()$Hedges_g), 2))) +
      geom_segment(aes(x = input$mean1, y = max(dat.dist()$Value)*0.5, 
                       xend = input$mean2, yend = max(dat.dist()$Value)*0.5), 
                   arrow = arrow(length = unit(0.02, "npc"), ends = "both")) +
      annotate("text", x = Inf, y = Inf, 
               hjust = 1.1, vjust = 2, size = 5,
               label = paste0("Diferencia de medias = ", round(abs(input$mean1 - input$mean2), 2))) +
      annotate("text", x = Inf, y = Inf, 
               hjust = 1.6, vjust = 6,
               label = paste0("r = ", input$corrxy)) +
      theme(legend.position="bottom", 
            legend.title=element_text(size=14),
            legend.text = element_text(size = 12))
  })
  
  # Create object with selected hypothesis alternative
  altern <<- reactive({
    dplyr::case_when(
      input$alts == "Condición 1 ≠ Condición 2" ~ "two.sided",
      input$alts == "Condición 1 > Condición 2" ~ "greater",
      TRUE ~ "less")
  })
  
  sig.lev <<- reactive({
    input$alpha
  })
  
  # Simulate samples and test significance in each
  dat.sim <- reactive({
    req(input$alts)
    dato <- ddply(map_dfr(seq_len(input$reps), ~dat() %>%
                            sample_n(input$sample_size) %>%
                            mutate(sample = as.factor(.x))),
                  .(sample),
                  summarise,
                  p = round(t.test(x = A, y = B,
                                   alternative = altern(), 
                                   paired = TRUE)$p.value, 3),
                  "Significación" = ifelse(p <= sig.lev(), "Significativo", "No significativo"))
    return(dato)
  })
  
  # Power simulation plot 
  output$powerPlot <- renderPlot({
    ggplot(dat.sim(), aes(x = p, fill = Significación)) +
      scale_fill_hue(direction = -1) +
      geom_histogram(bins = 1/input$alpha, breaks = seq(0, 1, input$alpha), alpha = 0.8) +
      scale_fill_manual(values = c("#4075de", "#ff5555")) +
      labs(y = "Conteo", x = "Valor p") +
      scale_x_continuous(breaks = pretty_breaks(n = 20)) +
      annotate("text", x = 0.5, y = Inf, size = 7, vjust = 2,
               label = paste0("Poder (1 - β) = ", round(sum(dat.sim()$Significación == "Significativo") / input$reps, 2))) +
      annotate("text", x = 0.5, y = Inf, vjust = 5,
               label = paste0("Tamaño de muestra = ", input$sample_size)) +
      annotate("text", x = 0.5, y = Inf, vjust = 6.5,
               label = paste0("α = ", input$alpha)) +
      theme(legend.position="bottom", 
            legend.title=element_text(size=14),
            legend.text = element_text(size = 12)) +
      guides(fill = guide_legend(reverse=TRUE))
  })
  
  output$powText <- renderText({
    paste("<b style=color:#ff5555;>INTERPRETACIÓN: </b>
          El poder no es más que la proporción de resultados significativos 
          (<em>p</em> < α). Así, si la diferencia real en la población fuera la especificada, con una 
          muestra aleatoria de <font color=\'#ff5555\'><b><em>n</em> = ", input$sample_size, "</b></font>, 
          obtendrías un resultado significativo en aproximadamente el <font color=\'#ff5555\'><b>", 
          percent(round(sum(dat.sim()$Significación == "Significativo") / input$reps, 2)),
          "</b></font> de los casos.")
  })
}

# Same theme for plots
thematic_shiny()

# Run the application 
shinyApp(ui = ui, server = server)
