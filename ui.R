# Hegselmann & Krause's Bounded Confidence Model
# << UI >>
# by Aydin Mohseni

# Load the shiny GUI library
library(shiny)
library(ggplot2)
library(ggthemes)

# Set encoding for special characters
Sys.setlocale("LC_ALL", "fr_FR.UTF-8")

# Define UI for application
shinyUI(fluidPage(
  # CSS for visual
  includeCSS("www/style.css"),
  
  # Title
  titlePanel("Bounded Confidence Model of Opinion Dynamics"),
  
  # Load MathJax
  withMathJax(),
  fluidRow(style = "background-color:#F2F2F2; margin-top: 30px; margin-bottom: 30px; padding: 10px",
           column(
             width = 6,
             # Introduction text:
             p(
               tags$b("Description."),
               "When does opinion formation within an interacting group lead to consensus, polarization, or fragmentation? The Hegselmann-Krausse bounded confidence model of opinion dynamics (2002) offers one attempt to explore this question."
             ),
             p("The dynamics is intuitive. Individuals begin with the opinions of \\(N\\) uniformly distributed across the unit interval \\((0,1)\\). At each time step \\(t\\), each individual \\(i\\) produces her opinion for the next time step \\(x_i(t+1)\\) by taking the average of the opinions of individuals (including herself) whose opinions differ from her own by no more than a certain confidence level \\(\\epsilon_i\\). That is, the dynamics are given by"),
             p("\\( \\displaystyle x_i(t+1)=|I(i,x(t))|^{-1} \\sum_{j \\in I(i,x(t))} x_j(t)\\),", align = "center"),
             p("where \\(I(i,x) = \\{1 \\leq j \\leq n: |x_i - x_j| \\leq \\epsilon_i \\} \\).")
           ),
           column(
             width = 6,
             # Introduction continued:
             p("In this way, the dynamics proceeds until the population of individuals converges to one of three steady states: (\\(i\\)) consensus, where all individuals arrive at a single opinion; (\\(ii\\)) polarization, where two opinions persist; and (\\(iii\\)) fragmentation, where multiple opinions persist."),
             p("One lesson from the model is an old one: complex dynamics can emerge from even the simplest assumptions."),
             br(),
             br(),
             # Refernces:
             tags$b("Citations"),
             p(
               "Hegselmann, R. & Krause, U. (2002). 'Opinion Dynamics and Bounded Confidence Models, Analysis, and Simulation'. Journal of Artifical Societies and Social Simulation (JASS). vol.5, no. 3."
             )
           )),
  
  # Sidebar for Parameter Input
  sidebarLayout(
    sidebarPanel(
      # Use MathJax
      withMathJax(),
      
      # Default radius of confidence interval
      sliderInput(
        "confidenceInterval",
        "Confidence interval (\\(\\epsilon\\)):",
        min = 0,
        max = 1,
        value = 0.2,
        step = 0.01
      ),
      
      # Bias in favor of right opinions
      sliderInput(
        "highBias",
        "High/low bias of confidence interval (\\(b\\)):",
        min = -1,
        max = 1,
        value = 0,
        step = 0.1
      ),
      
      # Number of agents
      sliderInput(
        "numberOfAgents",
        "Number of agents (\\(N\\)):",
        min = 1,
        max = 100,
        value = 100
      ),
      
      # Number of rounds of the simulation
      sliderInput(
        "numberOfRounds",
        "Number of interations of dyamics (\\(T\\)):",
        min = 1,
        max = 15,
        value = 10
      )
      
    ),
    
    # Main Panel with Plot
    mainPanel(style = "padding: 20px; margin-bottom: 0px; margin-top: -45px;",
              plotOutput("OpinionDynamicsPlot", height = "600px"))
  )
))