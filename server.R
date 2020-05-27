# Hegselmann & Krause's Bounded Confidence Model
# << SERVER >>
# by Aydin Mohseni

# Load the shiny GUI library
library(shiny)
library(ggplot2)
library(ggthemes)

# Allow for informative error messages
options(shiny.sanitize.errors = FALSE)

# Define server logic
shinyServer(function(input, output, session) {
  
  # Compute the dynamics
  computeDynamics <- reactive({
  
    # Set parameters
    R <- input$numberOfRounds # Number of rounds of the simulation
    N <- input$numberOfAgents # Number of agents
    
    ConfidenceInterval <- input$confidenceInterval # Default radius of confidence interval
    HighBias <- input$highBias # Bias in favor of higher opinions
    ConfidenceIntervalAbove <- ConfidenceInterval * exp(HighBias) # The range of opinions considered above that of an agent
    ConfidenceIntervalBelow <- ConfidenceInterval * exp(-HighBias) # The range of opinions considered bellow that of an an agent
    
    # Create a vector in which to store the history of opinions
    OpinionsMatrix <-
      data.frame(matrix(
        data = NA,
        nrow = R,
        ncol = N + 1
      ))
    # Set the initial opinions of all agents uniformly from 0 to 1
    InitialOpinionsVector <- seq(from = 0, to = 1, by = N ^ -1)
    OpinionsMatrix[1, ] <- InitialOpinionsVector
    
    # Run the Hegselmann & Krause opinion dynanmics
    for (round in 1:(R - 1)) { # For each round,
      for (agent in 1:(N + 1)) { # For each agent,
        # Get the current opinion of the agent
        Currentopinion <- OpinionsMatrix[round, agent]
        # Determine which peers are above the agent
        PeersAbove <- which(OpinionsMatrix[round, ] > Currentopinion & OpinionsMatrix[round, ] <= Currentopinion + ConfidenceIntervalAbove)
        # Determine which peers below the agent are close enough such that the agent will listen to them
        PeersBelow <- which(OpinionsMatrix[round, ] < Currentopinion & OpinionsMatrix[round, ] >= Currentopinion - ConfidenceIntervalBelow)
        # Compute the agent's new opinion as the average of her neighbors' opinions, including her own
        NewOpinion <-
          mean(c(Currentopinion,
                 as.vector(unlist(
                   OpinionsMatrix[round, PeersAbove]
                 )),
                 as.vector(unlist(
                   OpinionsMatrix[round, PeersBelow]
                 ))))
        # Update the agent's opinion
        OpinionsMatrix[round + 1, agent] <- NewOpinion
      }
    }
    
    # Create the plot
    DataForPlot <-
      data.frame(matrix(
        data = NA,
        ncol = 3,
        nrow = ((N + 1) * R)
      ))
    colnames(DataForPlot) <- c("Round", "Opinion", "Agent")
    DataForPlot$Round <- rep(1:R, each = (N + 1))
    DataForPlot$Agent <- as.character(rep(1:(N + 1), times = R))
    DataForPlot$Opinion <- as.vector(unlist(t(OpinionsMatrix)))
    DataForPlot$Agent <- factor(DataForPlot$Agent, levels = as.character(1:(N + 1)))
  
    # OUTPUT the results of our computations to be accessed by other reactive contexts
    return(list(DataForPlot))
    
  }) # END COMPUTATION OUTPUT
  
  # Render the plot
  output$OpinionDynamicsPlot <- renderPlot({
    
    # Set parameters
    R <- input$numberOfRounds # Number of rounds of the simulation
    N <- input$numberOfAgents # Number of agents
    
    # Import relevant variables
    DataForPlot <- computeDynamics()[[1]]
  
    # Plot the results
    HKplot <- ggplot(DataForPlot,
           aes(
             x = Round,
             y = Opinion,
             group = Agent,
             colour = Agent
           )) +
      geom_line(size = 0.5) +
      scale_color_manual(values = rainbow(N + 1)) +
      scale_x_continuous(breaks = seq(from = 1, to = R, by = 1)) +
      ggtitle("Opinion Dynamics") +
      labs(x = "Iterations", y = "Opinions") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_blank(), 
        #   element_text(
        #   size = 20,
        #   hjust = 0.5,
        #   margin = margin(b = 20, unit = "pt"),
        #   lineheight = 1.15
        # ),
        legend.position = "none",
        axis.title.x =  element_text(size = 16, margin = margin(t = 15, unit = "pt")),
        axis.title.y =  element_text(size = 16, margin = margin(r = 15, unit = "pt"))
      )
    print(HKplot)
    
  }) # END PLOT OUTPUT

}) ## EOD