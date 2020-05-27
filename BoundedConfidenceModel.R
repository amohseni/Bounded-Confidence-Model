# Hegselmann & Krause's Bounded Confidence Model

# Load packages.
library(ggplot2)

# Set global parameters
R <- 10 # Number of rouds of the simulation
N <- 100 # Number of agents

ConfidenceInterval <- 0.1 # Default radius of confidence interval
RightBias <- 0 # Bias in favor of right opinions
ConfidenceIntervalLeft <- ConfidenceInterval * exp(-RightBias) # The range of opinions considered to the left of an agent
ConfidenceIntervalRight <- ConfidenceInterval * exp(RightBias) # The range of opinions considered to the right an agent

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
    # Determine which peers are to the left of the agent
    PeersLeft <- which(OpinionsMatrix[round, ] > Currentopinion & OpinionsMatrix[round, ] <= Currentopinion + ConfidenceIntervalLeft)
    # Determine which peers are to the right of the agent are close enough such that the agent will listen to them
    PeersRight <- which(OpinionsMatrix[round, ] < Currentopinion & OpinionsMatrix[round, ] >= Currentopinion - ConfidenceIntervalRight)
    # Compute the agent's new opinion as the average of her neighbors' opinions, including her own
    NewOpinion <-
      mean(c(Currentopinion,
             as.vector(unlist(
               OpinionsMatrix[round, PeersLeft]
             )),
             as.vector(unlist(
               OpinionsMatrix[round, PeersRight]
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

# Plot the results
ggplot(DataForPlot,
       aes(
         x = Round,
         y = Opinion,
         group = Agent,
         colour = Agent
       )) +
  geom_line(size = 0.5) +
  scale_color_manual(values = rainbow(N + 1)) +
  ggtitle("") +
  labs(x = "Iterations", y = "Opinions") +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(b = 20, unit = "pt"),
      lineheight = 1.15
    ),
    legend.position = "none",
    axis.title.x =  element_text(margin = margin(t = 15, unit = "pt")),
    axis.title.y =  element_text(margin = margin(r = 15, unit = "pt"))
  )
# Plot the graph
