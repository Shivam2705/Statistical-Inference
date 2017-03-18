library(ggplot2)
no_sim<-1000 
sample_size<-40 
lambda <- 0.2
set.seed(3)
sim_matrix <- matrix(rexp(n = no_sim * sample_size, rate = lambda), no_sim, sample_size)
sim_mean <- rowMeans(sim_matrix)
sim_data <- data.frame(cbind(sim_matrix, sim_mean))
ggplot(data = sim_data, aes(sim_data$sim_mean,col = "red")) + geom_histogram(breaks = seq(2, 9, by = 0.2), col = "black", aes(fill = ..count..)) +labs(title = "Distribution of Mean", x = "Means on Simulation ", y = "Frequency for Simulation", col="red") + geom_vline(aes(xintercept=mean(sim_data$simulationMean)), col="red",linetype="dashed", size=1)
actual_mean <- mean(sim_mean)
theoretical_mean <- (1 / lambda)
actual_variance <- var(sim_mean)
theoretical_variance <- ((1 / lambda) ^ 2) / sample_size
qplot(sim_mean, geom = 'blank') + geom_line(aes(y=..density.., colour='blue'), stat='density', size=1) + stat_function(fun=dnorm, args=list(mean=(1/lambda), sd=((1/lambda)/sqrt(sample_size))),aes(colour='Empirical'), size=1) + geom_histogram(aes(y=..density.., fill=..density..), alpha=0.4,breaks = seq(2, 9, by = 0.2), col='red') + scale_fill_gradient("Density", low = "black", high = "green") + scale_color_manual(name='Density', values=c('blue', 'brown')) + theme(legend.position = c(0.85, 0.60)) +labs(title = "Mean Density Distribution", x = "Mean on Simulation", y = "Obtained Density")
qqnorm(sim_mean) 
qqline(sim_mean)

