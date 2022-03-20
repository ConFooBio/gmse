x_vals            <- seq(from = 0, to = 2*pi, by = 0.05);
x_shifts          <- runif(n = length(x_vals), min = -0.4, max = 0.4);
fitness_landscape <- sin(x_vals) + x_shifts;

solution <- sample(x = 1:length(x_vals), size = 1);
kmax     <- 10000;
k        <- 0;
while(k < kmax){
  temp       <- 1 - (1 + k) / kmax;
  fitness    <- fitness_landscape[solution];
  neighbour  <- sample(x = -10:10, size = 1);
  x_sample   <- solution + neighbour;
  if(x_sample < 1){
    x_sample <- 1;
  }
  if(x_sample > length(fitness_landscape)){
    x_sample <- length(fitness_landscape);
  }
  fitness_n  <- fitness_landscape[x_sample];
  if(fitness_n < fitness){
    pr_jump <- exp(-(fitness - fitness_n) / temp);
    rand_pr <- runif(n = 1, min = 0, max = 1);
    if(pr_jump > rand_pr){
      solution <- x_sample;
    }
  }else{
    solution <- x_sample;
  }
  k <- k + 1;
}

plot(x = x_vals, y = fitness_landscape, type = "l", lwd = 2);
points(x = x_vals[solution], y = fitness_landscape[solution],
       col = "red", pch = 20, cex = 1.5);
