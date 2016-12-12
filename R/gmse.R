

RESOURCE_t <- NULL;
RESOURCE_1 <- 100;
time_max   <- 2;
time       <- 0;

RESOURCE_t <- c(RESOURCE_t, RESOURCE_1);
while(time < time_max){
   RESOURCE_NEW  <- .Call("resource", RESOURCE_1);
   RESOURCE_1    <- RESOURCE_NEW;
   RESOURCE_t    <- c(RESOURCE_t, RESOURCE_1);
   time <- time + 1;
}