#'gmse_apply results summary
#'
#'Summarise gmse_apply() output
#'
#'@param data The full list as returned by the gmse function.
#'@param output An existing gmse_apply_summary output structure to append to.
#'  This should be a matrix with number of columns equal to the length of
#'  `include`. If NULL, a matrix with a single row with just the summary of
#'  given `data` gmse_apply() output is returned.
#'@param include A character vector listing which gmse_apply values should be
#'  reported/summarised. Currently allowable values are
#'  "res","obs","culls","scares","castrations","feeds","helps","tend_crops",
#'  "kill_crops","cull_cost","scare_cost", and "yield". Note that for all 
#'  actions and yield, the statistic returned is the sum across users; for 
#'  cull_cost and scare_cost the returned values are the mean costs across 
#'  users. Can be NULL; if so, if `output` is also
#'  NULL, all possible values are returned. If `output` is not NULL, only the
#'  values already present in `ouput` are returned, in the same column order.
#'@return A matrix of gmse_apply values, summarised.
#'@examples
#'\dontrun{
#'sim_old <- gmse_apply(get_res = "Full", scaring = FALSE, 
#'land_ownership = TRUE);
#'gmse_apply_summary(data = sim_old, include = c("res","obs","culls","yield"))
#'}
#'@export
gmse_apply_summary <- function(data, output = NULL, include = NULL) {
    
    if(length(data) == 4){
        stop("ERROR: Need the entire get_res = 'Full' gmse_apply output");
    }
    
    valid_includes <- c("res", "obs", "culls", "scares", "castrations", "feeds",
                        "helps", "tend_crops", "kill_crops", "yield",
                        "cull_cost", "scare_cost"
                        )
    
    if(is.null(output)) {
        if(is.null(include)) {
            include <- valid_includes
        }
        output <- matrix(NA, nrow = 1, ncol = length(include))
        colnames(output) <- include
    } else {
        if(!is.matrix(output)) {
            output <- try( {t(as.matrix(output)) }, silent = TRUE)
            if(class(output)=="try-error"){
                pt1 <- "Existing output needs to be a matrix,"
                pt2 <- " and failed to convert!"
                msg <- paste(pt1, pt2, sep = "")
                stop(msg)
            }
        }
        if(is.null(include)){
            include <- colnames(output)
        }
        if(ncol(output) != length(include)){
            pt1 <- "Existing output data specified does not match "
            pt2 <- "number of summary columns requested."
            msg <- paste(pt1, pt2, sep = "")
            stop(msg)
        } 
        if(!identical(colnames(output), include)){
            pt1 <- "Existing output data column order different to what "
            pt2 <- "was specified in 'include'."
            msg <- paste(pt1, pt2, sep = "")
            stop(msg)
        } 
    }
    
    if(sum(!(include %in% valid_includes))!=0) {
        invalid_includes <- include[which(!(include %in% valid_includes))]
        msg  <- "Some values requested in 'include' are not available: "
        stop(print(paste(msg, invalid_includes)))
    }
    
    res <- data$basic_output$resource_results
    obs <- data$basic_output$observation_results
    culls <- sum(data$basic_output$user_results[,"culling"], na.rm = TRUE)
    scares <- sum(data$basic_output$user_results[,"scaring"], na.rm = TRUE)
    castrations <- sum(data$basic_output$user_results[,"castration"], 
                       na.rm = TRUE)
    feeds <- sum(data$basic_output$user_results[,"feeding"], na.rm = TRUE)
    helps <- sum(data$basic_output$user_results[,"help_offspring"], 
                 na.rm = TRUE)
    tend_crops <- sum(data$basic_output$user_results[,"tend_crops"], 
                      na.rm = TRUE)
    kill_crops <- sum(data$basic_output$user_results[,"kill_crops"], 
                      na.rm = TRUE)
    cull_cost <- mean(data$COST[1,9,2:dim(data$COST)[3]])
    scare_cost <- mean(data$COST[1,8,2:dim(data$COST)[3]])
    yield <- sum(data$AGENTS[,16], na.rm = TRUE)
    
    outcols <- cbind(res, obs, culls, scares, castrations, 
                     feeds, helps, tend_crops,kill_crops, cull_cost, 
                     scare_cost, yield)[,include]
    output  <- rbind(output, outcols )
    
    ### Removes any lines that are all NA from output (typically only the 
    ### "first" line from a sim run, as the function needed to create an empty 
    ### output frame at that point.) Clumsy, but it works:
    output <- output[apply(apply(output, 2, function(x) !is.na(x)),1,sum)!=0,]
    
    return(output)
}
