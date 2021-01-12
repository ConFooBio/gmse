#' gmse_apply results summary
#'
#' Summarise gmse_apply() output
#'
#'@param data The full list as returned by the gmse function.
#'@param output An existing gmse_apply_summary output structure to append to. This should be a matrix with number of columns equal to the length of `include`. If NULL, a matrix with a single row with just the summary of given `data` gmse_apply() output is returned.
#'@param data A character vector listing which gmse_apply values should be reported/summarised. Currently allowable values are "res","obs","culls","scares","castrations","feeds","helps","tend_crops","kill_crops","yield". Note that for all actions and yield, the statistic returned is the sum across users. Can be NULL; if so, if `output` is also NULL, all possible values are returned. If `output` is not NULL, only the values already present in `ouput` are returned, in the same column order.
#'@return A matrix of gmse_apply values, summarised.
#'@examples
#'\dontrun{
#'sim_summary <- sim_old <- gmse_apply(get_res = "Full", scaring = FALSE, land_ownership = T);
#'gmse_apply_summary(data = sim_old, include = c("res","obs","culls","yield"))
#'}
#'@export
gmse_apply_summary <- function(data, 
                               output = NULL, 
                               include = NULL) {
    
    valid_includes <- c("res","obs","culls","scares",
                        "castrations","feeds","helps","tend_crops","kill_crops","yield")
    
    if(is.null(output)) {
        if(is.null(include)) {
            include <- valid_includes
        }
        output <- matrix(NA, nrow = 1, ncol = length(include))
        colnames(output) <- include
    } else {
        if(!is.matrix(output)) {
            output <- try({t(as.matrix(output))},silent=T)
            if(class(output)=="try-error") stop("Existing output needs to be a matrix, and failed to convert!")
        }
        if(is.null(include)) include <- colnames(output)
        if(ncol(output) != length(include)) stop("Existing output data specified does not match number of summary columns requested.")
        if(!identical(colnames(output),include)) stop("Existing output data column order different to what was specified in 'include'.")
    }
    
    if(sum(!(include %in% valid_includes))!=0) {
        invalid_includes <- include[which(!(include %in% valid_includes))]
        stop(print(paste("Some values requested in 'include' are not available: ", invalid_includes)))
    }
    
    res <- data$basic_output$resource_results
    obs <- data$basic_output$observation_results
    culls <- sum(data$basic_output$user_results[,"culling"],na.rm=T)
    scares <- sum(data$basic_output$user_results[,"scaring"],na.rm=T)
    castrations <- sum(data$basic_output$user_results[,"castration"],na.rm=T)
    feeds <- sum(data$basic_output$user_results[,"feeding"],na.rm=T)
    helps <- sum(data$basic_output$user_results[,"help_offspring"],na.rm=T)
    tend_crops <- sum(data$basic_output$user_results[,"tend_crops"],na.rm=T)
    kill_crops <- sum(data$basic_output$user_results[,"kill_crops"],na.rm=T)
    yield <- sum(data$AGENTS[,16],na.rm=T)
    
    output <- rbind(output, cbind(res, obs, culls, scares, castrations, 
                                 feeds, helps, tend_crops,kill_crops, yield)[,include] )
    
    ### Removes any lines that are all NA from output (typically only the "first" line from a sim run, 
    ###  as the function needed to create an empty output frame at that point.)
    ### Clumsy, but it works:
    output <- output[apply(apply(output, 2, function(x) !is.na(x)),1,sum)!=0,]
    return(output)
}