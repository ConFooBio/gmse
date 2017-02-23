SEXP test(SEXP LANDSCAPE){
    
    
    int land_x, land_y;      /* x and y maximum location given LANDSCAPE */
        int xloc, yloc;
    int layers;
    int *dim_LANDSCAPE;      /* Dimensions of the LANDSCAPE array incoming */
        int protected_n;
    double *land_ptr;        /* Pointer to LANDSCAPE (interface R and C) */
        double **land;           /* Array to store the landscape in C*/
        
        /* First take care of all the reading in of code from R to C */
        /* ====================================================================== */
        
        protected_n = 0;
        
        PROTECT( LANDSCAPE = AS_NUMERIC(LANDSCAPE) );
        protected_n++;
        land_ptr = REAL(LANDSCAPE);
        
        dim_LANDSCAPE  = INTEGER( GET_DIM(LANDSCAPE) );
        
        /* The C code for the model itself falls under here */
            /* ====================================================================== */
            
            /* Code below reads in the LANDSCAPE for easy of use */
            land_y = dim_LANDSCAPE[1];
            land_x = dim_LANDSCAPE[0];
            land   = malloc(land_x * sizeof(double *));
            for(xloc = 0; xloc < land_x; xloc++){
                land[xloc] = malloc(land_y * sizeof(double));   
            } /* LANDSCAPE is now stored as land */
                
                /* Do the biology here now */
                /* ====================================================================== */
                
                
                /* This code switches from C back to R */
                /* ====================================================================== */        
                
                
                
                UNPROTECT(protected_n);
            
            /* Free all of the allocated memory used in arrays */
                for(xloc = 0; xloc < land_x; xloc++){
                    free(land[xloc]);        
                }
            free(land);
            
            
            return(RESOURCE_NEW); 
}
/* ===========================================================================*/
    
    
    
    
    
    
    
    
    
    
    








res;
land;
paras;
agents;





testing(res, land, paras, agents);






testing   <- function(resource   = NULL, 
                      landscape  = NULL, 
                      paras      = NULL, 
                      agent      = NULL,
                      res_type   = 1,
                      samp_age   = 1,
                      agent_type = 0,
                      type_cat   = 1,
                      model      = "IBM"
){
    check_model <- 0;
    # Use DATA as an array for mark-recapture information
    # Time-stamp the mark-recapture data so can simulate within/over years
    if(model == "IBM"){
        # Relevant warnings below if the inputs are not of the right type
        if(!is.array(resource)){
            stop("Warning: Resources need to be in an array");   
        }
        if(!is.array(landscape)){
            stop("Warning: Landscape need to be in an array");
        } # TODO: make sure paras is right length below
        if(!is.vector(paras) | !is.numeric(paras)){
            stop("Warning: Parameters must be in a numeric vector");
        }
        if(!is.array(agent)){
            stop("Warning: Agents need to be in an array");
        }
        # If all checks out, first put the type into paras for easier input
        paras[8]  <- agent_type;
        paras[10] <- res_type;
        paras[17] <- samp_age;
        paras[18] <- type_cat;
        # Then run the population model
        ANECDOTAL_OUT  <- run_anecdotal_c( RESOURCE_c   = resource,
                                           LANDSCAPE_c  = landscape,
                                           PARAMETERS_c = paras,
                                           AGENT_c      = agent
        );
        check_model <- 1;
    }
    if(check_model == 0){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return(ANECDOTAL_OUT);
}

run_anecdotal_c <- function(RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c){
    .Call("anecdotal2", RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c);
}




