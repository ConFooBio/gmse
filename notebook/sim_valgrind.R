#  R -d "valgrind --tool=memcheck --leak-check=yes --track-origins=yes" --vanilla < notebook/sim_valgrind.R

library(GMSE);

sim1 <- gmse(time_max = 10, land_dim_1 = 45, perceive_scare = -0.3,
             land_ownership = TRUE, stakeholders = 7, scaring = TRUE,
             plotting = FALSE);

sim2 <- gmse_apply(time_max = 10, land_dim_1 = 45, perceive_scare = -0.3,
                   land_ownership = TRUE, stakeholders = 7, scaring = TRUE);
