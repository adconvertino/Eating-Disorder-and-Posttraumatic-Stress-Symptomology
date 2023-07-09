library(mlVAR)

# simulation with 10 nodes ----
    # Generate true network with 10 nodes:
    SampleSizeSim <- mlVARsim(nPerson=70, nNode = 10, nTime = 84, lag=1)

    # Estimate model:
    FitSim <- mlVAR(SampleSizeSim$Data, vars = SampleSizeSim$vars,
                 idvar = SampleSizeSim$idvar, lags = 1, estimator = "lmer",
                 temporal = "correlated", contemporaneous = "correlated")

    # Extract network:
    samples <- mlVARsample(FitSim, nTime = 84, nSample = c(40,50,70), pMissing = .25, nReps = 10, nCores = 1)
    
    # View network sim:
    summary(samples)
    
    # Export to file:
    write.csv(samples, "sim_10nodes.csv", row.names=FALSE)