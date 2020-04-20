data {
  // this section reads in the data that is piped through the r-command, and is only executed once
  int<lower=1> N; // where N is the number of subjects
  int<lower=1> T; // max number of trials per subject
  int<lower=1> TBlock; // number of trials before new symbols are used
  int<lower=1,upper=T> Tsubj[N]; //vector containing the number of trials for each subject

  // data for conflict paradigm(M)
  int<lower=1,upper=2> choice[N,T];
  real outcomeM[N, T];
  real outcomeS[N, T];
  real filter_money[N,T]; // Matrix of Nsub x T trials with 1 = reward outcome and 0 = no reward outcome
  real filter_shock[N,T]; // Matrix of Nsub x T trials with 1 = shock outcome and 0 = no shock outcome
  //  real filter_isUpdating[N,T]; // Matrix of Nsub x T trials with 1 = updating and 0 = no updating (model parameters not updated)
}

transformed data {
  // this section hard codes the initial value of the EV for the two options to zero
  vector[2] initV;  // initial values for EV
  initV = rep_vector(0.0, 2); // rep_vector(0.0,2) creates a vector c(0,0), i.e. zeros the EV for both options
}

parameters {
  // Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  vector[3] mu_p;
  vector<lower=0>[3] sigma;

  // Subject-level raw parameters (for Matt trick)
  vector[N] A_pr;  // learning rate
  vector[N] tau_pr;  // inverse temperature
  vector[N] wf_pr;  // weightening factor for shock
}

transformed parameters {
  // subject-level parameters
  vector<lower=0, upper=1>[N] A;
  vector<lower=0, upper=5>[N] tau;
  vector<lower=0, upper=1>[N] wf;

  for (i in 1:N) {
    A[i] = Phi_approx( mu_p[1] + sigma[1] * A_pr[i] );
    tau[i] = Phi_approx( mu_p[2] + sigma[2] * tau_pr[i]) * 5;
    wf[i] = Phi_approx( mu_p[3] + sigma[3] * wf_pr[i]);
  }
}

model {
  // Hyperparameters
  mu_p  ~ normal(0, 1);  // priors on the group learning rate(mu_p[1], mu_p[3] and mu_p[4]) and temperature (mu_p[2])
  sigma ~ cauchy(0, 5);  // prior on the standard deviation of the individual differences in A, B, C and tau

  // individual parameters
  A_pr   ~ normal(0, 1);  //priors on individual deviations, but this will be scaled with sigma[1]
  tau_pr ~ normal(0, 1);  //priors on individual deviation, but this will be scaled with sigma[2]
  wf_pr  ~ normal(0, 1);  //priors on individual deviations, but this will be scaled with sigma[3]

  // subject loop and trial loop
  for (i in 1:N) { //subject loop with i indexing subject
  // Variables for the conflict only model
    vector[2] ev; // declares expected value for the two alternatives, hence [2], as a local variable
    real PE;
    real avEV;
    // the conflict only model
    ev = initV; // setting both to zero before going through the trials

    for (t in 1:(Tsubj[i])) { //sets a loop going through Tsubj=number of trials of that participant
      if ((t % TBlock) == 1) {   //operator % in stan is modulus, and 1 mod 15 = 1 , 15 mod 15 = 0, 16 mod 15 = 1 etc It serves to detect the beginning of each block
        avEV= (ev[1] + ev[2]) / 2;
        ev[1]= avEV;
        ev[2]= avEV;
      }
      // choice
      choice[i,t] ~ categorical_logit( tau[i] * (ev));
      // prediction error
      PE = (filter_money[i,t]*outcomeM[i,t]*(wf[i]) + filter_shock[i,t]*outcomeS[i,t]*(1-wf[i]) + (1-filter_money[i,t])*outcomeS[i,t]*wf[i] + (1-filter_shock[i,t])*outcomeM[i,t]*(1-wf[i])) - ev[choice[i,t]];
      // value updating (learning)
      ev[choice[i,t]]  = ev[choice[i,t]] + A[i] * PE;
    }
  }
}

generated quantities {
  // for each sampling this will generate the actual values to use as output
  // this includes the population A and tau, that are properly scaled, as well as a cumulative log_lik over all trails for each participant.
  // For group level parameters
  real<lower=0,upper=1> mu_A;
  real<lower=0,upper=5> mu_tau;
  real<lower=0,upper=1> mu_wf;

  real tbtPE[N,T];
  real tbtevc[N,T];
  real tbtev1[N,T];
  real tbtev2[N,T];

  // For log likelihood calculation
  real log_lik[N,T]; // for the conflict choices
  real log_lik_sub[N]; // for the conflict choices

  // For posterior predictive check
  real y_pred[N,T];

  // Set all posterior predictions to 0 (avoids NULL values)
  for (i in 1:N) {
    for (t in 1:T) {
      y_pred[i,t] = -1;
    }
  }

  mu_A   = Phi_approx(mu_p[1]);
  mu_tau = Phi_approx(mu_p[2]) * 5;
  mu_wf   = Phi_approx(mu_p[3]);

  {
    // local section, this saves time and space
    for (i in 1:N) {
      // declaring variables for the money only model first, then for the money and shock conflict
      vector[2] ev;  // expected value for shock and money combined
      real PE;       // prediction error for shock and money combined
      real avEV;
      // Define the conflict model
      // Initialize values
      ev = initV;
      log_lik_sub[i] = 0;

      for (t in 1:(Tsubj[i])) {   // loop for every trial of each participant
        log_lik[i,t] = 0;

        if ((t % TBlock)==1) {   //operator% in stan is modulus, and 1 mod 15 = 1 , 15 mod 15 = 0, 16 mod 15 = 1 etc
          avEV=(ev[1]+ev[2])/2;
          ev[1]=avEV;ev[2]=avEV;
        }
        // compute action probabilities
        log_lik[i,t] = log_lik[i,t] + categorical_logit_lpmf(choice[i,t] | tau[i] * (ev));
        log_lik_sub[i] = log_lik_sub[i] + categorical_logit_lpmf(choice[i,t] | tau[i] * (ev));

        // prediction error
        PE = (filter_money[i,t]*outcomeM[i,t]*(wf[i]) + filter_shock[i,t]*outcomeS[i,t]*(1-wf[i]) + (1-filter_money[i,t])*outcomeS[i,t]*wf[i] + (1-filter_shock[i,t])*outcomeM[i,t]*(1-wf[i])) - ev[choice[i,t]];
        tbtPE[i, t] = PE;

        // generate posterior prediction for current trial
        y_pred[i,t] = categorical_rng(softmax(tau[i] * (ev)));

        // value updating (learning)
        ev[choice[i,t]] = ev[choice[i,t]] + A[i] * PE;
        tbtev1[i,t]=ev[1];
        tbtev2[i,t]=ev[2];
        tbtevc[i,t]= ev[choice[i,t]];
      }
    }
  }
}
