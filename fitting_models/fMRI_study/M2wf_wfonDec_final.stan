data {
  // this section reads in the data that is piped through the r-command, and is only executed once
    int<lower=1> N; //where N is the number of subjects
    int<lower=1> T; //max number of trials per subject
    int<lower=1> TBlock; //number of trials before new symbols are used
    int<lower=1,upper=T> Tsubj[N]; //vector containing the number of trials for each subject
   
   //data for conflict paradigm(M)
    int<lower=1,upper=2> choice[N,T]; //Matrix of N subjects x T trials of 1 and 2 indicating whether participants chose option 1 or 2 on every trial
    real outcomeM[N,T];  // Matrix of N subjects x T trials with no lower and upper bounds, indicating the Money reward or loss incurred on ever trial.   
    real outcomeS[N,T];  // Matrix of N subjects x T trials with no lower and upper bounds, indicating the Shock incurred on ever trial.   
    //real filter_money[N,T]; // Matrix of Nsub x T trials with 1 = reward outcome and 0 = no reward outcome
    //real filter_shock[N,T]; // Matrix of Nsub x T trials with 1 = shock outcome and 0 = no shock outcome
  
  
  }
  
transformed data {
  // this section hard codes the initial value of the EV for the two options to zero
    vector[2] initV;  // initial values for EV
    initV = rep_vector(0.0, 2); // rep_vector(0.0,2) creates a vector c(0,0), i.e. zeros the EV for both options
  }
  
parameters {
  // Declare all parameters as vectors for vectorizing
    // Hyper(group)-parameters  
    vector[4] mu_p;  
    vector<lower=0>[4] sigma;
      
    // Subject-level raw parameters (for Matt trick)
    vector[N] A_pr;    // learning rate for money
    vector[N] tau_pr;  // inverse temperature
    vector[N] B_pr;    // learning rate for shock
    vector[N] wf_pr;    // weightening factor for shock    
  
  }
  
transformed parameters {
    // subject-level parameters
    vector<lower=0,upper=1>[N] A;
    vector<lower=0,upper=5>[N] tau;
    vector<lower=0,upper=1>[N] B;
    vector<lower=0,upper=1>[N] wf;
  
    
    for (i in 1:N) {
      A[i]   = Phi_approx( mu_p[1]  + sigma[1]  * A_pr[i] );
      tau[i] = Phi_approx( mu_p[2]  + sigma[2]  * tau_pr[i] ) * 5;
      B[i]   = Phi_approx( mu_p[3]  + sigma[3]  * B_pr[i] );
      wf[i]  = Phi_approx( mu_p[4]  + sigma[4]  * wf_pr[i] ); // mu_p centered in 0, w_pr centered in 0, -> w centered in Phi_approx(0) = 0.5 (equal weight for money and shock)
    }
  }
  
model {
    // Hyperparameters
    mu_p  ~ normal(0, 1); // priors on the group learning rate(mu_p[1] and mu_p[3]) and temperature (mu_p[2])
    sigma ~ cauchy(0, 5); // prior on the standard deviation of the individual differences in A, B and tau (distribution of the sd of a normal distr)
    
    // individual parameters
    A_pr   ~ normal(0, 1); //priors on individual deviations, but this will be scaled with sigma[1]
    tau_pr ~ normal(0, 1); //priors on individual deviation, but this will be scaled with sigma[2]
    B_pr   ~ normal(0, 1); //priors on individual deviations, but this will be scaled with sigma[3]
    wf_pr  ~ normal(0, 1); //priors on individual deviations, but this will be scaled with sigma[4]
    
    // subject loop and trial loop
    for (i in 1:N) { //subject loop with i indexing subject

  // Variables for the conflict only model 
      vector[2] evM; // declares expected value for the two alternatives, hence [2], as a local variable
      vector[2] evS; // declares expected value for the two alternatives, hence [2], as a local variable
      real PEM;      // declares prediction error as a local variable. There is only one PE per trial hence no [2]
      real PES;      // declares prediction error as a local variable. There is only one PE per trial hence no [2]
      real avEVM;
      real avEVS;
  
  // the conflict only model 
      evM = initV; // setting both to zero before going through the trials
      evS = initV; // setting both to zero before going through the trials
  
      for (t in 1:(Tsubj[i])) { //sets a loop going through Tsubj=number of trials of that participant 
        
        if ((t % TBlock)==1) {   //operator% in stan is modulus, and 1 mod 15 = 1 , 15 mod 15 = 0, 16 mod 15 = 1 etc 
          avEVM = (evM[1] + evM[2]) / 2;
          evM[1] = avEVM;
          evM[2] = avEVM;
          avEVS = (evS[1] + evS[2]) / 2;
          evS[1] = avEVS;
          evS[2] = avEVS;
        }
        
        
        
        // choice
        choice[i,t] ~ categorical_logit(tau[i] * (evM*(wf[i]) + evS*(1 - wf[i])));
        // prediction error 
        PEM = (outcomeM[i,t] - evM[choice[i,t]]); 
        PES = (outcomeS[i,t] - evS[choice[i,t]]); 
        // value updating (learning) 
        evM[choice[i,t]] = evM[choice[i,t]] + A[i] * PEM; 
        evS[choice[i,t]] = evS[choice[i,t]] + B[i] * PES; 

        
      }
    }
  }
  
generated quantities {
  
  // for each sampling this will generate the actual values to use as output
  // this includes the population A and tau, that are properly scaled, as well as a cumulative log_lik over all trails for each participant. 
  
    // For group level parameters
    real<lower=0,upper=1> mu_A; 
    real<lower=0,upper=5> mu_tau;
    real<lower=0,upper=1> mu_B; 
    real<lower=0,upper=1> mu_wf;
    
    // For model regressors
    real tbtPEM[N,T];
    real tbtPES[N,T];
    real tbtevM[N,T];
    real tbtevS[N,T];
    real tbtbothevM[2, N, T];
    real tbtbothevS[2, N, T];
    real tbtevall[2, N, T];
    
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
    mu_B   = Phi_approx(mu_p[3]);
    mu_wf  = Phi_approx(mu_p[4]);
  
  
    { // local section, this saves time and space
      for (i in 1:N) {
   
   // declaring variables  for the money and shock conflict
   
        vector[2] evM; // expected value for money 
        vector[2] evS; // expected value for shock
        real PEM;      // prediction error
        real PES;      // prediction error
        real avEVM;    // averageof the expected value for money
        real avEVS;   // averageof the expected value for shock
  
  // Define the conflict model 
  
        // Initialize values
        evM = initV;
        evS = initV;
        
        log_lik_sub[i] = 0;
        
        for (t in 1:(Tsubj[i])) {// loop for every trial of each participant
              log_lik[i,t] = 0;

          if ((t % TBlock)==1) {   //operator% in stan is modulus, and 1 mod 15 = 1 , 15 mod 15 = 0, 16 mod 15 = 1 etc 
          avEVM = (evM[1] + evM[2]) / 2;
          evM[1] = avEVM;
          evM[2] = avEVM;
          avEVS = (evS[1] + evS[2]) / 2;
          evS[1] = avEVS;
          evS[2] = avEVS;
          
        }
         
          // compute action probabilities
          log_lik_sub[i] = log_lik_sub[i] + categorical_logit_lpmf(choice[i,t] | tau[i] * (evM*(wf[i]) + evS*(1 - wf[i])));
          log_lik[i,t] = log_lik[i,t] + categorical_logit_lpmf(choice[i,t] | tau[i] * (evM*(wf[i]) + evS*(1 - wf[i])));



          // prediction error 
          PEM = (outcomeM[i,t] - evM[choice[i,t]]); 
          PES = (outcomeS[i,t] - evS[choice[i,t]]); 
          
          // generate posterior prediction for current trial
          y_pred[i,t] = categorical_rng(softmax(tau[i] * (evM*(wf[i]) + evS*(1 - wf[i]))));


           // Store values for model regressors
          tbtPEM[i,t]= PEM;
          tbtPES[i,t]= PES;
          
          tbtevM[i,t]= evM[choice[i,t]];
          tbtevS[i,t]= evS[choice[i,t]];
          //bothevM[1:2, i, t] = evM; // if it does not work, replace this and the next line with the following 4
          //bothevS[1:2, i, t] = evS;
          tbtbothevM[1, i, t] = evM[1];
          tbtbothevS[1, i, t] = evS[1];
          tbtbothevM[2, i, t] = evM[2];
          tbtbothevS[2, i, t] = evS[2];
          tbtevall[1, i, t] = wf[i] * evM[1] + (1 - wf[i]) * evS[1];
          tbtevall[2, i, t] = wf[i] * evM[2] + (1 - wf[i]) * evS[2];
          
          // value updating (learning) 
        evM[choice[i,t]] = evM[choice[i,t]] + A[i] * PEM; 
        evS[choice[i,t]] = evS[choice[i,t]] + B[i] * PES; 
         
        }
  
      }   
    }
  }
  
