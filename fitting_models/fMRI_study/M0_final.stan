data {
  int<lower=1> N; //where N is the number of subjects
  int<lower=1> T; //max number of trials per subject
  int<lower=1> TBlock; //number of trials before new symbols are used
  int<lower=1,upper=T> Tsubj[N]; //vector containing the number of trials for each subject
   
  int<lower=1,upper=2> choice[N,T]; //Matrix of N subjects x T trials of 1 and 2 indicating whether participants chose option 1 or 2 on every trial
  real outcomeM[N,T];  // Matrix of N subjects x T trials with no lower and upper bounds, indicating the Money reward or loss incurred on ever trial
  real outcomeS[N,T];  // Matrix of N subjects x T trials with no lower and upper bounds, indicating the Shock incurred on ever trial.   
  }
  
transformed data {
  // this section hard codes the initial value of the EV for the two options to zero
  vector[2] initV;  // initial values for EV
  initV = rep_vector(0.0, 2); // rep_vector(0.0,2) creates a vector c(0,0), i.e. zeros the EV for both option
  }
  
parameters {
  // Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters  
  vector[1] mu_p;  
  vector<lower=0>[1] sigma;
      
  // Subject-level raw parameters (for Matt trick)
  vector[N] tau_pr;  // inverse temperature
  }
  
transformed parameters {
  // subject-level parameters
  vector<lower=0,upper=5>[N] tau;
    
  for (i in 1:N) {
   tau[i] = Phi_approx( mu_p[1] + sigma[1] * tau_pr[i] ) * 5;
   }
  }

model {
  // Hyperparameters
  mu_p  ~ normal(0, 1); // priors on the group temperature (mu_p[1])
  sigma ~ cauchy(0, 5); // prior on the standard deviation of the individual differences  tau
    
  // individual parameters
  tau_pr ~ normal(0,1); //priors on individual deviation, but this will be scaled with sigma[1]
   
  // subject loop and trial loop
  for (i in 1:N) { //subject loop with i indexing subject

  // Variables for the conflict only model 
  vector[2] ev; // declares expected value for the two alternatives, hence [2], as a local variable
  real PE;  
  real avEV;
  
  // the conflict only model 
  ev = initV; 
    
  for (t in 1:(Tsubj[i])) { //sets a loop going through Tsubj=number of trials of that participant 
    if ((t % TBlock)==1) {   //operator % in stan is modulus, and 1 mod 15 = 1 , 15 mod 15 = 0, 16 mod 15 = 1 etc It serves to detect the beginning of each block
    avEV= (ev[1] + ev[2]) / 2;
    ev[1]= avEV; 
    ev[2]= avEV;
    }
    choice[i,t] ~ categorical_logit( tau[i] * (ev) ); // this establishes that the actual choice (as provided in data) should follow the caterogiral logit regression with temperature * expected value 
  
    // prediction error 
    PE = (outcomeM[i,t]+outcomeS[i,t]) - ev[choice[i,t]]; //i.e. actual reward or loss - expected value of the choice.
    
    // value updating (learning) 
    ev[choice[i,t]] = ev[choice[i,t]]; //+ A[i] * PE; // this now NOT updates the expected value based on learning rate
    }
  }
}
  
generated quantities {
  
  // for each sampling this will generate the actual values to use as output
  // this includes the population A and tau, that are properly scaled, as well as a cumulative log_lik over all trails for each participant. 
  
  // For group level parameters
  real<lower=0,upper=5> mu_tau;
    
  // For log likelihood calculation
  real log_lik[N]; // for the conflict choices
    
  // For posterior predictive check
  real y_pred[N,T]; 
    
  // Set all posterior predictions to 0 (avoids NULL values)
  for (i in 1:N) {
    for (t in 1:T) {
      y_pred[i,t] = -1;
      }
    }
  
  mu_tau = Phi_approx(mu_p[1]) * 5;
   
  { // local section, this saves time and space
  for (i in 1:N) {
   
  // declaring variables  for the money and shock conflict
   
  vector[2] ev;  // expected value for shock and money combined
  real PE;       // prediction error for shock and money combined
  real avEV;
  
  // Define the conflict model 
  
  // Initialize values
  ev = initV;
        
  log_lik[i] = 0;
        
  for (t in 1:(Tsubj[i])) {// loop for every trial of each participant
    if ((t % TBlock)==1) {   //operator% in stan is modulus, and 1 mod 15 = 1 , 15 mod 15 = 0, 16 mod 15 = 1 etc 
      avEV=(ev[1]+ev[2])/2;
      ev[1]=avEV;ev[2]=avEV;
    }
         
  // compute action probabilities
  log_lik[i] = log_lik[i] + categorical_logit_lpmf(choice[i,t] | tau[i] * (ev));
          
  // prediction error 
  PE = (outcomeM[i,t]+outcomeS[i,t]) - ev[choice[i,t]];
  
  // generate posterior prediction for current trial
  y_pred[i,t] = categorical_rng(softmax(tau[i] * ev));
  
  // value updating (learning) 
  ev[choice[i,t]] = ev[choice[i,t]]; 
      }
    }   
  }
}
  
