data {
  int<lower=1> I;               // # cues
  int<lower=1> N;               // # participants
  int<lower=1> T;               // # of trials per participant
  int<lower=1> B;               // # of blocks (RR,RP)
  int<lower=0, upper=I> cue[N, B, T];  // cue for n
  int<lower=0, upper=1> choice[N, B, T];   // choice for n 
  int<lower=-2, upper=2> outcome[N, B, T]; // outcome for n
  int pretreat[N, B, 9]; // pretreat cues
  int prerwds[N, B, 9]; // pretreat rewards 
  
  real mu_mu_alpha_1;
  real mu_mu_re_re_1;
  real mu_mu_re_pu_1;
  real mu_mu_gobias_1;  
  
  real std_mu_alpha_1;  
  real std_mu_re_re_1;  
  real std_mu_re_pu_1;  
  real std_mu_gobias_1;    
  
  real mu_std_alpha_1;
  real mu_std_re_re_1;
  real mu_std_re_pu_1;
  real mu_std_gobias_1;    
  
  real std_std_alpha_1;
  real std_std_re_re_1;
  real std_std_re_pu_1; 
  real std_std_gobias_1;
    
  real mu_mu_alpha_2;
  real mu_mu_re_re_2;
  real mu_mu_re_pu_2;
  real mu_mu_gobias_2;  
  
  real std_mu_alpha_2;  
  real std_mu_re_re_2;  
  real std_mu_re_pu_2;  
  real std_mu_gobias_2;    
  
  real mu_std_alpha_2;
  real mu_std_re_re_2;
  real mu_std_re_pu_2;
  real mu_std_gobias_2;    
  
  real std_std_alpha_2;
  real std_std_re_re_2;
  real std_std_re_pu_2;
  real std_std_gobias_2;    
  
}

parameters {
  vector[4] mu_pr_1;
  vector[4] mu_pr_2;
  vector<lower=0>[4] sigma_1;
  vector<lower=0>[4] sigma_2;
  //vector[N] a_pr;
  //vector[N] rre_pr;
  //vector[N] rpu_pr;
//  vector[N] gobias_pr;
  
  vector[4] ind_pr[N];

}

transformed parameters {
  vector[2] lp_parts[N];
  vector[N] re_re;
  vector[N] re_pu;
  vector<lower=0,upper=1>[N] alpha;
  vector[N] gobias;
  
  for (i in 1:N) {
    alpha[i]  = Phi_approx(ind_pr[i,1]);
    re_re[i]  = exp(ind_pr[i,2]);
    re_pu[i]  = exp(ind_pr[i,3]);
    gobias[i] = ind_pr[i,4];
  }
  
  // manually computed log probability of the mixture components
  for (i in 1:N) {							   
    lp_parts[i,1] = log(.5) + normal_lpdf(ind_pr[i] | mu_pr_1, sigma_1);
	lp_parts[i,2] = log(.5) + normal_lpdf(ind_pr[i] | mu_pr_2, sigma_2);								
  }
  
}



model {
  
    // Prior on the group-level mean parameters
  // probit scale [-Inf, Inf] 
  mu_pr_1[1] ~ normal(mu_mu_alpha_1, std_mu_alpha_1);
  mu_pr_1[2] ~ normal(mu_mu_re_re_1, std_mu_re_re_1);
  mu_pr_1[3] ~ normal(mu_mu_re_pu_1, std_mu_re_pu_1);
  mu_pr_1[4] ~ normal(mu_mu_gobias_1, std_mu_gobias_1);
        
  sigma_1[1] ~ normal(mu_std_alpha_1, std_std_alpha_1)T[0,];
  sigma_1[2] ~ normal(mu_std_re_re_1, std_std_re_re_1)T[0,];
  sigma_1[3] ~ normal(mu_std_re_pu_1, std_std_re_pu_1)T[0,];
  sigma_1[4] ~ normal(mu_std_gobias_1, std_std_gobias_1)T[0,];      
  
  mu_pr_2[1] ~ normal(mu_mu_alpha_2, std_mu_alpha_2);
  mu_pr_2[2] ~ normal(mu_mu_re_re_2, std_mu_re_re_2);
  mu_pr_2[3] ~ normal(mu_mu_re_pu_2, std_mu_re_pu_2);
  mu_pr_2[4] ~ normal(mu_mu_gobias_2, std_mu_gobias_2);
  
  sigma_2[1] ~ normal(mu_std_alpha_2, std_std_alpha_2)T[0,];
  sigma_2[2] ~ normal(mu_std_re_re_2, std_std_re_re_2)T[0,];
  sigma_2[3] ~ normal(mu_std_re_pu_2, std_std_re_pu_2)T[0,];
  sigma_2[4] ~ normal(mu_std_gobias_2, std_std_gobias_2)T[0,];      


  for (n in 1:N){
    // Update the model with manually computed log probability
   target += log_sum_exp(lp_parts[n]);
  /* target += log_mix(lambda,
   normal_lpdf(alpha[n], mu_pr_1[1], sigma_1[1]),
   normal_lpdf(gobias[n], mu_pr_1[1], sigma_1[1]),
   normal_lpdf(re_re[n], mu_pr_1[1], sigma_1[1]),
   normal_lpdf(re_pu[n], mu_pr_1[1], sigma_1[1]),)*/
   
    for (b in 1:B){
      vector[I] Qgo;
      vector[I] Qnogo;
      vector[I] Wgo;
      vector[I] Wnogo;
      vector[I] pGo;
      vector[I] initV;
      initV  = rep_vector(0, I);
      
      Qgo = initV;
      Qnogo = initV;
      Wgo = initV;
      Wnogo = initV;
      
      // Pretreatment
      for (cc in 1:9) {
        if(prerwds[n,b,cc]>=1) {
        Qgo[pretreat[n,b,cc]] = Qgo[pretreat[n,b,cc]] + (re_re[n]*prerwds[n,b,cc] - Qgo[pretreat[n,b,cc]]); 
        } else {
          Qgo[pretreat[n,b,cc]] = Qgo[pretreat[n,b,cc]] + (re_pu[n]*prerwds[n,b,cc] - Qgo[pretreat[n,b,cc]]); 
        }
      }
      
      
      for (j in 1:T) {
        Wgo[ cue[n,b,j] ] = Qgo[ cue[n,b,j] ] + gobias[n];
        Wnogo[ cue[n,b,j] ] = Qnogo[ cue[n,b,j] ];
        pGo[ cue[n,b,j] ]   = inv_logit(Wgo[ cue[n,b,j] ] - Wnogo[ cue[n,b,j] ] );
        //choice[n,b,j] ~ bernoulli( pGo[ cue[n,b,j] ] );
        target += bernoulli_lpmf(choice[n,b,j] | pGo[ cue[n,b,j] ] );
        
       // update action values
        if (choice[n,b,j]) { // update go value 
          if(outcome[n,b,j]>=1) {
            Qgo[ cue[n,b,j] ]  = Qgo[ cue[n,b,j] ] + alpha[n] * (re_re[n]*outcome[n,b,j] - Qgo[ cue[n,b,j] ]);
          } else {
            Qgo[ cue[n,b,j] ]  = Qgo[ cue[n,b,j] ] + alpha[n] * (re_pu[n]*outcome[n,b,j] - Qgo[ cue[n,b,j] ]);
          } 
        }
        else { // update no-go value
          if(outcome[n,b,j]>=1) { 
          Qnogo[ cue[n,b,j] ] = Qnogo[ cue[n,b,j] ] + alpha[n] * (re_re[n]*outcome[n,b,j] - Qnogo[ cue[n,b,j] ]);  
          } else {
          Qnogo[ cue[n,b,j] ] = Qnogo[ cue[n,b,j] ] + alpha[n] * (re_pu[n]*outcome[n,b,j] - Qnogo[ cue[n,b,j] ]);
          }
      }
    }
  }
}
}


generated quantities {
  
  vector<lower=0, upper=1>[2] mu_alpha;
  int<lower=0,upper=1> z[N];  // group indicator
  vector[2] mu_gobias;
  vector<lower=0>[2] mu_re_re;
  vector<lower=0>[2] mu_re_pu;
  
  real log_lik[N];
  int y_pred[N,B,T];
  
  for (n in 1:N) {
    for (b in 1:B) {
      for (t in 1:T) {
        y_pred[n,b,t] = -1;
      }
    }
  }
  
  mu_alpha[1] = Phi_approx(mu_pr_1[1]);
  mu_alpha[2] = Phi_approx(mu_pr_2[1]);
  mu_re_re[1] = exp(mu_pr_1[2]);
  mu_re_re[2] = exp(mu_pr_2[2]);
  mu_re_pu[1] = exp(mu_pr_1[3]);
  mu_re_pu[2] = exp(mu_pr_2[3]);
  mu_gobias[1] = mu_pr_1[4];
  mu_gobias[2] = mu_pr_2[4];
  
  for (n in 1:N){
    log_lik[n] = 0;
    for (b in 1:B){
      vector[I] Qgo;
      vector[I] Qnogo;
      vector[I] Wgo;
      vector[I] Wnogo;
      vector[I] pGo;
      vector[I] initV;
      initV  = rep_vector(0, I);
      
      Qgo = initV;
      Qnogo = initV;
      Wgo = initV;
      Wnogo = initV;
      
      // Pretreatment
      for (cc in 1:9) {
        Qgo[pretreat[n,b,cc]] = Qgo[pretreat[n,b,cc]] + (prerwds[n,b,cc] - Qgo[pretreat[n,b,cc]]); 
      }
      
      for (j in 1:T) {
        Wgo[ cue[n,b,j] ] = Qgo[ cue[n,b,j] ] + gobias[n];
        Wnogo[ cue[n,b,j] ] = Qnogo[ cue[n,b,j] ];
        pGo[ cue[n,b,j] ]   = inv_logit(Wgo[ cue[n,b,j] ] - Wnogo[ cue[n,b,j] ] );
        log_lik[n] = log_lik[n] + bernoulli_lpmf(choice[n,b,j] | pGo[ cue[n,b,j] ] );

        // generate posterior prediction for current trial
        y_pred[n,b,j] = bernoulli_rng(pGo[ cue[n,b,j] ]);

       // update action values
        if (choice[n,b,j]) { // update go value 
          if(outcome[n,b,j]>=1) {
            Qgo[ cue[n,b,j] ]  = Qgo[ cue[n,b,j] ] + alpha[n] * (re_re[n]*outcome[n,b,j] - Qgo[ cue[n,b,j] ]);
          } else {
            Qgo[ cue[n,b,j] ]  = Qgo[ cue[n,b,j] ] + alpha[n] * (re_pu[n]*outcome[n,b,j] - Qgo[ cue[n,b,j] ]);
          } 
        }
        else { // update no-go value
          if(outcome[n,b,j]>=1) { 
          Qnogo[ cue[n,b,j] ] = Qnogo[ cue[n,b,j] ] + alpha[n] * (re_re[n]*outcome[n,b,j] - Qnogo[ cue[n,b,j] ]);  
          } else {
          Qnogo[ cue[n,b,j] ] = Qnogo[ cue[n,b,j] ] + alpha[n] * (re_pu[n]*outcome[n,b,j] - Qnogo[ cue[n,b,j] ]);
          }
        }
       }
     } 
   }
   
    for (i in 1:N) {
    vector[2] prob;
    prob = softmax(lp_parts[i]);
    z[i] = bernoulli_rng(prob[2]); 
  }
   
}



