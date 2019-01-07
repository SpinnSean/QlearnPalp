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
}

parameters {
  vector[4] mu_p;
  vector<lower=0>[4] sigma;
  vector[N] a_pr;
  vector[N] rre_pr;
  vector[N] rpu_pr;
  vector[N] gobias_pr;

}

transformed parameters {
  vector[N] re_re;
  vector[N] re_pu;
  vector<lower=0,upper=1>[N] alpha;
  vector[N] gobias;
  
  for (i in 1:N) {
    alpha[i]  = 0.98 *Phi_approx( mu_p[1] + sigma[1] * a_pr[i]) + 0.01 ;
  }
  re_re  = exp(mu_p[2] + sigma[2] * rre_pr);
  re_pu  = exp(mu_p[3] + sigma[3] * rpu_pr);
  gobias = mu_p[4] + sigma[4] * gobias_pr;

}

model {
  
  // group parameters (hyper)
  mu_p[1]  ~ normal(0, 1.0); 
  mu_p[2]  ~ normal(0, 1.0);
  mu_p[3]  ~ normal(0, 1.0);
  mu_p[4]  ~ normal(0, 10.0);
  sigma[1] ~ normal(0, 1.0);
  sigma[2] ~ normal(0, 3.0);
  sigma[3] ~ normal(0, 3.0);
  sigma[4] ~ normal(0, 5.0);
  
  // individual parameters
  a_pr ~ normal(0,1.0);
  rre_pr  ~ normal(0, 1.0);
  rpu_pr ~ normal(0,1.0);
  gobias_pr ~ normal(0,1.0);

  
  for (n in 1:N){
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
  
  real<lower=0, upper=1> mu_alpha;
  
  real mu_gobias;
  real<lower=0> mu_re_re;
  real<lower=0> mu_re_pu;
  
  real log_lik[N];
  int y_pred[N,B,T];
  
  for (n in 1:N) {
    for (b in 1:B) {
      for (t in 1:T) {
        y_pred[n,b,t] = -1;
      }
    }
  }
  
  mu_alpha = Phi_approx(mu_p[1]);
  mu_re_re = exp(mu_p[2]);
  mu_re_pu = exp(mu_p[3]);
  mu_gobias = mu_p[4];
  
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
}



