// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List sampleZ_cpp(const IntegerVector& n,
                 const NumericVector& p,
                 double delta,
                 const List& neighbor_list,
                 const NumericMatrix& dist_mat,
                 double lambda) {
  int K = n.size();
  std::vector<int> i_out;
  std::vector<int> j_out;
  std::vector<int> x_out;
  
  for(int k = 0; k < K; ++k) {
    int sz = n[k];
    if(sz <= 0) continue;
    IntegerVector is = neighbor_list[k];       // 1-based indices
    int m = is.size();
    
    // compute weights
    NumericVector w(m + 1);
    w[0] = (1 - delta) * p[k];
    double sumw = w[0];
    for(int t = 0; t < m; ++t) {
      int ii = is[t] - 1;                       // convert to 0-based
      double dij = dist_mat(ii, k);
      double g = exp(-dij / lambda);
      w[t+1] = delta * p[ii] * g;
      sumw += w[t+1];
    }
    for(int t = 0; t <= m; ++t) w[t] /= sumw;
    
    // draw multinomial
    IntegerVector counts(m + 1);
    R::rmultinom(sz, w.begin(), m+1, counts.begin());
    
    // store
    if(counts[0] > 0) {
      i_out.push_back(k+1);
      j_out.push_back(k+1);
      x_out.push_back(counts[0]);
    }
    for(int t = 0; t < m; ++t) {
      if(counts[t+1] > 0) {
        i_out.push_back(is[t]);
        j_out.push_back(k+1);
        x_out.push_back(counts[t+1]);
      }
    }
  }
  return List::create(_["i"] = i_out, _["j"] = j_out, _["x"] = x_out);
}

// [[Rcpp::export]]
double sampleLambda_cpp(const NumericVector& d_off,
                        const NumericVector& z_off,
                        double lambda,
                        double lambda_rate) {
  double lambda_prop = lambda * exp(0.1 * R::rnorm(0,1));
  int L = d_off.size();
  double logLik = 0.0;
  
  for(int k = 0; k < L; ++k) {
    logLik += z_off[k] * ((-d_off[k]/lambda_prop) - (-d_off[k]/lambda));
  }
  double logPrior = -lambda_rate * (lambda_prop - lambda);
  double logJac   = log(lambda_prop / lambda);
  double log_r    = logLik + logPrior + logJac;
  if(log(R::runif(0,1)) < log_r)
    return lambda_prop;
  return lambda;
}
