#include <RcppArmadillo.h>
// [[Rcpp::export]]
size_t armaIndexSize() {
  return sizeof(arma::uword);
}

#include <RcppArmadillo.h>
#include <vector>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
List sampleZ_cpp(const IntegerVector& n,
                 const NumericVector& p,
                 const double        delta,
                 const double        lambda,
                 const List&         neighbor) {
  int J = n.size();
  std::vector<int> i_out;
  std::vector<int> j_out;
  std::vector<double> x_out;

  for(int jj = 0; jj < J; ++jj) {
    int n_j = n[jj];
    if(n_j == 0) continue;

    // 1) Determine how many of these n_j are errors vs correct (binomial)
    int e_j = R::rbinom(n_j, delta);  // number of error-coupons for observed j
    int c_j = n_j - e_j;              // number of correct (self) allocations

    // 2) Record the self-allocations (Z_{jj} = c_j)
    if(c_j > 0) {
      i_out.push_back(jj + 1);
      j_out.push_back(jj + 1);
      x_out.push_back(static_cast<double>(c_j));
    }

    if(e_j == 0) continue;

    // 3) For the e_j errors, sample which true i they come from via multinomial
    List nbr_j = neighbor[jj];
    IntegerVector js = nbr_j["j"];   // 1-based neighbor indices
    NumericVector ds  = nbr_j["d"];  // corresponding distances
    int m_j = js.size();

    // If there are no neighbors, assign all errors back to self
    if(m_j == 0) {
      i_out.push_back(jj + 1);
      j_out.push_back(jj + 1);
      x_out.push_back(static_cast<double>(e_j));
      continue;
    }

    // Build weights for neighbors only (no self term)
    std::vector<int>    psi_idx(m_j);
    std::vector<double> psi_vals(m_j);
    for(int idx = 0; idx < m_j; ++idx) {
      psi_idx[idx]  = js[idx];              // 1-based i
      int i0        = js[idx] - 1;          // 0-based for p
      psi_vals[idx] = p[i0] * std::exp(-ds[idx] / lambda);
    }

    // Normalize the weights to sum to 1
    double sum_w = 0.0;
    for(double v : psi_vals) sum_w += v;
    // If sum_w is zero (all exp(-d/λ) extremely small), send errors to self
    if(sum_w <= 0) {
      i_out.push_back(jj + 1);
      j_out.push_back(jj + 1);
      x_out.push_back(static_cast<double>(e_j));
      continue;
    }
    for(double &v : psi_vals) v /= sum_w;

    // Sample e_j draws from this multinomial
    IntegerVector r_idx   = wrap(psi_idx);
    NumericVector r_probs = wrap(psi_vals);
    IntegerVector outcome = Rcpp::sample(r_idx, e_j, true, r_probs);

    // Tally up how many times each neighbor i appears
    std::unordered_map<int,int> counts;
    counts.reserve(e_j);
    for(int z : outcome) {
      counts[z]++;
    }

    // Push each (i, j) with its count
    for(const auto &pr : counts) {
      int i1 = pr.first;        // 1-based i index
      int cnt = pr.second;
      i_out.push_back(i1);
      j_out.push_back(jj + 1);
      x_out.push_back(static_cast<double>(cnt));
    }
  }

  return List::create(Named("i") = i_out,
                      Named("j") = j_out,
                      Named("x") = x_out);
}


// [[Rcpp::export]]
double compute_loglik_cpp(const S4&   Zsp,
                          const double lambda,
                          const List&  neighbor) {
  arma::sp_mat Z = as<arma::sp_mat>(Zsp);
  int K = neighbor.size();

  std::vector<double> denom(K, 0.0);
  for(int i = 0; i < K; ++i) {
    List nbr_i = neighbor[i];
    NumericVector ds  = nbr_i["d"];
    double sum_exp = 0.0;
    for(int idx = 0; idx < ds.size(); ++idx) {
      sum_exp += std::exp(-ds[idx] / lambda);
    }
    denom[i] = sum_exp;
  }

  double loglik = 0.0;
  for(arma::sp_mat::const_iterator it = Z.begin(); it != Z.end(); ++it) {
    int i = it.row();
    int j = it.col();
    double z_val = *it;
    if(i == j) continue;

    List nbr_j = neighbor[j];
    IntegerVector js = nbr_j["j"];
    NumericVector ds  = nbr_j["d"];
    double d_ij = 0.0;
    for(int idx = 0; idx < js.size(); ++idx) {
      if(js[idx] - 1 == i) {
        d_ij = ds[idx];
        break;
      }
    }
    loglik += z_val * (-d_ij / lambda - std::log(denom[i]));
  }

  return loglik;
}
