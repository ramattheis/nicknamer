// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <map>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame make_bayes_choice_dictionary_cpp(
    const CharacterVector& names,
    const arma::sp_mat& D,
    const NumericVector& p,
    double delta,
    double lambda
) {
  int K = names.size();

  // Prepare output vectors
  CharacterVector observed_out(K);
  CharacterVector standard_out(K);
  NumericVector p_standard_out(K, NA_REAL);

  // Progress bar
  Environment utils = Environment::namespace_env("utils");
  Function txt_pb = utils["txtProgressBar"];
  Function set_pb = utils["setTxtProgressBar"];
  RObject pb = txt_pb(Named("min", 0), Named("max", K), Named("style", 3));

  // Transpose for row‐access
  arma::sp_mat Dt = D.t();

  // 1) Determine standard_out for each i
  for (int i = 0; i < K; ++i) {
    checkUserInterrupt();
    observed_out[i] = names[i];
    standard_out[i] = NA_STRING;

    // self‐posterior
    double phi_self = (1.0 - delta) * p[i];

    // neighbors of i
    const arma::uword* col_ptrs = Dt.col_ptrs;
    const arma::uword* row_inds = Dt.row_indices;
    const double*      values   = Dt.values;
    arma::uword start = col_ptrs[i];
    arma::uword end   = col_ptrs[i + 1];
    int M = static_cast<int>(end - start);

    // weight vector
    NumericVector w(M);
    for (int m = 0; m < M; ++m) {
      w[m] = std::exp(-values[start + m] * lambda);
    }
    if (M > 0 && sum(w) > 0) w = w / sum(w);

    // full posterior vector
    int N = 1 + M;
    NumericVector all_phi(N);
    all_phi[0] = phi_self;
    for (int m = 0; m < M; ++m) {
      arma::uword j = row_inds[start + m];
      all_phi[m + 1] = delta * p[j] * w[m];
    }

    // normalize
    double tot = sum(all_phi);
    if (tot > 0) all_phi = all_phi / tot;

    // find unique max
    double mx = max(all_phi);
    int count_max = 0, idx_max = -1;
    for (int k2 = 0; k2 < N; ++k2) {
      if (all_phi[k2] == mx) {
        count_max++;
        idx_max = k2;
      }
    }
    if (count_max == 1 && mx > 0) {
      if (idx_max == 0) {
        standard_out[i] = names[i];
      } else {
        standard_out[i] = names[row_inds[start + (idx_max - 1)]];
      }
    }

    set_pb(pb, i + 1);
  }

  // 2) Compute p_standard sums for each group
  std::map<std::string, double> sum_p;
  for (int i = 0; i < K; ++i) {
    if (standard_out[i] != NA_STRING) {
      std::string st = Rcpp::as<std::string>(standard_out[i]);
      sum_p[st] += p[i];
    }
  }
  for (int i = 0; i < K; ++i) {
    if (standard_out[i] != NA_STRING) {
      std::string st = Rcpp::as<std::string>(standard_out[i]);
      p_standard_out[i] = sum_p[st];
    }
  }

  return DataFrame::create(
    Named("observed")    = observed_out,
    Named("standard")    = standard_out,
    Named("p_standard")  = p_standard_out
  );
}
