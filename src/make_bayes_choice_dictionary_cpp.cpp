// force 64-bit indices
#define ARMA_64BIT_WORD

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
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

  // Initialize progress bar
  Environment utils = Environment::namespace_env("utils");
  Function txt_pb = utils["txtProgressBar"];
  Function set_pb = utils["setTxtProgressBar"];
  RObject pb = txt_pb(Named("min", 0), Named("max", K), Named("style", 3));

  // Precompute transpose for efficient row access
  arma::sp_mat Dt = D.t();

  for (int i = 0; i < K; ++i) {
    checkUserInterrupt();
    observed_out[i] = names[i];
    standard_out[i] = NA_STRING;

    // 1) Self posterior
    double phi_self = (1.0 - delta) * p[i];

    // 2) Extract neighbors
    const arma::uword* col_ptrs = Dt.col_ptrs;
    const arma::uword* row_inds = Dt.row_indices;
    const double*      values   = Dt.values;
    arma::uword start = col_ptrs[i];
    arma::uword end   = col_ptrs[i + 1];
    int M = static_cast<int>(end - start);

    // 3) Compute neighbor contributions
    NumericVector w(M);
    for (int m = 0; m < M; ++m) {
      double d = values[start + m];
      w[m] = std::exp(-d / lambda);
    }
    if (M > 0 && sum(w) > 0) w = w / sum(w);

    // 4) Compute full posterior vector (self + neighbors)
    int N = 1 + M;
    NumericVector all_phi(N);
    all_phi[0] = phi_self;
    for (int m = 0; m < M; ++m) {
      arma::uword j = row_inds[start + m];
      all_phi[m + 1] = delta * p[j] * w[m];
    }

    // 5) Normalize
    double tot = sum(all_phi);
    if (tot > 0) all_phi = all_phi / tot;

    // 6) Identify max and check uniqueness
    double mx = max(all_phi);
    int count_max = 0;
    int idx_max = -1;
    for (int k2 = 0; k2 < N; ++k2) {
      if (all_phi[k2] == mx) {
        count_max++;
        idx_max = k2;
      }
    }
    // Only record if unique and non-zero
    if (count_max == 1 && mx > 0) {
      // idx_max==0 means self; else neighbor at row_inds
      if (idx_max == 0) {
        standard_out[i] = names[i];
      } else {
        standard_out[i] = names[ row_inds[start + (idx_max - 1)] ];
      }
    }

    // Advance progress bar
    set_pb(pb, i + 1);
  }

  return DataFrame::create(
    Named("observed") = observed_out,
    Named("standard") = standard_out
  );
}
