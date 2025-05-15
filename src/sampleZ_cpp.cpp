// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List sampleZ_cpp(
    const IntegerVector& n,
    const NumericVector& p,
    double delta,
    const List& neighbor_list,
    const List& G_list
) {
  int K = n.size();
  std::vector<int> i_out, j_out, x_out;

  for(int k = 0; k < K; ++k) {
    int N = n[k];
    if(N <= 0) continue;

    // neighbors and precomputed weights
    IntegerVector js = neighbor_list[k];
    NumericVector Gk = G_list[k];
    int m = js.size();

    // build probability vector: self + neighbors
    NumericVector probs(m + 1);
    probs[0] = (1.0 - delta) * p[k];
    double sumw = probs[0];
    for(int t = 0; t < m; ++t) {
      int idx = js[t] - 1;  // 0-based index
      double w = delta * p[idx] * Gk[t];
      probs[t + 1] = w;
      sumw += w;
    }
    for(int t = 0; t <= m; ++t) {
      probs[t] /= sumw;
    }

    // draw counts
    IntegerVector counts(m + 1);
    R::rmultinom(N, probs.begin(), m + 1, counts.begin());

    // record nonzeros
    if(counts[0] > 0) {
      i_out.push_back(k + 1);
      j_out.push_back(k + 1);
      x_out.push_back(counts[0]);
    }
    for(int t = 0; t < m; ++t) {
      if(counts[t + 1] > 0) {
        i_out.push_back(js[t]);
        j_out.push_back(k + 1);
        x_out.push_back(counts[t + 1]);
      }
    }
  }
  return List::create(
    Named("i") = i_out,
    Named("j") = j_out,
    Named("x") = x_out
  );
}
