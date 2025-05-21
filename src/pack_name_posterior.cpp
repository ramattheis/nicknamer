// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List pack_name_posterior_cpp(
    CharacterVector names,    // data$string
    List         neighbor_list,
    NumericVector x_avg,      // post$x_avg
    NumericVector p_avg       // post$p_avg
) {
  int K = names.size();
  List out(K);

  for (int k = 0; k < K; ++k) {

    // pull out the CHARSXP and turn it into std::string
    std::string nm = Rcpp::as<std::string>(names[k]);

    // grab the k-th neighbor info
    List nb = neighbor_list[k];
    IntegerVector js = nb["j"];         // 1-based indices
    int M = js.size();

    // build the xs and ps vectors
    NumericVector xs(M), ps(M);
    for (int i = 0; i < M; ++i) {
      int idx = js[i] - 1;              // convert to 0-based
      xs[i] = x_avg[idx];
      ps[i] = p_avg[idx];
    }

    // assemble the packed list element
    out[k] = List::create(
      Named("name") = nm,
      Named("nb")   = nb,
      Named("id")   = k + 1,            // to match R’s 1-based id
      Named("x")    = x_avg[k],
      Named("p")    = p_avg[k],
      Named("xs")   = xs,
      Named("ps")   = ps
    );
  }

  return out;
}
