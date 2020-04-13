#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<bool> IntervalContains(NumericVector x, long double inf, long double sup, long double min,
                                      long double max, bool bound, const char* class_str) {
  int size = x.size();

  std::vector<bool>  ret(size, true);

  for(int i = 0; i < size; i++){
    if (std::isnan (x[i])) {
      ret[i] = false;
    } else if (std::strcmp (class_str, "integer") == 0 && std::floor (x[i]) != x[i]) {
      ret[i] = false;
    } else if (bound & (x[i] < inf || x[i] > sup)) {
      ret[i] = false;
    } else if (!bound & (x[i] < min || x[i] > max)) {
      ret[i] = false;
    }
  }

  return ret;
}

// [[Rcpp::export]]
bool IntervalContainsAll(NumericVector x, long double inf, long double sup, long double min,
                             long double max, bool bound, const char* class_str) {
  int size = x.size();

  for(int i = 0; i < size; i++){
    if (std::isnan (x[i])) {
      return false;
    } else if (std::strcmp (class_str, "integer") == 0 && std::floor (x[i]) != x[i]) {
      return false;
    } else if (bound & (x[i] < inf || x[i] > sup)) {
      return false;
    } else if(!bound & (x[i] < min || x[i] > max)) {
      return false;
    }
  }

  return true;
}
