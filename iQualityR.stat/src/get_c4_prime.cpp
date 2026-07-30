#include <Rcpp.h>
using namespace Rcpp;

//' @title Calculate c4' Constant via Monte Carlo Simulation
//' @name get_c4_prime_cpp
//' @description
//' Bias correction factor for MSSD (Mean Squared Successive Difference)
//' based sigma estimation. Returns the expected value of
//' \eqn{\sqrt{\sum (x_i - x_{i-1})^2 / (2(n-1))}} for standard normal
//' samples, used to unbias `sigma_estimate(method = "mssd")`.
//'
//' @param n Sample size (integer, >= 2).
//' @param B Number of Monte Carlo iterations (default 1,000,000).
//' @return Numeric scalar (double) — the c4' bias correction factor.
//'   Returns `NA_real_` for `n < 2`, and `sqrt(2/pi)` for `n = 2`.
//' @export
//' @examples
//' # c4' for small samples (small B for speed in examples;
//' # increase B for production accuracy)
//' set.seed(123)
//' get_c4_prime_cpp(n = 5, B = 1000)
//' get_c4_prime_cpp(n = 10, B = 1000)
//'
//' # n = 2 has closed form sqrt(2/pi) — no simulation needed
//' get_c4_prime_cpp(n = 2)  # approx 0.7978846
//' sqrt(2 / pi)
// [[Rcpp::export]]
 double get_c4_prime_cpp(int n, int B = 1000000) {
     if (n < 2) return R_NaReal;
     if (n == 2) {
         // n=2: c4' equals standard c4(2) = sqrt(2/pi)
         return sqrt(2.0/M_PI);
     }

     double total_sqrt_mssd = 0;
     for(int b = 0; b < B; ++b) {
         double sum_diff_sq = 0;
         double x_prev = R::rnorm(0, 1);
         for(int i = 1; i < n; ++i) {
             double x_curr = R::rnorm(0, 1);
             sum_diff_sq += pow(x_curr - x_prev, 2);
             x_prev = x_curr;
         }
         total_sqrt_mssd += sqrt(sum_diff_sq / (2.0 * (n - 1)));
     }
     return total_sqrt_mssd / B;
 }
