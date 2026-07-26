#include <Rcpp.h>
using namespace Rcpp;

//' @title Calculate c4 prime Constant using Monte Carlo Simulation
 //' @description Calculates the bias correction factor for MSSD-based sigma estimation.
 //' @param n Sample size.
 //' @param B Number of iterations for the simulation.
 //' @name get_c4_prime_cpp
 //' @export
 // [[Rcpp::export]]
 double get_c4_prime_cpp(int n, int B = 1000000) {
     if (n < 2) return R_NaReal;
     if (n == 2) {
         // n=2 时，c4_prime 等于标准 c4(2)
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
