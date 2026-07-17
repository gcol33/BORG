#include <Rcpp.h>
#include "BORG_types.h"
#include "index_checks.h"
#include "hash_utils.h"

using namespace Rcpp;

// [[Rcpp::export]]
List checkIndexOverlap(IntegerVector train_idx, IntegerVector test_idx) {
   // Indices are used as opaque set keys (never dereferenced), so they are
   // compared as-is without any base conversion.
   IndexSet train_set;
   for (int i = 0; i < train_idx.size(); ++i) {
       train_set.insert(train_idx[i]);
   }

   std::vector<int> overlap;
   for (int i = 0; i < test_idx.size(); ++i) {
       int idx = test_idx[i];
       if (train_set.find(idx) != train_set.end()) {
           overlap.push_back(idx);
       }
   }

   return List::create(
       Named("has_overlap") = overlap.size() > 0,
       Named("n_overlap") = (int)overlap.size(),
       Named("overlap_indices") = wrap(overlap)
   );
}

// [[Rcpp::export]]
List checkDuplicateRows(
   NumericMatrix data,
   IntegerVector train_idx,
   IntegerVector test_idx
) {
   // Exact row-value duplicate detection (bit-for-bit, full precision).

   int n_row = data.nrow();
   checkIndexBounds(train_idx, n_row, "train_idx");
   checkIndexBounds(test_idx, n_row, "test_idx");

   int n_train = train_idx.size();
   int n_test = test_idx.size();
   int p = data.ncol();

   // Build the set of exact training-row keys
   std::unordered_set<std::string> train_keys;

   for (int i = 0; i < n_train; ++i) {
       int row_idx = train_idx[i] - 1;  // 1-based -> 0-based
       train_keys.insert(rowKey(data, row_idx, p));
   }

   // Check test rows for exact matches against training rows
   std::vector<int> duplicates;

   for (int i = 0; i < n_test; ++i) {
       int row_idx = test_idx[i] - 1;  // 1-based -> 0-based
       if (train_keys.find(rowKey(data, row_idx, p)) != train_keys.end()) {
           duplicates.push_back(test_idx[i]);  // Return 1-based
       }
   }

   return List::create(
       Named("has_duplicates") = duplicates.size() > 0,
       Named("n_duplicates") = (int)duplicates.size(),
       Named("duplicate_indices") = wrap(duplicates)
   );
}

// [[Rcpp::export]]
List checkTemporalOrder(
   NumericVector timestamps,
   IntegerVector train_idx,
   IntegerVector test_idx
) {
   int n_ts = timestamps.size();
   checkIndexBounds(train_idx, n_ts, "train_idx");
   checkIndexBounds(test_idx, n_ts, "test_idx");

   // Find max train timestamp
   double max_train_time = R_NegInf;
   for (int i = 0; i < train_idx.size(); ++i) {
       int idx = train_idx[i] - 1;  // 0-based
       if (!NumericVector::is_na(timestamps[idx])) {
           if (timestamps[idx] > max_train_time) {
               max_train_time = timestamps[idx];
           }
       }
   }

   // Find test observations that predate training
   std::vector<int> violations;
   for (int i = 0; i < test_idx.size(); ++i) {
       int idx = test_idx[i] - 1;  // 0-based
       if (!NumericVector::is_na(timestamps[idx])) {
           if (timestamps[idx] < max_train_time) {
               violations.push_back(test_idx[i]);  // 1-based
           }
       }
   }

   return List::create(
       Named("has_violations") = violations.size() > 0,
       Named("n_violations") = (int)violations.size(),
       Named("violation_indices") = wrap(violations),
       Named("max_train_time") = max_train_time
   );
}

// [[Rcpp::export]]
List checkGroupOverlap(
   IntegerVector groups,
   IntegerVector train_idx,
   IntegerVector test_idx
) {
   int n_grp = groups.size();
   checkIndexBounds(train_idx, n_grp, "train_idx");
   checkIndexBounds(test_idx, n_grp, "test_idx");

   // Build set of training groups
   std::unordered_set<int> train_groups;
   for (int i = 0; i < train_idx.size(); ++i) {
       int idx = train_idx[i] - 1;  // 0-based
       train_groups.insert(groups[idx]);
   }

   // Check for overlapping groups in test
   std::unordered_set<int> overlapping_groups;
   std::vector<int> violation_indices;

   for (int i = 0; i < test_idx.size(); ++i) {
       int idx = test_idx[i] - 1;  // 0-based
       int group = groups[idx];
       if (train_groups.find(group) != train_groups.end()) {
           overlapping_groups.insert(group);
           violation_indices.push_back(test_idx[i]);  // 1-based
       }
   }

   std::vector<int> overlap_vec(overlapping_groups.begin(), overlapping_groups.end());

   return List::create(
       Named("has_overlap") = overlapping_groups.size() > 0,
       Named("n_overlapping_groups") = (int)overlapping_groups.size(),
       Named("overlapping_groups") = wrap(overlap_vec),
       Named("n_affected_rows") = (int)violation_indices.size(),
       Named("affected_indices") = wrap(violation_indices)
   );
}

// [[Rcpp::export]]
double computeCorrelation(NumericVector x, NumericVector y) {
   // Pearson correlation with NA handling
   int n = x.size();
   if (n != y.size()) {
       stop("Vectors must have same length");
   }

   // First pass: pairwise-complete means
   double sum_x = 0, sum_y = 0;
   int count = 0;
   for (int i = 0; i < n; ++i) {
       if (!NumericVector::is_na(x[i]) && !NumericVector::is_na(y[i])) {
           sum_x += x[i];
           sum_y += y[i];
           count++;
       }
   }

   if (count < 2) {
       return NA_REAL;
   }

   double mean_x = sum_x / count;
   double mean_y = sum_y / count;

   // Second pass: accumulate centered products (avoids the catastrophic
   // cancellation of the sum-of-squares form for large-magnitude data)
   double var_x = 0, var_y = 0, cov_xy = 0;
   for (int i = 0; i < n; ++i) {
       if (!NumericVector::is_na(x[i]) && !NumericVector::is_na(y[i])) {
           double dx = x[i] - mean_x;
           double dy = y[i] - mean_y;
           var_x += dx * dx;
           var_y += dy * dy;
           cov_xy += dx * dy;
       }
   }

   if (var_x <= 0 || var_y <= 0) {
       return NA_REAL;
   }

   return cov_xy / (sqrt(var_x) * sqrt(var_y));
}
