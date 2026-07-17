#ifndef INDEX_CHECKS_H
#define INDEX_CHECKS_H

#include <Rcpp.h>
#include "BORG_types.h"

// Fast index overlap check
inline bool hasOverlap(const IndexSet& set1, const IndexSet& set2) {
   // Check smaller set against larger for efficiency
   const IndexSet& smaller = (set1.size() < set2.size()) ? set1 : set2;
   const IndexSet& larger = (set1.size() < set2.size()) ? set2 : set1;

   for (int idx : smaller) {
       if (larger.find(idx) != larger.end()) {
           return true;
       }
   }
   return false;
}

// Convert R integer vector to IndexSet
inline IndexSet toIndexSet(Rcpp::IntegerVector vec) {
   IndexSet result;
   for (int i = 0; i < vec.size(); ++i) {
       result.insert(vec[i]);
   }
   return result;
}

// Validate that every entry of a 1-based index vector lies in [1, n].
// Rcpp element access is unchecked, so callers that dereference these
// indices must guard against out-of-range or NA values first.
inline void checkIndexBounds(Rcpp::IntegerVector idx, int n, const char* name) {
   for (int i = 0; i < idx.size(); ++i) {
       int v = idx[i];
       if (Rcpp::IntegerVector::is_na(v) || v < 1 || v > n) {
           Rcpp::stop("%s contains an out-of-range index; all indices must be in [1, %d]",
                      name, n);
       }
   }
}

#endif // INDEX_CHECKS_H
