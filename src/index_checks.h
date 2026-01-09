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

#endif // INDEX_CHECKS_H
