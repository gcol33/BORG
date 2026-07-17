#ifndef HASH_UTILS_H
#define HASH_UTILS_H

#include <Rcpp.h>
#include <string>
#include <functional>
#include <cstring>

// Exact row key: the concatenated raw bytes of each cell's double value.
// Two rows produce the same key iff every cell is bit-for-bit identical, so
// comparison is exact at full double precision (no decimal truncation) and
// works for values of any magnitude, including sub-1e-6 and NA/NaN.
inline std::string rowKey(const Rcpp::NumericMatrix& data, int row_idx, int p) {
   std::string key;
   key.reserve(static_cast<size_t>(p) * sizeof(double));
   for (int j = 0; j < p; ++j) {
       double v = data(row_idx, j);
       char bytes[sizeof(double)];
       std::memcpy(bytes, &v, sizeof(double));
       key.append(bytes, sizeof(double));
   }
   return key;
}

// Simple string hash combining for row comparison
// Uses FNV-1a hash algorithm
inline size_t fnv1a_hash(const std::string& str) {
   size_t hash = 14695981039346656037ULL;  // FNV offset basis
   for (char c : str) {
       hash ^= static_cast<size_t>(c);
       hash *= 1099511628211ULL;  // FNV prime
   }
   return hash;
}

// Combine multiple hash values
inline size_t combineHashes(size_t h1, size_t h2) {
   // Boost-style hash combine
   return h1 ^ (h2 + 0x9e3779b9 + (h1 << 6) + (h1 >> 2));
}

#endif // HASH_UTILS_H
