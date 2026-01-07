#ifndef HASH_UTILS_H
#define HASH_UTILS_H

#include <string>
#include <functional>

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
