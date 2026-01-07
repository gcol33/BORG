#ifndef BORG_TYPES_H
#define BORG_TYPES_H

#include <vector>
#include <string>
#include <unordered_set>

// Index set for efficient lookup
typedef std::unordered_set<int> IndexSet;

// Risk structure
struct BorgRiskItem {
   std::string type;
   std::string severity;  // "hard_violation" or "soft_inflation"
   std::string description;
   std::vector<int> affected_indices;
   std::string source_object;
};

typedef std::vector<BorgRiskItem> RiskList;

#endif // BORG_TYPES_H
