
#include <iostream>
#include "template.h"

int main() {
  // The following compile and run
  std::cout << max(1, 2) << '\n';
  std::cout << max(1.5, 2.5) << std::endl;

  // The following will only compile with multiple types in the max template.
  std::cout << max(1, 2.5) << std::endl; // returns an int (IFF return type is T)
  std::cout << max(3.1, 2) << std::endl; // returns a double (IFF return type is T)
}
