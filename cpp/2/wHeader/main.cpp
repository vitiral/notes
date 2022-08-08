#include <iostream>
#include "add.hpp"


// Demonstrating linker error
int myFn(int x) { return x; }

int main() {
  std::cout << "The sum of 3 and 4 is: " << add(3, 4) << '\n';
  return 0;
}
