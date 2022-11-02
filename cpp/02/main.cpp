#include <iostream>

// needed so main.cpp knows that add() is a function defined elsewhere
// Note: YOU DON'T NEED AN H FILE
// zomg, I NEVER even understood this fact.
int add(int x, int y);

// Demonstrating linker error
int myFn(int x) { return x; }

int main() {
  std::cout << "The sum of 3 and 4 is: " << add(3, 4) << '\n';
  return 0;
}
