
#include <iostream>

int main() {
  std::cout << "Enter an int: " << std::flush;
  int inp {};
  std::cin >> inp;
  std::cout << "Double that number: " << (inp * 2) << std::endl;

  return 0;
}
