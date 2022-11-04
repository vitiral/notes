#include <iostream>

int main() {
  int length{8};
  int* array{ new int[length]{} }; // length is NOT constant
  array[0] = 5;
  std::cout << "array@0 = " << array[0] << '\n';
  delete array;
  return 0;
}
