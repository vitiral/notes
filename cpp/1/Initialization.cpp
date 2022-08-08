/*
 * It used to be that some types required copy initialiation, and some requred
 * direct. Brace was added to provide a more consistent syntax. It also allows
 * initializing with a list of values, hence it is sometimes called list
 * initalization.
 */
#include <iostream>

int main()
{
  int x = 4;    // Copy initialization (not recommended)
  int y(4);     // Direct initialization (rarely used)
  int z = (4);  // Not sure... copy direct?
  
  int width1 { 5 }; // direct brace, aka uniform, init
  int width2 = { 5 }; // copy brace
  int width3{}; // value initialization, aka zero initialization

  std::cout << "Yo " << width1 << " " << x << '\n'
            << "Now enter two numbers separated by spaces: "
            << std::flush;

  int inp1{};
  int inp2{};
  std::cin >> inp1 >> inp2;
  std::cout << "You entered " << inp1 << " and " << inp2 << std::endl;

  int uninit;
  std::cout << "Unint: " << uninit << std::endl;

  return 0;
}
