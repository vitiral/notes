#include <iostream>

void plusPlus() {
  int i = 0;
  std::cout << "while i++" << '\n';
  while (i++ < 10) {
      std::cout << "i=" << i << '\n';
  }
  std::cout << "do while ++i" << '\n';
  i = 0;
  do {
      std::cout << "i=" << i << '\n';
  } while(++i < 10);
}

int main()
{
  plusPlus();
  return 0;
}
