
#include <functional>
#include <iostream>
#include <cassert>


enum class Op: int {
  Add, Sub,
};

int add(int a, int b) { return a + b; }
int sub(int a, int b) { return a - b; }

using Operator = int(*)(int, int);
Operator operators[] = {add, sub};

std::istream& operator>> (std::istream& is, Op& val) {
  std::string s = std::string("hello");
  is >> s;
  if(s == "+")        val = Op::Add;
  else if (s == "-")  val = Op::Sub;
  else assert(0);
  return is;
}

int main() {
  Op op;
  std::cout << "Choose an operator [+ -]: ";
  std::cin >> op;
  int opI = static_cast<int>(op);
  std::cout << "Got opI: " << opI << '\n';
  Operator opFn = operators[opI];
  std::cout << "Choose two values: ";
  int a, b;
  std::cin >> a >> b;
  std::cout << "Result: " << opFn(a, b) << '\n';
  return 0;
}
