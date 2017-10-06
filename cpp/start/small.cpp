// a small c++ program
#include <iostream>
#include <string>

int main() {
    // ask for a person's name
    std::cout << "Please enter your first name: ";

    // read the name
    std::string name;  // define name
    std::cin >> name;  // read into

    // write a greeting
    std::cout
        << "Hello, "
        << name
        << "!"
        << std::endl;
    return 0;
}
