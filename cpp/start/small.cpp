// a small c++ program
#include <iostream>
#include <string>

int main() {
    // ask for a person's name
    std::cout << "Please enter your first name: ";

    // read the name
    std::string name;  // define name
    // note: confusingly this ONLY reads a single word
    std::cin >> name;  // read into

    std::string greeting = "Hello, " + name + "!";
    const std::string spaces(greeting.size(), ' ');
    const std::string inside = "* " + spaces + " *";

    const std::string outside(inside.size(), '*');

    // write a greeting
    std::cout
        << std::endl
        << outside << std::endl
        << inside << std::endl
        << "* " << greeting << " *" << std::endl
        << inside << std::endl
        << outside << std::endl;
    return 0;
}
