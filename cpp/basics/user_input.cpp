// take user input to compute grades

#include <iomanip>
#include <ios>
#include <iostream>
#include <string>

using std::cin;
using std::cout;
using std::endl;

using std::setprecision;
using std::string;
using std::streamsize;

int main() {
    cout << "Enter first name: ";
    string name;
    cin >> name;
    cout << "Hello, " << name << endl;

    // get midterm and final grades
    cout << "Please enter your midterm and final grades: ";
    double mid, fin;
    cin >> mid >> fin;

    // ask for homework grades
    cout << "Enter homework grades, followed by EOF: ";
    int count = 0;
    double sum = 0;

    double x; // value to read into

    // invarant: we have read `count` grades so far, sum is their sum.
    // note: instance `cin` can evaluate to an integer, which gets converted into a bool
    // The value is based on the last-read value.
    // Object Oriented for-the-win yes?
    while (cin >> x) {
        ++count;
        sum += x;
    }

    // write the result
    streamsize prec = cout.precision();
    cout << "Your final grade is "
        << setprecision(3)
        << 0.2 * mid + 0.4 * fin + 0.4 * sum / count
        << setprecision(prec)
        << endl;
    return 0;
}
