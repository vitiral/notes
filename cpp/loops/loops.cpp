#include <iostream>
#include <string>

using std::cout;
using std::cin;
using std::string;
using std::endl;

int main()
{
    // ask for the person's name
    cout << "Please enter your first name: ";
    // read the name
    string name;
    cin >> name;

    // build the message that we intend to write
    const string greeting = "Hello, " + name + "!";

    // the number of spaces around the name
    const size_t pad = 1;

    // calculated total number of of rows
    // - padding is done on top + bottom
    // - plus the name line itself and outside box
    const size_t rows = pad * 2 + 3;

    // calculated total number of cols
    const string::size_type cols = greeting.size() + pad * 2 + 2;

    int r = 0;
    // invariant: r rows have been written
    while (r < rows) {
        string::size_type c = 0;
        // invariant: we have already written `c` chars to the row
        while ( c < cols) {
            if (r == 0 || r == rows - 1 || c == 0 || c == cols -1 ) {
                // border column or row
                cout << "*";
                ++c;
            } else if (r == pad + 1 && c == pad + 1) {
                cout << greeting;
                c += greeting.size();
            } else {
                // pad column/row
                cout << " ";
                ++c;
            }
        }
        cout << endl;
        ++r;
    }

    return 0;
}
