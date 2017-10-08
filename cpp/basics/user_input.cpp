// take user input to compute grades

#include <iomanip>
#include <ios>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <stdexcept>

/* iomanip::    */  using std::setprecision;

// iostream::
using std::cin;
using std::cout;
using std::endl;

/* string::     */  using std::string;
/* ??           */  using std::streamsize;
/* vector::     */  using std::vector;
/* algorithm::  */  using std::sort;
/* stdexcept::  */  using std::domain_error;

// compute the median of a vector
double median(vector<double> vec) {
    // I get the feeling that `size_type` changes depending on the size of the
    // item in vector... incredible -- I never even considered that.
    typedef vector<double>::size_type vec_sz;
    vec_sz size = vec.size();

    if (0 == size) {
        throw domain_error("median of an empty vector");
    }

    sort(vec.begin(), vec.end());

    // wow... this has be the the most foot-shootie thing I have ever seen...
    // you can literally do this:
    //     vector<double> testing;
    //     testing.push_back(2.3);
    //     sort(vec.begin(), testing.end());

    const vec_sz i = size / 2;
    if (size % 2 == 0) {
        // invarant: smallest size == 1
        return vec[i] + vec[i - 1] / 2;
    } else {
        return vec[i];
    }
}

// compute the final course grade
double course_grade(const double mid, const double fin, const double homework) {
    return 0.2 * mid + 0.4 * fin + 0.4 * homework;
}

double course_grade(const double mid, const double fin, const vector<double>& hw) {
    if (hw.size() == 0) {
        throw domain_error("student has done no homework");
    }
    return course_grade(mid, fin, median(hw /* !auto-clone! */));
}


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
    vector<double> homework;
    double x; // value to read into

    // invarant: we have read `count` grades so far, sum is their sum.
    // note: instance `cin` can evaluate to an integer, which gets converted into a bool
    // The value is based on the last-read value.
    // Object Oriented for-the-win yes?
    while (cin >> x) {
        homework.push_back(x);
    }

    // write the result
    streamsize prec = cout.precision();
    // note the "auto reference" -- references act similar to the ones
    // in rust, but in C++ they are supposedly more like a
    // "non-copy alias that you can pass between functions"... weird.
    cout << "Your final grade is "
        << setprecision(3)
        /* << course_grade(mid, fin, median(homework)) */
        << course_grade(mid, fin, /* ! auto reference ! */ homework)
        << setprecision(prec)
        << endl;
    return 0;
}
