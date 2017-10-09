
#include <algorithm>
#include <iomanip>
#include <ios>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

#include "grade.h"
#include "student.h"

using std::cin;
using std::cout;
using std::domain_error;
using std::endl;
using std::max;

using std::setprecision;
using std::sort;
using std::streamsize;
using std::string;
using std::vector;

int main() {
    vector<StudentInfo> students;

    StudentInfo record;
    string::size_type maxlen = 0;  // max length of longest name

    // read and store all student data
    // invariant: `students` contains all student data read so far
    //   maxlen contains length of longest student name.
    while (read(cin, record)) {
        // find length of longest name
        maxlen = max(maxlen, record.name.size());
        students.push_back(record);
    }

    // alphabetize by name
    sort(students.begin(), students.end(), compare);

    // write the names and grades
    for (vector<StudentInfo>::size_type i = 0;
         i != students.size(); ++i) {

        // write the name, padded on the right to maxlen + 1 chars
        cout << students[i].name
            << string(maxlen + 1 - students.name.size(), ' ');

        // try and write the grade
        try {
            double final_grade = grade(students[i]);
            streamsize prec = cout.precision();
            cout << setprecision(3) << final_grade
                << setprecision(prec);
        } catch (domain_error e) {
            cout << e.what();
        }
        cout << endl;
    }
    return 0;
}
