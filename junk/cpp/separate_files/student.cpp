#include <string>
#include <vector>
#include <iostream>

#include "student.h"

using std::vector;
using std::istream;

bool compare(const StudentInfo& left, const StudentInfo& right) {
    return left.name < right.name;
}

// read homework grades into a vector
istream& read_homework(istream&/*mut*/ is, vector<double>&/*mut*/ hw) {
    // There might already be an error condition with is (??? wtf ???)
    // so I guess we don't do anything in that case lolz
    if (is) {
        // For some insane reason this makes sense I guess?
        // ... not really -- the caller should totally control
        // this...
        hw.clear();

        // Read homework grades

        double x;
        // note: instance of `istream` can evaluate to an integer, which gets
        // converted into a bool The value is based on the last-read value.
        // Object Oriented + side effect + return-vals from operators
        // for-the-win yes? ... oh ya, EXTREMELY clear, thankyou.
        while (is >> x) {
            hw.push_back(x);
        }

        // from tutorail: clear the stream so that input will work for the next
        // student...  Okay, I guess since we are ignoring errors that makes
        // sense.
        is.clear();
    }
    return is;
}


istream& read(istream&/*mut*/ is, StudentInfo&/*mut*/ s) {
    // read and store the student's name, midterm and final
    is >> s.name >> s, s.midterm >> s.final_;

    hread_hw(is, s.homework); // read and store all of student's homework grades
    return is;
}
