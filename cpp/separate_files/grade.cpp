#include <stdexcept>
#include <vector>
#include "grade.h"
#include "calc.h"
#include "student.h"

using std::domain_error;
using std::vector;

// compute the final course grade
double grade(const double mid, const double fin, const double homework) {
    return 0.2 * mid + 0.4 * fin + 0.4 * homework;
}

double grade(const double mid, const double fin, const vector<double>& hw) {
    if (hw.size() == 0) {
        throw domain_error("student has done no homework");
    }
    return grade(mid, fin, median(hw /* !auto-clone! */));
}

double grade(const StudentInfo& student) {
    return grade(student.midterm, student.final_, /*ref*/student.homework);
}

