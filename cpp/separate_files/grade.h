#ifndef GUARD_grade_h
#define GUARD_grade_h

#include <vector>
#include "student.h"

double grade(const double fin, const double mid, const double homework);
double grade(const double fin, const double mid, const std::vector<double>& homework);
double grade(const StudentInfo&);

#endif
