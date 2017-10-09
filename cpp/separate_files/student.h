#ifndef GUARD_student_h
#define GUARD_student_h

#include <string>
#include <vector>
#include <iostream>

struct StudentInfo {
    std::string name;
    double midterm, final_;
    std::vector<double> homework;
};

bool compare(const StudentInfo& left, const StudentInfo& right);

std::istream& read_homework(std::istream& in, std::vector<double>&/*mut*/ hw);
std::istream& read(std::istream&/*mut*/ is, StudentInfo&/*mut*/ s) {

#endif
