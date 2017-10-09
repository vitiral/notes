
#include <vector>
using std::vector;

#include <stdexcept>
using std::domain_error;

#include <algorithm>
using std::sort;

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

