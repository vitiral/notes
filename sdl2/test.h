#ifndef __RETT_TEST_H
#define __RETT_TEST_H

#define TEST(NAME) \
  void test_ ## NAME() {                    \
  cout << "## Running test_" #NAME << '\n';

#define END_TEST  }

#define CALL_TEST(NAME)       test_ ## NAME()

#define ASSERT_EQ(A, B) \
  if (not (A == B)) {   \
    cout << "FAIL! " << A << " != " << B << '\n'; \
    assert(A == B); \
  }

#endif //__RETT_TEST_H
