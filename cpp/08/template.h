#ifndef __TEMPLATE_H
#define __TEMPLATE_H

// Alternatives to using multiple types:
// - Define a concrete `max` which uses double. Drawback: template will never be
//   used if the function can be matched with numerical type conversion.
// - Require caller to use max<double>(1.5, 2) or max<int>(1.5, 2) -- whichever
//   they prefer.
//
// `auto` is used so that the compiler determines the "best" native type,
// instead of always returning type T or U.
template <typename T, typename U>
auto max(T x, U y) {
  return (x > y) ? x : y;
}

#endif
