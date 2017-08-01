// injected variables
current_build = {
  "version": "10.7.0.25"
  //"version": "8.7.0.25"
}

version_strs = [
  "7.4.1.2",
  "8.2.0.198",
  "8.4.0.68",
  "8.5.0.13",
  "8.6.0.25",
  "9.0.0.1554",
  "9.1.0.38",
  "9.2.0.43",
]

// copy pasta

out = function() {

  // compare two equal size arrays
  // returns: 1 if `a` is larger, -1 if `a` is smaller, 0 if equal
  compareArrays = function(a, b) {
    if (a.length != b.length) {
      throw "array lengths are not equal: " + a.length + " != " + b.length
    }

    for (i = 0; i < a.length; i++) {
      if (a[i] > b[i]) return 1;
      if (a[i] < b[i]) return -1;
    }

    return 0;
  };

  // get a version "object" (a split array)
  getVersionRay = function(v) {
    return v.split(".").map(parseFloat)
  }

  map = function(f, l) {
    var i = 0;
    var out = [];
    for (i = 0; i < l.length; i++) {
      out.push(f(l[i]));
    }
    return out;
  }

  filter = function(f, l) {
    var i = 0;
    var out = [];
    for (i = 0; i < l.length; i++) {
      if (f(l[i])) {
        out.push(l[i]);
      }
    }
    return out;
  }

  // return ray to str
  getVersionStr = function(a) {
    return a.join(".")
  }

  current_build_ray = getVersionRay(current_build.version)
  version_rays = map(getVersionRay, version_strs)

  // versions must be less than current_build AND
  // must be less than 2 major versions below
  use_version_rays = filter(
    function(v) {
      return (compareArrays(current_build_ray, v) == 1
       && v[0] >= current_build_ray[0] - 1
      );
    }
    , version_rays)
  use_version_strs = map(getVersionStr, use_version_rays);
  return use_version_strs;
}()

print("use_versions:", JSON.stringify(out));
