// injected variables
current_build = {
  "version": "10.7.0.25"
  //"version": "8.7.0.25"
}

released_version_strs = [
  "8.2.0.198",
  "8.4.0.68",
  "8.5.0.13",
  "8.6.0.25",
  "9.0.0.1554",
  "9.1.0.38",
  "9.2.0.43",
]

// Helper Methods

// compare two equal size arrays
// returns: 1 if `a` is larger, -1 if `a` is smaller, 0 if equal
compareArrays = function(a, b) {
  var elA, elB, i, len;
  if (a.length != b.length) {
    throw "array lengths are not equal: " + a.length + " != " + b.length
  }

  for (i = 0, len = Math.min(a.length, b.length); i < len; i++) {
    elA = a[i], elB = b[i];
    if (elA > elB) return 1;
    if (elA < elB) return -1;
  }

  return 0;
};


// get a version "object" (a split array)
getVersionRay = function(v) {
  return v.split(".").map(parseFloat)
}

// return ray to str
getVersionStr = function(a) {
  return a.join(".")
}

current_build_ray = getVersionRay(current_build.version)
released_version_rays = released_version_strs.map(getVersionRay)

// versions must be less than current_build AND
// must be less than 2 major versions below
use_version_rays = released_version_rays.filter(
  function(v) {
    return (compareArrays(current_build_ray, v) == 1
     && v[0] >= current_build_ray[0] - 1
    );
  }
)
use_version_strs = use_version_rays.map(getVersionStr)

print("use_versions:", JSON.stringify(use_version_strs));
