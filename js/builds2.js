// injected variables
current_build = {
  "version": "10.7.0.25"
  //"version": "8.7.0.25"
}

builds = [
  {buildID: 1, version: "7.4.1.2"},
  {buildID: 2, version: "8.2.0.198"},
  {buildID: 3, version: "8.4.0.68"},
  {buildID: 4, version: "8.5.0.13"},
  {buildID: 5, version: "8.6.0.25"},
  {buildID: 6, version: "9.0.0.1554"},
  {buildID: 7, version: "9.1.0.38"},
  {buildID: 8, version: "9.2.0.43"},
  {buildID: 0, version: "9.3.0.40"},
]

// copy pasta

out = function() {

  // compare two equal size arrays
  // returns: 1 if `a` is larger, -1 if `a` is smaller, 0 if equal
  compareArrays = function(a, b) {
    if (a.length != b.length) {
      throw "array lengths are not equal: " + a.length + " != " + b.length;
    }

    for (var i = 0; i < a.length; i++) {
      if (a[i] > b[i]) return 1;
      if (a[i] < b[i]) return -1;
    }

    return 0;
  };

  // get a version "object" (a split array)
  getVersionRay = function(v) {
    return v.split(".").map(parseFloat);
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
    return a.join(".");
  }

  removeIndexes = function(ray, indexes) {
    var out = [];
    for (var i=0; i < ray.length; i++) {
      if (indexes.indexOf(i) == -1) {
        out.push(ray[i]);
      }
    }
    return out;
  }

  var current_version = getVersionRay(current_build.version);
  var fluorine_ga = getVersionRay("9.0.0.1554");
  var fluorine_patch1 = getVersionRay("9.1.0.38");
  var fluorine_patch2 = getVersionRay("9.2.0.43");

  // FOGBUGS 24037: remove fluorine patch 0 and 1 if build > patch 2
  var check_24037 = function(build) {
    var version = getVersionRay(build.version);
    return !(compareArrays(version, fluorine_ga) == 0 || compareArrays(version, fluorine_patch1) == 0);
  }

  var out = filter(check_24037, builds);

  return out;
}()

print("out:", JSON.stringify(out));
