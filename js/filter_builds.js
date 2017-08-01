
builds = [
  {buildID: 1},
  {buildID: 2},
  {buildID: 3},
  {buildID: 2},
]
print("In : ", JSON.stringify(builds));

out = function () {
  var seen = {};
  var out = [];
  for (var i = 0; i < builds.length; i++) {
    var build = builds[i]
    var id = build.buildID.toString();
    if (!(id in seen)) {
      seen[id] = true;
      out.push(build);
    }
  }
  return out;
}()

print("Out: ", JSON.stringify(out));
