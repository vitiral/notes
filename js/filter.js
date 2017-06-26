

nodes = [{"nodeName": '1'}, {"nodeName": '2'}]
orphan_nodes = [nodes[1]]

print("nodes:", JSON.stringify(nodes))
print("orphan_nodes:", JSON.stringify(orphan_nodes))

out = (function() {
  names = {};
  for (i=0; i<nodes.orphan_nodes.length; i++) {
    node = orphan_nodes[i]
    names[node["nodeName"]] = true;
  }
  out = [];
  for (i=0; i<nodes.length; i++) {
    node = nodes[i]
    if (!(node["nodeName"] in names)) {
      out.push(node)
    }
  }
  return out;
})();

print("Got:", JSON.stringify(out));
