
nodes = [
	{
		"associatedFServiceID": 0,
		"associatedMasterServiceID": 3,
		"attributes": {},
		"cip": "10.26.66.86",
		"cipi": "Bond10G",
		"fibreChannelTargetPortGroup": null,
		"mip": "172.26.66.86",
		"mipi": "Bond1G",
		"name": "VWC-EN336",
		"nodeID": 3,
		"platformInfo": {
			"chassisType": "R620",
			"cpuModel": "Intel(R) Xeon(R) CPU E5-2640 0 @ 2.50GHz",
			"nodeMemoryGB": 72,
			"nodeType": "SF3010"
		},
		"sip": "10.26.66.86",
		"sipi": "Bond10G",
		"softwareVersion": "8.2.0.198",
		"uuid": "4C4C4544-0047-4810-8051-C2C04F515631",
		"virtualNetworks": []
	},
	{
		"associatedFServiceID": 0,
		"associatedMasterServiceID": 2,
		"attributes": {},
		"cip": "10.26.66.85",
		"cipi": "Bond10G",
		"fibreChannelTargetPortGroup": null,
		"mip": "172.26.66.85",
		"mipi": "Bond1G",
		"name": "VWC-EN335",
		"nodeID": 2,
		"platformInfo": {
			"chassisType": "R620",
			"cpuModel": "Intel(R) Xeon(R) CPU E5-2640 0 @ 2.50GHz",
			"nodeMemoryGB": 72,
			"nodeType": "SF3010"
		},
		"sip": "10.26.66.85",
		"sipi": "Bond10G",
		"softwareVersion": "8.1.1.5",
		"uuid": "4C4C4544-005A-4310-8051-B3C04F515631",
		"virtualNetworks": []
	},
	{
		"associatedFServiceID": 0,
		"associatedMasterServiceID": 4,
		"attributes": {},
		"cip": "10.26.66.87",
		"cipi": "Bond10G",
		"fibreChannelTargetPortGroup": null,
		"mip": "172.26.66.87",
		"mipi": "Bond1G",
		"name": "VWC-EN337",
		"nodeID": 4,
		"platformInfo": {
			"chassisType": "R620",
			"cpuModel": "Intel(R) Xeon(R) CPU E5-2640 0 @ 2.50GHz",
			"nodeMemoryGB": 72,
			"nodeType": "SF3010"
		},
		"sip": "10.26.66.87",
		"sipi": "Bond10G",
		"softwareVersion": "8.1.1.5",
		"uuid": "4C4C4544-0033-3310-8051-C7C04F515631",
		"virtualNetworks": []
	}
]

version = "8.1.1.5",

orphan = function (){
    for (var i = 0; i < nodes.length; i++) {
        if (nodes[i]["softwareVersion"] == version) {
            return nodes[i]
        }
    }
}()

print(orphan.softwareVersion)

