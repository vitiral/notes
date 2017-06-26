
var jsonServer = require('json-server')

// Returns an Express server
var server = jsonServer.create()

// use default middleware
server.use(jsonServer.defaults())

var router = jsonServer.router('db.json')
server.use(router)

var port = 4000
console.log('Listening at port ' + 4000)
server.listen(port)

