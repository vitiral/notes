
function hello(name) {
    console.log( `Hello ${name}!` );
}

function getFile(path) {
    $.getJSON(path, function(json) {
        console.log(json); // this will show the info it in firebug console
    });
}


function loadJSON(callback) {
    var xobj = new XMLHttpRequest();
    xobj.overrideMimeType("application/json");
    xobj.open('GET', 'file.json', true);
    xobj.onreadystatechange = function () {
        if (xobj.readyState == 4 && xobj.status == "200") {

        // .open will NOT return a value but simply returns undefined in async mode so use a callback
        callback(xobj.responseText);

        }
    }
    xobj.send(null);
}

// // Call to function with anonymous callback
loadJSON(function(response) {
// Do Something with the response e.g.
//jsonresponse = JSON.parse(response);

// Assuming json data is wrapped in square brackets as Drew suggests
//console.log(jsonresponse[0].name);

});
