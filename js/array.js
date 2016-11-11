// Find the maximum value in a list of numbers
oldray = ["hi", "hello", "what"]
newray = ["new", "new2"]

print("oldray:", oldray)
newray.forEach(function(v) {oldray.push(v)}, this)

print("Extended:", oldray)
print("one more:", oldray.push('added'))

function shuffleArray(array) {
    for (var i = array.length - 1; i > 0; i--) {
        var j = Math.floor(Math.random() * (i + 1));
        var temp = array[i];
        array[i] = array[j];
        array[j] = temp;
    }
    return array;
}

print(shuffleArray(oldray))

print(oldray.filter(function(n) {return n == "hello"})[0])
