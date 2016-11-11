
text = 'hello there bob-dole'
print(text.indexOf('h'))
print(text.indexOf('-'))
print(text.slice(0, text.indexOf('-')))

hi = 'hello'
print(hi.indexOf('-'))
print(function (){
    index = hi.indexOf('-')
    if (index == -1){
        return hi
    } else {
        return hi.slice(0, index)
    }
}())

print("version.string".replace(".", "-"))
