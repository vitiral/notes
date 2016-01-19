
values = [1,2,3,4,5,6,7]
index = 0
even = []
while(index < values.length){
    value = values[index]
    print("processing", value)
    if(!(values[index] % 2)){
        even.push(value)
    }
    index += 1
}

print("Got:", even)
