
# cycle overview
cycle.js (or ts) is a ultra simple web "framework" that treats the user as a
"function" which accepts inputs/outputs and the web-renderer as another
"function" which only accepts inputs and outputs.

Everything in it is "reactive" as well as purely functional. This makes it
fundamentally simple and testable compared to other models that exist.

there are three main components: main(), drivers and run()

main is structured like:
```
function main(sources) {
    ... do stuff with sources
    return sinks;
}
```

sources and sinks are both asynchronous generators.

Drivers are simply objects that take your sinks and render them to the user,
then accept input and convert them into sources for main.

run ties these two together into a loop (or a cycle).

