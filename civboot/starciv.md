# Starciv

It's a space game set in an asteroid belt. Humanity has destroyed this solar
system including it's planets and sun. There are only asteroids left. The
player starts with a civboot and some mining drones. They can build parts for
better ships and factories, as well as more civboots with upgraded parts.

One thing they can build is computers. Computers get n ops/sec, with an
exponential curve for cpu cost.

Players program on spor and fngi. The server code allows only 30 operations per
minute as API calls, including requests for map info. Players can have cpus
running in game, then the player (which is in the starting Civboot) can talk to
them through communication nodes.

Game communication happens through an API, which can be manual or client side
code (bot) of any language.

There is no collision. Instead, if a ship is in contact with 4 times it's mass
it's speed is cut in half. There are other ways to reduce ship speed. Ships
have acceleration and max velocity, it's not orbital mechanics.

There's mining, manufacturing, research, ship building, combat.

The end goal in the game is to be the first to build a faster than light
civboot. The research curves, along with the map size, are configurable to
either speed up or slow down the game.

