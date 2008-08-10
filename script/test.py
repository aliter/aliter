"""
Name: Test NPC
Map:
  Name: prontera
  x: 156
  y: 188
  Direction: South
Sprite: 403
"""

from time import sleep

def foo():
    say("You chose 'Foo!'")
    close()

def bar():
    say("You chose 'Bar!'")
    close()

# for i in xrange(9):
#     say(i + 1)
#     sleep(1)

say("Hi, how are you?")

next()

say("I'm doing great, thanks for asking!")

menu({ "Foo": foo, "Bar": bar })