"""
Name: Test NPC
Map:
  Name: prontera
  x: 156
  y: 188
  Direction: South
Sprite: 403
"""

def foo():
    say("You chose 'Foo!'")
    close()

def bar():
    say("You chose 'Bar!'")
    close()

say("Hi, how are you?")
next()

say("I'm doing great, thanks for asking!")
next()

say("What's that? You're choosing something?")
next(foo)

menu({ "Foo": foo, "Bar": bar })
