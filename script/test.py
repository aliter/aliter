"""
Name: Test NPC
Map:
  Name: prontera
  x: 156
  y: 188
  Direction: South
Sprite: 403
"""

try:
    self.variable += 1
except:
    self.variable = 0
    
def foo():
    say("You chose 'Foo!'")
    close()

def bar():
    say("You chose 'Bar!'")
    close()

say("Hi, how are you? %d" % self.variable)
next()

say("I'm doing great, thanks for asking!")
next()

markMap(1, 156, 188, 255)
markMap(2, 156, 156, 0, 255)
say("What's that? You're choosing something?")
next(foo)

menu({ "Foo": foo, "Bar": bar })
