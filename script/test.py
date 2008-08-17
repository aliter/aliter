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

try:
    self.variable += 1
except:
    self.variable = 0
    
def foo():
    say("You chose 'Foo!'")
    next()
    say("Hi")
    next(bar)
    say("You can't see me!")

def bar():
    say("You chose 'Bar!'")
    next()
    say("Hi")
    close()

say("Hi, how are YOU!? %d" % self.variable)
next()

say("I'm doing great, thanks for asking!")
menu(("Foo", foo), ("Bar", bar))
say("You can't see me.")
# next()

# from time import sleep
# for i in xrange(10):
#     say("Hello!")
#     sleep(1)

#close()

# close()
# 
# sleep(5)
# 
# say("HA! You thought I was gone, DIDN'T YOU!?")
# next()
# say("Well, uh, I am now. Bye~")
# close()

# markMap(1, 156, 188, 255)
# markMap(2, 156, 156, 0, 255)
# say("What's that? You're choosing something?")
# next(foo)
# 
# menu({ "Foo": foo, "Bar": bar })
