"""
Name: Test NPC
Map:
  Name: prontera
  x: 156
  y: 188
  Direction: South
Sprite: 403
"""

# from time import sleep

def page1():
    say("Hi, how are you?")
    next(page2)

def page2():
    say("I'm doing great, thanks for asking!")
    menu({ "Foo": foo, "Bar": bar })

def foo():
    say("You chose 'Foo!'")
    close()

def bar():
    say("You chose 'Bar!'")
    close()

page1()

# for i in xrange(9):
#     say(i + 1)
#     sleep(1)