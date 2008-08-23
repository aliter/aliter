"""
ID: Center
Name: Kafra Employee
Map:
  Name: prontera
  x: 146
  y: 89
  Direction: South
Sprite: 117

Duplicates:
  -
    ID: Center
    Map:
      Name: prontera
      x: 152
      y: 188
      Direction: South
    Sprite: 117
  
  -
    ID: North
    Map:
      Name: prontera
      x: 152
      y: 326
      Direction: East
    Sprite: 112
  
  -
    ID: South
    Map:
      Name: prontera
      x: 151
      y: 29
      Direction: North
    Sprite: 115
  
  -
    ID: East
    Map:
      Name: prontera
      x: 282
      y: 200
      Direction: Northeast
    Sprite: 114
  -
    ID: West
    Map:
      Name: prontera
      x: 29
      y: 207
      Direction: South
    Sprite: 113
  
"""

from script.kafras.base import KafraBase

class PronteraKafras(KafraBase):
    messages = {
        "North": ("[Kafra Employee]",
                  "Welcome to the Kafra Corporation~",
                  "The Kafra Services are always here to support you.",
                  "So how can I be of service today?"),
        "South": ("[Kafra Employee]",
                  "Welcome~!",
                  "The Kafra Services are always on your side.",
                  "So how can I help you?"),
        "East": ("[Kafra Employee]",
                 "Welcome!",
                 "The Kafta Corporation will always support the adventurers of Rune-Midgard with its excellent service.",
                 "So what can I do for you today?"),
        "West": ("[Kafra Employee]",
                 "The Kafra Corporation is always working to provide you with convenient services.",
                 "How may I be of assistance?"),
        "Center": ("[Kafra Employee]",
                   "Welcome to the Kafra Corporation.",
                   "The Kafra services are always on your side.",
                   "How may I assist you?")
    }
    
    cutins = { "North": "kafra_06",
               "South": "kafra_03",
               "East": "kafra_04",
               "West": "kafra_05",
               "Center": "kafra_01" }
    
    prices = {
        "Izlude": 600,
        "Geffen": 1200,
        "Payon": 1200,
        "Morroc": 1200,
        "Orc Dungeon": 1200,
        "Alberta": 1800
    }
    
    markers = [(151, 29), (29, 207), (282, 200), (152, 326)]
    
    def __init__(self, id):
        """Begins the NPC."""
        self.id = id
        
        cutin(self.cutins[id])
        say(*self.messages[id])
        menu(*self.menus[0])
    
    def save(self):
        if self.id == "North":
            actor.save("prontera", 157, 327)
        elif self.id == "South":
            actor.save("prontera", 150, 33)
        elif self.id == "East":
            actor.save("prontera", 281, 203)
        elif self.id == "West":
            actor.save("prontera", 33, 208)
        elif self.id == "Center":
            actor.save("prontera", 116, 73)
    
        self.saved("in the city of Prontera")

PronteraKafras(self.id)
"""
// North ==================================
prontera,152,326,3	script	Kafra Employee::kaf_prontera	112,{
	callfunc "F_Kafra",5,0,0,40,800;
}
// SOUTH ======================================
prontera,151,29,0	script	Kafra Employee::kaf_prontera2	115,{
	callfunc "F_Kafra",5,0,0,40,800;
}

//WEST=========================================
prontera,29,207,6	script	Kafra Employee::kaf_prontera3	113,{
	callfunc "F_Kafra",5,0,0,40,800;
}

// East ===========================================
prontera,282,200,2	script	Kafra Employee::kaf_prontera4	114,{
	callfunc "F_Kafra",5,0,0,40,800;
}

// Center ===========================================
prontera,146,89,6	script	Kafra Employee::kaf_prontera5	117,{
	callfunc "F_Kafra",5,0,0,40,800;
}
"""