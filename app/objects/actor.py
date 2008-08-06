from twisted.internet import reactor

from model import Model
from app import log

class Actor(Model):
    required = [
        'name',
    ]
    optional = [
        ('baseLevel', 1),
        ('baseExp',   0),
        ('jobExp',    0),
        ('str',   5),
        ('agi',   5),
        ('vit',   5),
        ('int',   5),
        ('dex',   5),
        ('luk',   5),
        ('maxHP', 40),
        ('hp',    40),
        ('map',   'new_zone01'),
        ('x',     53),
        ('y',     111),
    ]
    saveData = [
        'name', 'baseLevel', 'baseExp', 'jobExp', 'str',
        'agi', 'vit', 'int', 'dex', 'luk',
        'maxHP', 'hp', 'map', 'x', 'y',
    ]
    
    def __init__(self, **kwargs):
        super(Actor, self).__init__(**kwargs)
        
        self.direction = 0
        self.toX       = self.x
        self.toY       = self.y
        self.walkPath  = []
        self.walkPathOffset = 0
        self.speed     = 150
        self.moveDelay = self.speed / 1000.0
