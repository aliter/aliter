from model import Model
from manager import Manager
from character import Characters


class Mercenary(Model):
    required = [
        'characterID'
    ]
    optional = [
        ('id', None),
        ('class', 0),
        ('hp', 1),
        ('sp', 1),
        ('killCounter', 0),
        ('timeRemaining', 0)
    ]
    saveData = [
        'id', 'characterID', 'class', 'hp', 'sp', 'killCounter', 'timeRemaining'
    ]
    
    def master(self):
        """
        Returns the mercenary's master.
        """
        return Characters.get(self.characterID)
    

class MercenaryManager(Manager):
    modelClass = Mercenary
    cacheDict  = {}
    table  = 'mercenaries'
    schema = [
        'id', 'characterID', 'class', 'hp', 'sp', 'killCounter', 'timeRemaining'
    ]
    
Mercenaries = MercenaryManager()