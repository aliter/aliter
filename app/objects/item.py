from app.utils.hashcompat import sha_constructor

from model import Model
from manager import Manager


class Item(Model):
    required = [
        'id', 'name', 'cleanName'
    ]
    optional = [
        ('type', 0),
        ('priceBuy', 0),
        ('priceSell', None),
        ('weight', 0),
        ('attack', None),
        ('defence', None),
        ('range', None),
        ('slots', None),
        ('equipJobs', 4294967295),
        ('equipUpper', 7),
        ('equipGeners', 2),
        ('equipLocations', None),
        ('weaponLevel', None),
        ('equipLevel', None),
        ('refineable', None),
        ('view', None),
        ('script', None),
        ('equipScript', None),
        ('unequipScript', None)
    ]
    saveData = [
        'name', 'type', 'priceBuy', 'priceSell', 'weight', 'attack',
        'defence', 'range', 'slots', 'equipJobs', 'equipUpper',
        'equipGenders', 'equipLocations', 'weaponLevel', 'equipLevel',
        'refineable', 'view', 'script', 'equipScript', 'unequipScript'
    ]

class ItemManager(Manager):
    modelClass = Item
    cacheDict  = {}
    table  = 'items'
    schema = [
        'id', 'cleanName', 'name', 'type', 'priceBuy', 'priceSell', 'weight',
        'attack', 'defence', 'range', 'slots', 'equipJobs', 'equipUpper',
        'equipGenders', 'equipLocations', 'weaponLevel', 'equipLevel',
        'refineable', 'view', 'script', 'equipScript', 'unequipScript'
    ]

Items = ItemManager()
