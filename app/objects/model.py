import time

from app.utils import hashcompat

from struct import pack, unpack, calcsize
from threading import Timer


class Model(object):
    required = []
    optional = []
    
    def __init__(self, **kwargs):
        # Required attributes
        try:
            for key in self.required:
                exec 'self.%s = kwargs[key]' % key
        except AttributeError:
            raise InvalidAccount
        
        # Optional attributes
        for key, default in self.optional:
            if key in kwargs:
                exec 'self.%s = kwargs[key]' % key
            else:
                exec 'self.%s = default' % key
    
    def getForSave(self):
        data = {}
        for key in self.saveData:
            data[key] = getattr(self, key)
        return data
