import sha

from model import Model
from manager import Manager


class Account(Model):
    required = [
        'username', 'password', 'Email',
    ]
    optional = [
        ('id',         None),
        ('lastLogin',  None),
        ('gender',     0),
        ('loginCount', 0),
        ('gmLevel',    0),
        ('lastIP',     None),
        ('banUntil',   None),
    ]
    
    def hashPassword(self, password):
        return sha.new(password).hexdigest()
    
    def verifyPassword(self, password):
        if not self.password or not password:
            return False
        return self.password == self.hashPassword(password)

class AccountManager(Manager):
    modelClass = Account
    cacheDict  = {}
    table  = 'accounts'
    schema = [
        'id', 'username', 'password', 'Email', 'gender',
        'loginCount', 'lastLogin', 'lastIP', 'gmLevel', 'banUntil',
    ]
Accounts = AccountManager()
