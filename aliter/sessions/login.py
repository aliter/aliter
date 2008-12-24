import random
import sys
from socket import inet_aton

from aliter import log
from aliter.shared import config
from aliter.objects import Session, Accounts, Characters
from aliter.packets import receivedPackets, sendPacket
from aliter.inter import setLoginID


class LoginSession(Session):
    def __init__(self):
        Session.__init__(self, receivedPackets['login'], log.login)
    
    def login(self, packetVersion, username, password, region):
        account = Accounts.get(username=username)
        
        # We currently only support packet version 18 and newer
        if packetVersion < 18:
            self.log('Login from %s failed - Incorrect packet version (v%s).' % (self.transport.client[0], packetVersion), log.LOW)
            self.failed(0x05)
        elif not account:
            self.log('Login from %s failed - Account "%s" doesn\'t exist.' % (self.transport.client[0], username), log.LOW)
            self.failed(0x00) # Unregistered ID
        elif not account.verifyPassword(password):
            self.log('Login from %s failed - Incorrect password for "%s".' % (self.transport.client[0], username), log.LOW)
            self.failed(0x01) # Incorrect password
        else:
            self.id = username
            self.log('Login from %s successful, sending char server details.' % self.transport.client[0], log.LOW)
            
            loginIDa, loginIDb = random.randint(1, min(4294967295, sys.maxint)), random.randint(1, min(4294967295, sys.maxint))
            setLoginID(account.id, loginIDa, loginIDb)
            
            charServerPack = []
            for name, server in config['CharServer'].items():
                charServerPack.append({
                    'ip': inet_aton(server['address']['host']),
                    'port': server['address']['port'],
                    'name': name,
                    'maintenance': server['maintenance'],
                    'new': server['new'],
                })
            
            self.sendPacket(
                0x69,
                loginIDa=loginIDa,
                accountID=account.id,
                loginIDb=loginIDb,
                gender=account.gender,
                charServer=charServerPack,
            )
    
    def failed(self, type, message=''):
        self.sendPacket(
            0x6a,
            type=type,
            message=message
        )
