from datetime import datetime

from constants import *

# Priority status
DEBUG, LOW, NORMAL, HIGH, CRITICAL = 10, 20, 30, 40, 50

def _getColor(priority):
    color = ANSI_DEFAULT
    if priority == CRITICAL:
        color = ANSI_LIGHT_RED
    if priority == HIGH:
        color = ANSI_YELLOW
    return color

def console(message, priority=NORMAL, prefix=None, color=None):
    if not color:
        color = _getColor(priority)
    for line in message.rsplit('\n'):
        if line:
            output = [ANSI_WHITE, datetime.now().time().strftime('%H:%M:%S ')]
            if prefix:
                output.append(prefix)
            output.extend([color, line, ANSI_DEFAULT])
            print ''.join(output)
        else:
            print ''

def unbound(**kwargs):
    console(**kwargs)

def bound(message, name, prefixColor, id=None, **kwargs):
    prefix = [prefixColor, name]
    if id:
        prefix.append(('<%s>' % id, '')[id == None])
    prefix.append(': ')
    console(message, prefix=''.join(prefix), **kwargs)

def login(message, priority=NORMAL, id=None, **kwargs):
    bound(message, 'Login', ANSI_PURPLE, id, priority=priority, **kwargs)

def char(message, priority=NORMAL, id=None, **kwargs):
    bound(message, 'Char', ANSI_LIGHT_BLUE, id, priority=priority, **kwargs)

def map(message, priority=NORMAL, id=None, **kwargs):
    bound(message, 'Map', ANSI_LIGHT_CYAN, id, priority=priority, **kwargs)
