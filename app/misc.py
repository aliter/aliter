import re, os, socket, string, sys, traceback, csv

from twisted.internet import reactor
from twisted.internet.error import CannotListenError

def chunk(input, size):
    "Chunk a list"
    output  = []
    current = []
    for cell in input:
        current.append(cell)
        if len(current) == size:
            output.append(current)
            current = []
    output.append(current)
    return output

def stringHex(string):
    "Make hexadecimal human-readable digits"
    # String representation
    list = []
    for char in string:
        high = hex(ord(char)).replace('0x', '')
        if len(high) == 1:
            high = '0' + high
        list.append(high)
    # Make human-readable and return
    nibbles = [' '.join(row) for row in chunk(list, 4)]
    newline = False
    for position in range(len(nibbles)-1, 0, -1):
        nibbles.insert(position, (' - ', '\n')[newline])
        newline = not newline
    return ''.join(nibbles)

def fixTermination(*args):
    "Remove excess C-style string terminations"
    out = []
    for arg in args:
        if type(arg) == str:
            index = arg.find('\0')
            if index > -1:
                out.append(arg[:arg.index('\0')])
            else:
                out.append(arg)
        else:
            out.append(arg)
    if len(out) == 1:
        return out[0]
    return out

tick = 0
def getTick():
    "Temporary server tick count"
    global tick
    tick += 1
    return tick

def _expandStruct(match):
    if match.group(2) == 's':
        return match.group(1) + match.group(2)
    return match.group(2) * int(match.group(1))
def expandStruct(string):
    "Convert all number repetitions such as '4c' into their expanded form ('cccc')"
    return re.sub('(\d+)(\w)', _expandStruct, string)

def addContext(variable, params):
    "Replace all script .variables with their appropriate value from params"
    if type(variable) == str:
        scriptVariable = re.compile('\.[a-zA-Z0-9_]+')
        while 1:
            match = scriptVariable.search(variable)
            if not match:
                break
            if match.group(0)[1:] not in params:
                params[match.group(0)[1:]] = 0
            param    = params[match.group(0)[1:]]
            variable = variable.replace(match.group(0), str(param))
            #print '>>> '+match.group(0)+' = '+str(param)
    #print variable
    return variable

def splitCommand(line, params=None):
    # Make sure there are even quotation marks
    if line.count('"') % 2:
        line += '"'
    
    words = csv.reader([line], delimiter=' ', escapechar='\\').next()
    words = [element for element in words if element != '']
    return words

def ttysize():
    "Determine the size of the user's TTY session"
    fp = os.popen('stty -a', 'r')
    ln1 = fp.readline()
    fp.close()
    if not ln1:
        raise ValueError, 'tty size not supported for input'
    vals = {'rows':None, 'columns':None}
    for ph in string.split(ln1, ';'):
        x = string.split(ph)
        if len(x) == 2:
            vals[x[0]] = x[1]
            vals[x[1]] = x[0]
    return vals['rows'], vals['columns']
