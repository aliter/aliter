import sys, traceback, re
from struct import pack, unpack, calcsize

from twisted.internet import protocol

from app import log, packets
from app.exceptions import IllegalPacket
from app.packets import receivedPackets, sendPacket
from app.misc import expandStruct, stringHex, fixTermination

class Server(protocol.ServerFactory):
    def __init__(self, protocol, logMethod, **kwargs):
        self.protocol, self.log = protocol, logMethod
        self.log('Server starting up...')
    
    # TODO: Work out where this is really suppose to go
    def shutdown(self, port):
        self.log('Server shutting down...', log.HIGH)
        port.stopListening()
        del port

class Session(protocol.Protocol):
    id      = None
    packets = {}
    
    def __init__(self, packets, logMethod):
        self.logMethod = logMethod
        
        # Expand packets for optimization
        self.packets = {}
        for key, origPacket in packets.iteritems():
            packet = []
            if not len(origPacket):
                # Ignore this packet if no method defined
                self.packets[key] = [None, '=h', 2]
                continue
            if origPacket[0]:
                packet.append(getattr(self, origPacket[0], None)) # Convert string to function 
            else:
                packet.append(None)
            if len(origPacket) > 1:
                packet.append('=h'+origPacket[1]) # Finish struct
                
                # Find packet length
                if len(origPacket) > 2:
                    if 'packetLen' in origPacket[2:]:
                        # If we have a "packetLen" argument then the packet doesn't have a set size
                        packet.append(None)
                        packetLenOffset = origPacket[2:].index('packetLen')
                    else:
                        packet.append(calcsize(packet[1]))
                else:
                    packet.append(calcsize(packet[1]))
            else:
                packet.append('=h')
                packet.append(2)
            packetLenOffset = None
            if len(origPacket) > 2: # Move all arguments into a child list.
                packet.append(origPacket[2:])
            else:
                packet.append([])
            packet.append(len(packet[3])) # Number of arguments
            
            # Determine which arguments require fixTermination()
            strings   = []
            packet[1] = expandStruct(packet[1])
            decoded   = re.sub('\d+', '0', packet[1])[2:]
            offset    = 0
            index     = decoded.find('0')
            while index > -1:
                offset -= decoded[:index].count('x')
                strings.append(packet[3][index+offset])
                offset += index
                decoded = decoded[index+1:]
                index   = decoded.find('0')
            packet.append(strings)
            
            # Flexi-length packets
            packet.append(packetLenOffset)
            
            self.packets[key] = packet
    
    def dataReceived(self, payload):
        # Split payload into smaller packets if needed
        # TODO: If multiple packets show up it most likely means buffer wasn't cleared
        #       so the packets in the old buffer will be repeated.
        packets = []
        while len(payload):
            packetID = eval('0x'+stringHex(payload[1:2])+stringHex(payload[0:1]))
            if packetID in self.packets:
                packetStruct = self.packets[packetID][1]
                if self.packets[packetID][2]:
                    packetPayload = payload[:self.packets[packetID][2]]
                else: # Flexi-length
                    length = eval('0x'+stringHex(payload[3:4])+stringHex(payload[2:3]))
                    packetPayload = payload[:length]
                    headerStruct = packetStruct.replace('!', '')
                    flexiLen = len(packetPayload) - calcsize(headerStruct)
                    packetStruct = packetStruct.replace('!', '%ss' % flexiLen)
            else:
                packetStruct  = None
                packetPayload = payload
            packets.append((packetID, packetStruct, packetPayload))
            payload = payload[len(packetPayload):]
        
        # Loop through the packets
        for thisPacket in packets:
            try:
                if thisPacket[0] in self.packets:
                    packet    = self.packets[thisPacket[0]]
                    if not packet[0]:
                        continue # No method defined, skip
                    arguments = {}.fromkeys(packet[3])
                    # print thisPacket
                    # print unpack(thisPacket[1], thisPacket[2])[1:]
                    values    = unpack(thisPacket[1], thisPacket[2])[1:]
                    for index in range(packet[4]):
                        arguments[packet[3][index]] = values[index]
                        if packet[3][index] in packet[5] and packet[3][index] != 'position':
                            arguments[packet[3][index]] = fixTermination(arguments[packet[3][index]])
                    if 'packetLen' in arguments:
                        del arguments['packetLen']
                    packet[0](**arguments)
                else:
                    if 'packet' in self.__class__.__dict__:
                        eval('self.packet(*thisPacket)')
                    else:
                        self.log('Received unhandled packet:', log.HIGH)
                        self.log(stringHex(thisPacket[2]), log.HIGH)
            except IllegalPacket:
                self.log('Illegal packet received.', log.HIGH)
                #break
            except:
                self.log('Exception raised <%s>: %s' % (sys.exc_info()[0], sys.exc_info()[1]), log.CRITICAL)
                for line in traceback.format_tb(sys.exc_info()[2]):
                    self.log(line[:-1], log.CRITICAL)
                #break
    
    def log(self, message, priority=log.NORMAL, **kwargs):
        kwargs['id'] = self.id
        self.logMethod(message, priority, **kwargs)
    
    def sendRaw(self, data):
        return self.transport.write(data)
    
    def sendPacket(self, packetID, **kwargs):
        return packets.sendPacket(self.transport.write, packetID, **kwargs)
