import re, random
import sys # Testing stuff

from exceptions import ScriptError
from shared import maps
from objects import NPC, Warp
from misc import splitCommand, addContext

class ScriptObject(object):
    files     = {}
    nextNPCID = 5000000
    npcs      = {}
    tempVar   = 0
    
    subcommandPattern = re.compile(r'\{\s*([^{}]*)\s*\}')
    varAssignPattern  = re.compile(r'^\.(\w+)\s*(?<!=)=(?!=)\s*(.*)')
    
    def _makeCommand(self, line):
        # TODO: Optimize .count()'s
        if line[0].isdigit() or line[0] in ('.', "'", '"') or line[:4] == 'not ' \
        or line.count('==') or line.count('!=') or line.count('>=') or line.count('<=') \
        or line.count('>') or line.count('<'): # These two mean you can't have complex subcommands
            return '_eval '+line
        return line
    
    # Loads a script into memory and compiles it
    #    Line format: [fileLine, indent, line, command]
    # Command format: [firstWord, wordsNoFirst, stringNoFirst]
    def load(self, filename):
        self.labels   = {}
        self.commands = {}
        self.commandsOffset = 0
        
        file      = open('script/%s.txt' % filename)
        fileLines = file.readlines()
        file.close()
        
        #----------------------------------------------
        # Count indents and standardize all lines
        
        indents = [0]
        lineNum = 0
        lines   = []
        for fileLine in fileLines:
            lineNum += 1
            fileLine = fileLine.rstrip()
            fileLine = fileLine.expandtabs(4)
            line     = [(filename, lineNum), None, fileLine.lstrip(), None]
            if not len(line[2]) or line[2][0] == '#':
                continue
            else:
                indent = len(fileLine) - len(line[2])
                if indent > indents[-1]:
                    indents.append(indent)
                elif indent < indents[-1]:
                    while indent < indents[-1]:
                        indents.pop()
                line[1] = len(indents)-1
            lines.append(line)
        
        # Always have a command at the end of the file
        lines.append([lines[-1][0], 0, 'pass', None])
        
        #----------------------------------------------
        # Split expressions (Variable assignment)
        
        offset = 0
        for line in lines:
            while 1:
                match = self.varAssignPattern.search(line[2])
                if not match:
                    break
                #print line, match.groups()
                #break
                line[2] = '_set '+match.group(1)
                lines.insert(offset, [line[0], line[1], self._makeCommand(match.group(2)), None])
                #self.tempVar += 1
            offset += 1
        
        #----------------------------------------------
        # Split expressions (Statements)
        # TODO: Clean up
        
        offset = 0
        skip   = 0
        for line in lines:
            if skip:
                skip -= 1
                continue
            word = line[2].split()[0]
            if word in ('if', 'elif', 'while', 'switch'):
                expression  = line[2][len(word)+1:].lstrip()
                line[2]     = word
                whileOffset = 0
                if word == 'while':
                    skip += 1
                    lines.insert(offset, [line[0], line[1], '_markWhile', None])
                    whileOffset = 1
                skip += 1
                lines.insert(offset+whileOffset, [line[0], line[1], self._makeCommand(expression), None])
                if word == 'elif':
                    skip   += 1
                    lines.insert(offset+whileOffset, [line[0], line[1], '_markElif', None])
                    offset += 1
                if word == 'while':
                    offset += 1
                offset += 1
            offset += 1
        
        #----------------------------------------------
        # Split expressions (Subcommands)
        # TODO: Make recursive? Having a subcommand in a var assignment doesn't work (May not even be this section's problem)
        
        offset = 0
        for line in lines:
            while 1:
                match = self.subcommandPattern.search(line[2])
                if not match:
                    break
                line[2] = line[2].replace(match.group(0), '._temp%s' % self.tempVar)
                lines.insert(offset, [line[0], line[1], '_set _temp%s' % self.tempVar, None])
                lines.insert(offset, [line[0], line[1], self._makeCommand(match.group(1)), None])
                self.tempVar += 1
            offset += 1
        
        #----------------------------------------------
        # Parse statements
        
        offset = 0
        for line in lines:
            #print line[2]
            words = splitCommand(line[2])
            
            if words[0] == 'case':
                # Must have at least one child
                if lines[offset+1][1] <= line[1]:
                    lines.insert(offset+1, [line[0], line[1]+1, 'pass', None])
            
            elif words[0] in ('say', 'message'):
                # Move all children into params
                lastLineNum = lines[offset+1][0][1]
                if lines[offset+1][1] <= line[1]:
                    raise scriptError('Speech is required for this command')
                if len(words) > 1:
                    words = [words[0], words[1]]
                else:
                    words = [words[0], None]
                while lines[offset+1][1] > line[1]:
                    if lines[offset+1][0][1] - lastLineNum > 1:
                        # Double new-line = new message
                        lines.insert(offset+1, [lines[offset+1][0], line[1], line[2], None])
                        lines.insert(offset+1, [lines[offset+1][0], line[1], 'next', None])
                        break
                    else:
                        lastLineNum = lines[offset+1][0][1]
                        words.append(lines[offset+1][2])
                        del lines[offset+1]
            
            elif words[0] == 'markMap':
                # Force format: [id, posX, posY, red, green, blue]
                dict = {}
                for word in words[1:]:
                    if word[0] == '<':
                        dict['color'] = word[1:-1].split(',')
                        if len(dict['color']) < 3:
                            raise ScriptError('Color element requires three values')
                    elif word[0] == '[':
                        dict['position'] = word[1:-1].split(',')
                        if len(dict['position']) < 2:
                            raise ScriptError('Position element requires two values')
                    else:
                        dict['id'] = word
                
                if 'id' not in dict \
                or 'color' not in dict \
                or 'position' not in dict :
                    raise ScriptError('Invalid map marking definition')
                
                words = [
                    'markMap',
                    dict['id'], dict['position'][0], dict['position'][1],
                    dict['color'][0], dict['color'][1], dict['color'][2],
                ]
                
            elif words[0] == 'npc':
                # Force format: [nameID, name, posMap, posX, posY, posDir, sprite]
                dict = {}
                for word in words[1:]:
                    if word[0] == '<':
                        dict['nameID'] = word[1:-1]
                    elif word[0] == '[':
                        dict['position'] = word[1:-1].split(',')
                        if len(dict['position']) < 4:
                            raise ScriptError('Position element requires four values')
                    else:
                        if 'name' not in dict:
                            dict['name'] = word
                        else:
                            dict['sprite'] = word
                
                if 'position' not in dict \
                or 'sprite' not in dict \
                or 'name' not in dict :
                    raise ScriptError('Invalid NPC definition')
                
                # Optional parameters
                if 'nameID' not in dict:
                    dict['nameID'] = None
                
                words = [
                    'npc',
                    dict['nameID'], dict['name'], dict['position'][0], dict['position'][1],
                    dict['position'][2], dict['position'][3], dict['sprite'],
                ]
            
            elif words[0] == 'duplicate':
                # Force format: [nameID, name, posMap, posX, posY, posDir, sprite, parentNameID]
                dict   = {}
                isName = True
                for word in words[1:]:
                    if word[0] == '<':
                        key = 'parent'
                        if 'parent' in dict:
                            key = 'nameID'
                        dict[key] = word[1:-1]
                    elif word[0] == '[':
                        isName = False # Name must be defined before position
                        dict['position'] = word[1:-1].split(',')
                        if len(dict['position']) < 4:
                            raise ScriptError('Position element requires four values')
                    else:
                        if isName and 'name' not in dict:
                            dict['name'] = word
                        else:
                            dict['sprite'] = word
                
                if 'parent' not in dict \
                or 'position' not in dict:
                    raise ScriptError('Invalid NPC duplication definition')
                
                # Optional parameters
                if 'nameID' not in dict:
                    dict['nameID'] = None
                
                # These are fetched at execution
                if 'name' not in dict:
                    dict['name'] = None
                if 'sprite' not in dict:
                    dict['sprite'] = None
                
                words = [
                    'duplicate',
                    dict['nameID'], dict['name'], dict['position'][0], dict['position'][1],
                    dict['position'][2], dict['position'][3], dict['sprite'], dict['parent'],
                ]
            
            elif words[0] == 'warp':
                # Force format: [nameID, posMap, posX, posY, toMap, toX, toY, spanX, spanY]
                dict = {}
                for word in words[1:]:
                    if word[0] == '<':
                        dict['nameID'] = word[1:-1]
                    elif word[0] == '[':
                        key = 'position'
                        if 'position' in dict:
                            key = 'to'
                        dict[key] = word[1:-1].split(',')
                        if len(dict[key]) < 3:
                            raise ScriptError('Position element requires three values')
                    else:
                        if 'spanX' not in dict:
                            dict['spanX'] = word
                        else:
                            dict['spanY'] = word
                
                if 'position' not in dict \
                or 'to' not in dict:
                    raise ScriptError('Invalid warp definition')
                
                # Optional parameters
                if 'nameID' not in dict:
                    dict['nameID'] = None
                if 'spanX' not in dict:
                    dict['spanX'] = 1
                if 'spanY' not in dict:
                    dict['spanY'] = 1
                
                words = [
                    'warp',
                    dict['nameID'], dict['position'][0], dict['position'][1], dict['position'][2],
                    dict['to'][0], dict['to'][1], dict['to'][2], dict['spanX'], dict['spanY'],
                ]
            
            line[3] = [words[0], words[1:], line[2][len(words[0])+1:]]
            offset += 1
        
        #----------------------------------------------
        # Store script
        
        #for line in lines:
        #    print line
        #sys.exit(1)
        
        self.files[filename] = lines
        
        #----------------------------------------------
        # Execute level 0
        
        pointer = {
            'file': filename,
            'offset': 0,
            'register': None,
            'vars': {},
            'extra': {},
            'halt': False,
        }
        while 1:
            pointer = self.execute(pointer)
            if pointer['halt'] or pointer['offset'] < 0:
                break
        
        print ''
    
    def _didWeEscape(self, curIndent, pointer, wasBreak=False):
        # Did we just escape an indent?
        curIndent += 1
        if pointer['offset'] == -1:
            nextIndent = 0
        else:
            nextIndent = self.files[pointer['file']][pointer['offset']][1]
        if wasBreak:
            loopTo = 1000 # Keep going and going until we reach a loop or level 0
        else:
            loopTo = curIndent-nextIndent-1
        #print curIndent, nextIndent
        for diff in range(0, loopTo):
            if not len(pointer['extra']['indentStack']):
                # Escaped higher than where we entered, end script
                pointer['offset'] = -1
                break
            curIndent -= 1
            type       = pointer['extra']['indentStack'].pop()
            #print 'Escaping with type "%s"' % type
            
            if type == 'while':
                while 1:
                    if pointer['offset'] < 0:
                        # We hit SOF
                        # This should never be called
                        # TODO: Verify claim and remove
                        raise ScriptError('While escaped itself! Reached SOF')
                        #break
                    command = self.files[pointer['file']][pointer['offset']-1]
                    #print 'w>>>>>>',command[3]
                    if command[1] == curIndent - 1 and command[3][0] == '_markWhile':
                        # pointer['offset'] += 1
                        break # Found it
                    if command[1] < curIndent - 1:
                        # This should never be called
                        # I think it gets called when a command that isn't suppose to have children does
                        # TODO: Verify claim and remove
                        raise ScriptError('While escaped itself!')
                        #return self._didWeEscape(curIndent-1, pointer)
                    pointer['offset'] -= 1
                if wasBreak:
                    # Find the while statement (Currently sitting at marker)
                    while 1:
                        if self.files[pointer['file']][pointer['offset']-1][3][0] == 'while':
                            break
                        pointer['offset'] += 1
                    pointer = self._doJump('sameIndent', pointer)
                break # Don't continue after a looping command
            
            elif type == 'if':
                if nextIndent < curIndent - 1:
                    #return self._didWeEscape(curIndent-1, pointer)
                    continue # No need to check for elses
                mark = False
                while 1:
                    #if pointer['offset'] >= len(self.files[pointer['file']]):
                    #    # We hit EOF (Should never be called)
                    #    pointer['offset'] = -1
                    #    return pointer
                    command = self.files[pointer['file']][pointer['offset']]
                    #print 'i>>>>>>',command[3]
                    if command[1] < curIndent - 1:
                        # We skipped an "else" and after it is a higher escape
                        return self._didWeEscape(curIndent-1, pointer, wasBreak)
                    elif command[1] == curIndent - 1:
                        if command[3][0] == '_markElif':
                            mark = True
                        elif command[3][0] == 'elif':
                            mark = False
                        elif command[3][0] != 'else' and not mark:
                            break
                    pointer['offset'] += 1
            
            elif type == 'switch':
                escIndent = curIndent - (wasBreak and 2 or 1)
                while 1:
                    if pointer['offset'] < 0:
                        # We hit SOF
                        # This should never be called
                        # TODO: Verify claim and remove
                        raise ScriptError('Switch escaped itself! Reached SOF')
                        #break
                    command = self.files[pointer['file']][pointer['offset']-1]
                    #print 's>>>>>>',command, curIndent
                    if command[1] == escIndent and command[3][0] == 'switch':
                        # pointer['offset'] += 1
                        break # Found it
                    if command[1] < escIndent:
                        # This should never be called
                        # I think it gets called when a command that isn't suppose to have children does
                        # TODO: Verify claim and remove
                        raise ScriptError('Switch escaped itself!')
                        #return self._didWeEscape(curIndent-1, pointer)
                    pointer['offset'] -= 1
                pointer = self._doJump('sameIndent', pointer)
                break # Don't continue after a switch command
            
        return pointer
    
    def _doJump(self, type, pointer, params=None):
        "Find next command that isn't the next one"
        #print 'Jump:', type,
        jumpIndent = self.files[pointer['file']][pointer['offset']-1][1]
        
        if type == 'sameIndent':
            # Always jump to the next command with same indent
            jumpIndent = self.files[pointer['file']][pointer['offset']-1][1]
            #print jumpIndent,
            while self.files[pointer['file']][pointer['offset']][1] > jumpIndent:
                # TODO: Deal with escaping while jumping (Done?)
                pointer['offset'] += 1
                if pointer['offset'] >= len(self.files[pointer['file']]):
                    # We hit EOF
                    pointer['offset'] = -1
                    break
        
        if type == 'switch':
            # Keep jumping until the correct "case" is found or end of switch
            # TODO: Deal with escaping while jumping
            while self.files[pointer['file']][pointer['offset']][1] > jumpIndent:
                command = self.files[pointer['file']][pointer['offset']]
                #print '===',command[3], params
                if command[1] == jumpIndent+1 and command[3][0] == 'case':
                    if eval(addContext(command[3][1][0], pointer['vars'])) == params:
                        break # Found it
                pointer['offset'] += 1
                if pointer['offset'] >= len(self.files[pointer['file']]):
                    # We hit EOF
                    pointer['offset'] = -1
                    break
        #sys.exit(1)
        
        # Have we hit EOF?
        #if pointer['offset'] >= len(self.files[pointer['file']]):
        #    pointer['offset'] = -1
        #    return pointer
        
        #print self.files[pointer['file']][pointer['offset']][1]
        # Did we just escape an indent?
        pointer = self._didWeEscape(jumpIndent, pointer)
        
        return pointer # Just return the next line for now
    
    def execute(self, pointer):
        # Verify pointer
        if pointer['file'] not in self.files \
        or pointer['offset'] >= len(self.files[pointer['file']]):
            raise ScriptError('Invalid parameters')
        
        # Set up local variables
        line    = self.files[pointer['file']][pointer['offset']]
        command = line[3]
        pointer['halt'] = False
        if 'indentStack' not in pointer['extra']:
            pointer['extra']['indentStack'] = []
        
        # Increment pointer
        pointer['offset'] += 1
        if pointer['offset'] == len(self.files[pointer['file']]):
            pointer['offset'] = -1
        
        # Did we just escape an indent?
        pointer = self._didWeEscape(line[1], pointer)
        
        # Debug
        print ('    ' * line[1])+str(command)
        #sys.exit(1)
        
        # Add context to parameters
        params = []
        for param in command[1]:
            params.append(addContext(param, pointer['vars']))
        
        #----------------------------------------------
        # Control structure commands
        
        if command[0] in ('pass', '_markElif'):
            pass
        
        elif command[0] == '_eval':
            pointer['register'] = eval(addContext(command[2], pointer['vars']))
        
        elif command[0] == '_set':
            pointer['vars'][params[0]] = pointer['register']
            #pointer['register'] = None
        
        elif command[0] in ('if', 'elif'):
            #print pointer['register']
            if pointer['register']:
                pointer['extra']['indentStack'].append('if')
                #print '[[[ Added "if" to indent stack ]]]'
            else:
                pointer = self._doJump('sameIndent', pointer)
        
        elif command[0] == 'else':
            pointer['extra']['indentStack'].append('if') # TODO: Should it append "if" or "else"?
            #print self.files[pointer['file']][pointer['offset']]
            #sys.exit(1)
        
        elif command[0] == 'while':
            if pointer['register']:
                pointer['extra']['indentStack'].append('while')
                #print '[[[ Added "while" to indent stack ]]]'
            else:
                pointer = self._doJump('sameIndent', pointer)
        
        elif command[0] == 'break':
            pointer = self._didWeEscape(line[1], pointer, True)
        
        elif command[0] == 'switch':
            pointer['extra']['indentStack'].append('switch')
            pointer = self._doJump('switch', pointer, pointer['register'])
        
        elif command[0] == 'case':
            pointer['extra']['indentStack'].append('case')
        
        #----------------------------------------------
        # Mathematical commands
        
        elif command[0] == 'random':
            if len(params) > 1:
                pointer['register'] = random.randint(int(params[0]), int(params[1]))
            else:
                pointer['register'] = random.randint(0, int(params[0])-1)
        
        #----------------------------------------------
        # Character-related commands
        
        elif command[0] == 'name':
            pointer['extra']['sayName'] = addContext(params[0], pointer['vars'])
        
        elif command[0] in ('say', 'message'):
            if 'character' not in pointer['vars']:
                raise ScriptError('This command cannot be called here')
            if command[0] == 'say':
                pointer['vars']['character'].session.sendPacket(
                    0xb4,
                    actorID=pointer['extra']['npc'].id,
                    message='[%s]' % (params[0] or pointer['extra']['sayName']),
                )
            for message in params[1:]:
                pointer['vars']['character'].session.sendPacket(
                    0xb4,
                    actorID=pointer['extra']['npc'].id,
                    message=message,
                )
        
        elif command[0] == 'next':
            if 'character' not in pointer['vars']:
                raise ScriptError('This command cannot be called here')
            pointer['vars']['character'].session.sendPacket(
                0xb5,
                actorID=pointer['extra']['npc'].id,
            )
            pointer['register'] = '_next'
            pointer['halt']     = True
        
        elif command[0] in ('close', 'close2'):
            if 'character' not in pointer['vars']:
                raise ScriptError('This command cannot be called here')
            pointer['vars']['character'].session.sendPacket(
                0xb6,
                actorID=pointer['extra']['npc'].id,
            )
            pointer['register'] = '_'+command[0]
            pointer['halt']     = True
        
        elif command[0] in ('select', 'prompt'):
            if 'character' not in pointer['vars']:
                raise ScriptError('This command cannot be called here')
            pointer['vars']['character'].session.sendPacket(
                0xb7,
                actorID=pointer['extra']['npc'].id,
                items=':'.join(params),
            )
            pointer['register'] = '_'+command[0]
            pointer['halt']     = True
        
        elif command[0] == 'markMap':
            if 'character' not in pointer['vars']:
                raise ScriptError('This command cannot be called here')
            pointer['vars']['character'].session.sendPacket(
                0x144,
                actorID=pointer['extra']['npc'].id,
                type=1,
                pointID=int(params[0]),
                x=int(params[1]),
                y=int(params[2]),
                red=int(params[3]),
                green=int(params[4]),
                blue=int(params[4]),
            )
        
        elif command[0] == 'removeMark':
            if 'character' not in pointer['vars']:
                raise ScriptError('This command cannot be called here')
            pointer['vars']['character'].session.sendPacket(
                0x144,
                actorID=pointer['extra']['npc'].id,
                type=2,
                pointID=int(params[0]),
                x=0,
                y=0,
                red=0,
                green=0,
                blue=0,
            )
        
        elif command[0] == 'cutin':
            if 'character' not in pointer['vars']:
                raise ScriptError('This command cannot be called here')
            cutin = params[0]
            pointer['vars']['character'].session.sendPacket(
                0x1b3,
                filename=cutin,
                position=int(params[1]),
            )
            pointer['extra']['cutin'] = cutin
        
        elif command[0] == 'hideCutin':
            if 'character' not in pointer['vars']:
                raise ScriptError('This command cannot be called here')
            pointer['vars']['character'].session.sendPacket(
                0x1b3,
                filename='',
                position=255,
            )
            del pointer['extra']['cutin'] # TODO: Check to make sure cutin actually is defined
        
        #----------------------------------------------
        # Spawning commands
        
        elif command[0] == 'npc':
            if params[2] not in maps:
                raise ScriptError('Required map "%s" not loaded' % params[2])
            
            id  = self.nextNPCID
            npc = NPC(
                id, params[1], [params[2], int(params[3]), int(params[4]), int(params[5])],
                int(params[6]), pointer['file'], pointer['offset']
            )
            self.nextNPCID += 1
            if params[0]:
                self.npcs[params[0]] = npc
            maps[params[2]].npcs[id] = npc
            
            pointer = self._doJump('sameIndent', pointer)
        
        elif command[0] == 'duplicate':
            if params[7] not in self.npcs:
                raise ScriptError("NPC \"%s\" doesn't exist" % params[7])
            if params[2] not in maps:
                raise ScriptError('Required map "%s" not loaded' % params[2])
            
            if not params[1]:
                params[1] = self.npcs[params[7]].name
            if not params[6]:
                params[6] = self.npcs[params[7]].sprite
            
            id  = self.nextNPCID
            npc = NPC(
                id, params[1], [params[2], int(params[3]), int(params[4]), int(params[5])],
                int(params[6]), self.npcs[params[7]].scriptFile, self.npcs[params[7]].scriptOffset
            )
            self.nextNPCID += 1
            if params[0]:
                self.npcs[params[0]] = npc
            maps[params[2]].npcs[id] = npc
        
        elif command[0] == 'warp':
            if params[1] not in maps:
                raise ScriptError('Required map "%s" not loaded' % params[1])
            if params[4] not in maps:
                raise ScriptError('Required map "%s" not loaded' % params[4])
            
            id   = self.nextNPCID
            warp = Warp(
                id, [params[1], int(params[2]), int(params[3])],
                [params[4], int(params[5]), int(params[6])], int(params[7]), int(params[8])
            )
            self.nextNPCID += 1
            if params[0]:
                self.warps[params[0]] = warp
            maps[params[1]].warps[id] = warp
        
        #----------------------------------------------
        
        return pointer
Scripts = ScriptObject()
