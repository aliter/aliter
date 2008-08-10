from actor import Actor
from manager import Manager


class Character(Actor):
    required = [
        'accountID', 'charNum',
    ]
    optional = [
        ('id',       None),
        ('job',      0),
        ('jobLevel', 1),
        ('zeny',     0),
        ('maxSP',    11),
        ('sp',       11),
        ('statusPoints', 0),
        ('skillPoints',  0),
        ('partyID',      0),
        ('guildID',      0),
        ('petID',        0),
        ('homunculusID', 0),
        ('hairStyle',    0),
        ('hairColor',    0),
        ('clothesColor', 0),
        ('viewWeapon',   1),
        ('viewShield',   0),
        ('viewHeadTop',  0),
        ('viewHeadMiddle', 0),
        ('viewHeadBottom', 0),
        ('saveMap',   'new_zone01'),
        ('saveX',     53),
        ('saveY',     111),
        ('online',    0),
        ('fame',      0),
    ]
    saveData = [
        'id', 'accountID', 'charNum', 'job', 'jobLevel', 
        'zeny', 'maxSP', 'sp', 'statusPoints', 'skillPoints', 
        'partyID', 'guildID', 'petID', 'homunculusID', 'hairStyle', 
        'hairColor', 'clothesColor', 'viewWeapon', 'viewShield', 'viewHeadTop', 
        'viewHeadMiddle', 'viewHeadBottom', 'saveMap', 'saveX', 'saveY', 
        'online', 'fame', 
    ]
    
    def __init__(self, **kwargs):
        self.required.extend(super(Character, self).required)
        self.optional.extend(super(Character, self).optional)
        self.saveData.extend(super(Character, self).saveData)
        super(Character, self).__init__(**kwargs)
        
        self.gameID = self.accountID
    
    def load(self):
        from app.event import Event
        Event.warp(self, self.saveX, self.saveY, self.saveMap)
    
    def save(self, map, x, y):
        from app.event import Event
        self.saveMap = map
        self.saveX = x
        self.saveY = y
        Characters.save(self)
    
    def loadInventory(self):
        from app.objects import Inventory, Items
        from app.packets import generatePacket
        
        inventory = Inventory.getAll(characterID = self.id)
        
        equips = []
        items = []
        
        index = 2
        for stock in inventory:
            item = Items.get(stock.itemID)
            if item.equipLocations == None:
                items.append(generatePacket(
                    "inventoryItem",
                    index = index,
                    itemID = item.id,
                    type = item.type,
                    identified = stock.identified,
                    amount = stock.amount,
                    card1 = stock.card1,
                    card2 = stock.card2,
                    card3 = stock.card3,
                    card4 = stock.card4
                ))
            else:
                # Crazy-ass client logic.
                card1 = stock.forger and 255 or stock.card1
                card2 = stock.forger and stock.element + (stock.very * 1280) or stock.card2
                card3 = stock.forger and (18928 + (stock.forger - 150000)) or stock.card3
                card4 = stock.forger and 2 or stock.card4
                
                equips.append(generatePacket(
                    "inventoryEquip",
                    index = index,
                    itemID = item.id,
                    type = item.type,
                    identified = stock.identified,
                    equipLocations = item.equipLocations,
                    equipPoint = stock.equipLocation,
                    refine = stock.refine,
                    card1 = card1,
                    card2 = card2,
                    card3 = card3,
                    card4 = card4
                ))
            
            index += 1
        
        self.session.sendPacket(
            0x1ee,
            data = "".join(items)
        )
        
        self.session.sendPacket(
            0xa4,
            data = "".join(equips)
        )
            

class CharacterManager(Manager):
    modelClass = Character
    cacheDict  = {}
    table  = 'characters'
    schema = [
        'id', 'accountID', 'charNum', 'name', 'job',
        'baseLevel', 'baseExp', 'jobLevel', 'jobExp', 'zeny',
        'str', 'agi', 'vit', 'int', 'dex',
        'luk', 'maxHP', 'hp', 'maxSP', 'sp',
        'statusPoints', 'skillPoints', 'partyID', 'guildID', 'petID',
        'homunculusID', 'hairStyle', 'hairColor', 'clothesColor', 'viewWeapon',
        'viewShield', 'viewHeadTop', 'viewHeadMiddle', 'viewHeadBottom', 'map',
        'x', 'y', 'saveMap', 'saveX', 'saveY', 'online',
        'fame',
    ]

Characters = CharacterManager()
