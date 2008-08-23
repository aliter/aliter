from model import Model
from manager import Manager
from character import Characters


class Guild(Model):
    required = [
        'name', 'masterID'
    ]
    optional = [
        ('id', None),
        ('level', 1),
        ('connectedMembers', 0),
        ('capacity', 0),
        ('averageLevel', 0),
        ('exp', 0),
        ('nextExp', 2000000),
        ('skillPoints', 0),
        ('messageTitle', ''),
        ('messageBody', '')
    ]
    saveData = [
        'id', 'name', 'masterID', 'level', 'connectedMembers', 'capacity',
        'averageLevel', 'exp', 'nextExp', 'skillPoints', 'messageTitle',
        'messageBody'
    ]
    
    def master(self):
        """
        Returns the guild's master.
        """
        return Characters.get(self.masterID)
    
    def members(self):
        """
        Returns the guild's members.
        """
        return Characters.getAll(guildID = self.id)
    
    def emblem(self):
        """
        Returns the guild's emblem.
        """
        return GuildEmblems.get(guildID = self.id)
    
    def positions(self):
        """
        Returns the guild's positions.
        """
        return GuildPositions.getAll(guildID = self.id)
    
    def relations(self):
        """
        Returns the guild's relations.
        """
        return GuildRelations.getAll(guildID = self.id)

class GuildManager(Manager):
    modelClass = Guild
    cacheDict  = {}
    table  = 'guilds'
    schema = [
        'id', 'name', 'masterID', 'level', 'connectedMembers', 'capacity',
        'averageLevel', 'exp', 'nextExp', 'skillPoints', 'messageTitle',
        'messageBody'
    ]

Guilds = GuildManager()


class GuildEmblem(Model):
    """
    Guild emblems.
    """
    required = [
        'guildID'
    ]
    optional = [
        ('id', None),
        ('data', '')
    ]
    saveData = [
        'id', 'guildID', 'data'
    ]

class GuildEmblemManager(Manager):
    modelClass = GuildEmblem
    cacheDict  = {}
    table  = 'guildEmblems'
    schema = [
        'id', 'guildID', 'data'
    ]

GuildEmblems = GuildEmblemManager()


class GuildRelation(Model):
    """
    Relationships between guilds, e.g. allies/oppositions.
    """
    required = [
        'guildID', 'relatedID', 'type'
    ]
    optional = [
        ('id', None),
        ('name', '')
    ]
    saveData = [
        'id', 'guildID', 'relatedID', 'name', 'type'
    ]

class GuildRelationManager(Manager):
    modelClass = GuildRelation
    cacheDict  = {}
    table  = 'guildRelations'
    schema = [
        'id', 'guildID', 'relatedID', 'name', 'type'
    ]

GuildRelations = GuildRelationManager()


class GuildPosition(Model):
    """
    Positions of a guild.
    """
    required = [
        'guildID', 'positionID'
    ]
    optional = [
        ('id', None),
        ('name', 'Position'),
        ('mode', 0),
        ('tax', 0)
    ]
    saveData = [
        'id', 'guildID', 'positionID', 'name', 'mode', 'tax'
    ]

class GuildPositionManager(Manager):
    modelClass = GuildPosition
    cacheDict  = {}
    table  = 'guildPositions'
    schema = [
        'id', 'guildID', 'positionID', 'name', 'mode', 'tax'
    ]

GuildPositions = GuildPositionManager()