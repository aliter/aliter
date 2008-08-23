from model import Model
from manager import Manager


class Monster(Model):
    required = [
    ]
    optional = [
    ]
    saveData = [
    ]

class MonsterManager(Manager):
    modelClass = Monster
    cacheDict  = {}
    table  = "monsters"
    schema = [
        "id", "cleanName", "translatedName", "internationalName", "level",
        "hp", "sp", "baseExp", "jobExp", "atkMin", "atkMax", "physicalDef",
        "magicDef", "str", "agi", "vit", "int", "dex", "luk", "attackType",
        "skillRange", "sightRange", "scale", "race", "element", "mode", "speed",
        "attackDelay", "attackAnimation", "damageAnimation", "mvpExp",
        "mvpBonusExp", "mvpdropID1", "mvpdropRate1", "mvpdropID2",
        "mvpdropRate2", "mvpdropID3", "mvpdropRate3", "dropID1", "dropRate1",
        "dropID2", "dropRate2", "dropID3", "dropRate3", "dropID4",
        "dropRate4", "dropID5", "dropRate5", "dropID6", "dropRate6",
        "dropID6", "dropRate7", "dropID8", "dropRate8", "dropID9",
        "dropRate9", "dropCardID"
    ]

Monsters = MonsterManager()
