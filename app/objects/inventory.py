from model import Model
from manager import Manager


class InventoryItem(Model):
    required = [
        "characterID", "itemID"
    ]
    optional = [
        ("id", None),
        ("amount", 1),
        ("equipLocation", 0),
        ("identified", 1),
        ("refine", 0),
        ("broken", 0),
        ("forger", 0),
        ("element", 0),
        ("very", 0),
        ("card1", 0),
        ("card2", 0),
        ("card3", 0),
        ("card4", 0)
    ]
    saveData = [
        "id", "characterID", "itemID", "amount", "equipLocation",
        "identified", "refine", "broken", "forger", "element", "very",
        "card1", "card2", "card3", "card4"
    ]

class InventoryManager(Manager):
    modelClass = InventoryItem
    cacheDict  = {}
    table  = "inventory"
    schema = [
        "id", "characterID", "itemID", "amount", "equipLocation",
        "identified", "refine", "broken", "forger", "element", "very",
        "card1", "card2", "card3", "card4"
    ]

Inventory = InventoryManager()
