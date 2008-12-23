from app.constants import *

class KafraBase(object):
    """Base class, common to all Kafras."""
    def saved(self, where):
        say("Your Respawn Point has been saved here %s." % where,
            "Thank you for using the Kafra Services.")
        close()
    
    def storage(self):
        """docstring for storage"""
        say("Pretend I just opened your storage.")
        close()

    def warp(self, selection):
        """docstring for doWarp"""
        name, price = selection.split(" -> ")
        price = int(price)
        next()
        if actor.zeny < price and not actor.hasItem(7060):
            say("[Kafra Employee]",
                "I'm sorry, but you don't have enough zeny for the Teleport Service.",
                "The fee to teleport to %s is %d zeny." % (name, price))
            close()
        else:
            if actor.hasItem(7060):
                actor.takeItem(7060)
            else:
                actor.zeny -= price # FIXME: Make this stick.
        
            print "Taking %d zeny." % price
        
            if (name == "Alberta"):
                actor.warp("alberta", 117, 56)
            elif (name == "Al De Baran"):
                actor.warp("aldebaran", 143, 110)
            elif (name == "Aldebaran"):
                actor.warp("aldebaran", 143, 110)
            elif (name == "Comodo"):
                actor.warp("comodo", 207, 144)
            elif (name == "Izlude"):
                actor.warp("izlude", 91, 105)
            elif (name == "Geffen"):
                actor.warp("geffen", 120, 39)
            elif (name == "Morroc"):
                actor.warp("morroc", 156, 46)
            elif (name == "Payon"):
                actor.warp("payon", 168, 103)
            elif (name == "Prontera"):
                actor.warp("prontera", 116, 72)
            elif (name == "Mjolnir Dead Pit"):
                actor.warp("mjolnir_02", 82, 347)
            elif (name == "Comodo Pharos Beacon"):
                actor.warp("cmd_fild07", 127, 134)
            elif (name == "Orc Dungeon"):
                actor.warp("gef_fild10", 52, 326)
            elif (name == "Umbala"):
                actor.warp("umbala", 130, 130)
            elif (name == "Juno"):
                actor.warp("yuno", 157, 123)
        
            close()
    
    def teleport(self):
        """docstring for teleport"""
        next()
        if actor.map == "einbroch":
            say("[Kafra Employee]",
                "Because of the ^FF0000Limited Transport Agreemeent^000000,"
                "the Kafra Corporation cannot provide Teleport Services in the Schwaltzvalt Republic.")
            next()
            say("[Kafra Employee]",
                "We ask that you please use the Airship Service instead.",
                "Thank you for your understanding and cooperation.")
            next()
            menu(menus[4])
        else:
            say("[Kafra Employee]",
                "Please choose your destination.")
        
            menuItems = []
        
            for location, price in warps.iteritems():
                menuItems.append(("%s -> %d" % (location, price), doWarp))
        
            menuItems.append(("Cancel", kafraEnd))
        
            menu(*menuItems)

    def pushcart():
        """docstring for pushcart"""
        if actor.job != JOB["Merchant"]:
            say("[Kafra Employee]",
                "I'm sorry, but the Pushcart rental service is only available to Merchants, Blacksmiths, Master Smiths, Alchemists and Biochemists.")
        # TODO: Check for a cart, and give one.
        close()

    def guide():
        """docstring for guide"""
        say("[Kafra Employee]",
            "WIP...")
        
        close()

    def info():
        """docstring for info"""
        say("[Kafra Employee]",
            "WIP...")
        
        close()

    def end():
        """docstring for cancel"""
        if actor.map == "niflheim":
            say("^666666Kaffffra n-never diiiiiiiiiiiiiies. On...",
                "On y-yooour siiiiide~^000000")
        else:
            say("We, here at the Kafra Corporation,",
                "are always endeavoring to provide you with the best services. We hope that we meet your adventuring needs and standards of excellence.")
        close()
    
    menus = (
        (
            ("Save", save),
            ("Use Storage", storage),
            ("Use Teleport Service", teleport),
            ("Rent a Pushcart", pushcart),
            ("Guide", guide),
            ("Check Other Information", info),
            ("Cancel", end)
        ),
        ( ("Save", save), ("Use Storage", storage), ("Cancel", end) ),
        ( ("Use Storage", storage), ("Cancel", end) ),
        (
            ("Save", save),
            ("Use Storage", storage),
            ("Rent a Pushcart", pushcart),
            ("Guide", guide),
            ("Check Other Information", info),
            ("Cancel", end)
        ),
        ( # Used for Einbech
            ("Save", save),
            ("Use Storage", storage),
            ("Use Teleport Service", teleport),
            ("Rent a Pushcart", pushcart),
            ("Guide", guide),
            ("Check Other Information", info),
            ("Cancel", end)
        ),
        (
            ("Use Storage", storage),
            ("Rent a Pushcart", pushcart),
            ("Check Other Information", info),
            ("Guide", guide),
            ("Cancel", end)
        ),
        (
            ("Use Storage", storage),
            ("Guide", guide),
            ("Check Other Information", info),
            ("Cancel", end)
        ),
        (
            ("Save", save),
            ("Use Storage", storage),
            ("Rent a Pushcart", pushcart),
            ("Cancel", end)
        ),
        (
            ("Save", save),
            ("Use Storage", storage),
            ("Check Other Information", info),
            ("Cancel", end)
        )
    )
