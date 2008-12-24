from aliter.db import engine, Base
from aliter.objects import *

Base.metadata.create_all(engine)
