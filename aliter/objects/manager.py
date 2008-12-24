from app import db


class Manager(object):
    cache = {}
    
    def _rowToObject(self, schema, row):
        object = {}
        
        for offset, field in enumerate(row):
            object[schema[offset]] = field
        
        return self.modelClass(**object)
    
    def _generateWHERE(self, *args, **kwargs):
        output = []
        operator = " AND "
        operators = {}
        for field, value in kwargs.iteritems():
            checks = (len(output) + 1) / 2
            
            if field[:-1] == "op" and field[-1].isdigit(): # This is an operator setting.
                place = int(field[-1])
                
                if place > checks: # Queue this operator up
                    operators[place] = " %s " % value
                elif place < checks: # It's already been processed; replace the operator after it
                    output[place - 1] = " %s " % value
                else: # Goodie, we can just set the operator
                    operator = " %s " % value
                
                continue
            elif checks in operators: # An operator has been queued for this
                operator = operators[checks]
                
            # Find condition
            # TODO: LT, GT, LTE, GTE, etc.
            condition = '='
            
            if field in self.schema:
                # Seperate all values with an AND
                if len(output):
                    output.append(operator)
                
                # Determine type
                if type(value) not in (int, float, long, complex):
                    value = '"%s"' % value.replace('"', '\\"')
                
                output.append('`%s` %s %s' % (field, condition, value))
            
            # Set the operator back to normal
            if operator != " AND ":
                operator = " AND "
        
        return "".join(output)
    
    def get(self, id=None, cache=True, force=False, **kwargs):
        # Already cached?
        if not force and 'id' in kwargs and kwargs['id'] in self.cache:
            return self.cache[kwargs['id']]
        
        # Generate WHERE clause
        if id and 'id' not in kwargs:
            kwargs['id'] = id
        
        where = self._generateWHERE(**kwargs)
        if not where:
            return None
        
        row = db.getOne('SELECT * FROM `%s` WHERE %s LIMIT 1' % (self.table, where))
        if not row:
            return None
        
        model = self._rowToObject(self.schema, row)
        
        if cache or force:
            self.cache[model.id] = model
        
        return model

    def getAll(self, cache=True, **kwargs):
        # Generate WHERE clause
        where = self._generateWHERE(**kwargs)
        if not where:
            return []
        
        rows = db.getAll('SELECT * FROM `%s` WHERE %s' % (self.table, where))
        if not rows:
            return []
        
        models = []
        for row in rows:
            model = self._rowToObject(self.schema, row)
            
            if cache:
                self.cache[model.id] = model
            
            models.append(model)
        
        return models

    def create(self, cache=True, **kwargs):
        #try:
        object = self.modelClass(**kwargs)
        
        if cache:
            self.cache[object.id] = object
        
        return self.save(object)
        #except:
        #    return False
        return True

    def save(self, object):
        data   = object.getForSave()
        keys   = []
        values = []
        
        for key, value in data.iteritems():
            keys.append('`%s` = %%s' % key)
            values.append(value)
        
        cursor = db.cursor()
        if object.id:
            query = 'UPDATE `%s` SET %s WHERE `id` = %%s' % (self.table, ', '.join(keys))
            values.append(data['id'])
            cursor.execute(query, values)
            db.commit()
            return object
        else:
            query = 'INSERT INTO `%s` SET %s' % (self.table, ', '.join(keys))
            cursor.execute(query, values)
            db.commit()
            object.id = cursor.lastrowid
            return object

    def delete(self, id = None, **kwargs):
        if id:
            del self.cache[id]
            cursor = db.cursor()
            cursor.execute('DELETE FROM `%s` WHERE `id` = %%s' % self.table, id)
            db.commit()
        else:
            # TODO: Remove from cache
            where = self._generateWHERE(**kwargs)
            if not where:
                return
            
            cursor = db.cursor()
            cursor.execute("DELETE FROM `%s` WHERE %s" % (self.table, where))
        
        return True

