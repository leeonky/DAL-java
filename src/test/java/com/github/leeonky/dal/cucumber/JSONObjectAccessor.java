package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.runtime.PropertyAccessor;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;

public class JSONObjectAccessor implements PropertyAccessor<JSONObject> {
    @Override
    public Object getValue(JSONObject instance, Object name) {
        try {
            return instance.get((String) name);
        } catch (JSONException e) {
            return JSONObject.NULL;
        }
    }

    @Override
    public Set<Object> getPropertyNames(JSONObject instance) {
        Set<Object> set = new LinkedHashSet<>();
        Iterator iterator = instance.keys();
        while (iterator.hasNext())
            set.add(iterator.next().toString());
        return set;
    }

    @Override
    public boolean isNull(JSONObject instance) {
        return instance == null || instance.equals(JSONObject.NULL);
    }
}
