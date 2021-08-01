package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.util.PropertyAccessor;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

public class JsonPropertyAccessor implements PropertyAccessor<JSONObject> {
    @Override
    public Object getValue(JSONObject instance, String name) {
        try {
            return instance.get(name);
        } catch (JSONException e) {
            return JSONObject.NULL;
        }
    }

    @Override
    public Set<String> getPropertyNames(JSONObject instance) {
        Set<String> set = new HashSet<>();
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
