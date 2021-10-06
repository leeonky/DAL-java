package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.runtime.ArrayAccessor;
import org.json.JSONArray;
import org.json.JSONException;

public class JSONArrayAccessor implements ArrayAccessor<JSONArray> {
    @Override
    public Object get(JSONArray jsonArray, int index) {
        try {
            return jsonArray.get(index);
        } catch (JSONException e) {
            throw new IllegalStateException(e);
        }
    }

    @Override
    public int size(JSONArray jsonArray) {
        return jsonArray.length();
    }
}
