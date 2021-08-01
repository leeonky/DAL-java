package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.util.ListAccessor;
import org.json.JSONArray;
import org.json.JSONException;

public class JSONArrayListAccessor implements ListAccessor<JSONArray> {
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
