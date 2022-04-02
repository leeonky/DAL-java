package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;

public interface ExcuteableNode {
    Data getPropertyValue(Data data);
}
