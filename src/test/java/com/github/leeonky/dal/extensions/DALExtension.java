package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.Extension;

public class DALExtension implements Extension {

    public static Extension extensionForTest = dal -> {
    };

    @Override
    public void extend(DAL dal) {
        extensionForTest.extend(dal);
    }
}
