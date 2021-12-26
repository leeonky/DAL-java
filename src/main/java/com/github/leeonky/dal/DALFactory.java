package com.github.leeonky.dal;

import java.util.Iterator;
import java.util.ServiceLoader;

public interface DALFactory {
    static DAL create() {
        Iterator<DALFactory> iterator = ServiceLoader.load(DALFactory.class).iterator();
        if (iterator.hasNext())
            return iterator.next().newInstance();
        return new DAL().extend();
    }

    DAL newInstance();
}
