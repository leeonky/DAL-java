package com.github.leeonky.dal.runtime.verifier;

import com.github.leeonky.dal.runtime.Data;

public interface FieldVerifier {
    boolean verify(Data object);
}
