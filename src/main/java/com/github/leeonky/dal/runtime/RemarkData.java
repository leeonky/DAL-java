package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public class RemarkData extends RuntimeData {
    private final String remark;

    public RemarkData(Data data, DALRuntimeContext runtimeContext, String remark) {
        super(data, runtimeContext);
        this.remark = remark;
    }

    public String remark() {
        return remark;
    }

    public Data acceptRemarkAsParameter() {
        return data().map(acceptor -> ((DataRemarkParameterAcceptor) acceptor).apply(remark()));
    }
}
