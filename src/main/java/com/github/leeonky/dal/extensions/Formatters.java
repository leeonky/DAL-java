package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.Order;

import static com.github.leeonky.dal.runtime.Order.BUILD_IN;

@Order(BUILD_IN)
public class Formatters implements Extension {

    @Override
    public void extend(DAL dal) {
        dal.getRuntimeContextBuilder()
                .registerValueFormat(new com.github.leeonky.dal.format.Formatters.String())
                .registerValueFormat(new com.github.leeonky.dal.format.Formatters.URL())
                .registerValueFormat(new com.github.leeonky.dal.format.Formatters.Instant())
                .registerValueFormat(new com.github.leeonky.dal.format.Formatters.LocalDate())
                .registerValueFormat(new com.github.leeonky.dal.format.Formatters.LocalDateTime())
                .registerValueFormat(new com.github.leeonky.dal.format.Formatters.Enum<>())
                .registerValueFormat(new com.github.leeonky.dal.format.Formatters.Number())
                .registerValueFormat(new com.github.leeonky.dal.format.Formatters.PositiveInteger())
                .registerValueFormat(new com.github.leeonky.dal.format.Formatters.Integer())
                .registerValueFormat(new com.github.leeonky.dal.format.Formatters.PositiveNumber())
                .registerValueFormat(new com.github.leeonky.dal.format.Formatters.ZeroNumber())
                .registerValueFormat(new com.github.leeonky.dal.format.Formatters.Boolean())
        ;
    }
}
