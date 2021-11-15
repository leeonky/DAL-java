package com.github.leeonky.dal;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.ServiceLoader;

import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class DALFactoryTest {

    @Test
    void create_DAL_from_DAL_factory() {
        DAL instance = new DAL();
        DALFactory mockDALFactory = () -> instance;

        DAL actual;
        try (MockedStatic<ServiceLoader> utilities = Mockito.mockStatic(ServiceLoader.class)) {
            ServiceLoader mockServiceLoader = mock(ServiceLoader.class);
            when(mockServiceLoader.iterator()).thenReturn(singletonList(mockDALFactory).iterator());
            utilities.when(() -> ServiceLoader.load(DALFactory.class)).thenReturn(mockServiceLoader);
            actual = DAL.create();
        }

        assertThat(actual).isEqualTo(instance);
    }

    @Test
    void create_DAL_by_new_when_no_DAL_factory() {
        assertThat(DAL.create()).isInstanceOf(DAL.class);
    }
}