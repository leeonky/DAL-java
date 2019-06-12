package com.github.leeonky.dal.util;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Stream;

public class BeanUtil {
    public static Object getPropertyValue(Object instance, String name) throws Exception {
        try {
            return requireGetter(instance.getClass().getMethod("get" + StringUtil.capitalize(name))).invoke(instance);
        } catch (Exception ex) {
            try {
                return requireGetter(instance.getClass().getMethod("is" + StringUtil.capitalize(name))).invoke(instance);
            } catch (Exception e) {
                return instance.getClass().getField(name).get(instance);
            }
        }
    }

    private static Method requireGetter(Method method) {
        return isGetter(method) ? method : null;
    }

    private static boolean isGetter(Method m) {
        if (m.getParameters().length == 0) {
            if (m.getName().startsWith("get") && !m.getReturnType().equals(void.class) && !m.getName().equals("getClass"))
                return true;
            return m.getName().startsWith("is") && m.getReturnType().equals(boolean.class);
        }
        return false;
    }

    public static Set<String> findPropertyNames(Class<?> clazz) {
        Set<String> properties = new HashSet<>();
        Field[] fields = clazz.getFields();
        Stream.of(fields).map(Field::getName).forEach(properties::add);
        Method[] methods = clazz.getMethods();
        Stream.of(methods).filter(BeanUtil::isGetter)
                .map(Method::getName).map(BeanUtil::getPropertyNameFromGetter).forEach(properties::add);
        return properties;
    }

    private static String getPropertyNameFromGetter(String s) {
        return StringUtil.unCapitalize(s.replaceAll("^get", "").replaceAll("^is", ""));
    }

    public static String getClassName(Object obj) {
        return obj == null ? null : obj.getClass().getName();
    }
}
