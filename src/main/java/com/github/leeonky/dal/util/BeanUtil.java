package com.github.leeonky.dal.util;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
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

    public static Set<Member> findPropertyReaders(Class<?> clazz) {
        Set<Member> members = new HashSet<>();
        Method[] methods = clazz.getMethods();
        Set<String> getters = Stream.of(methods).filter(BeanUtil::isGetter)
                .peek(members::add)
                .map(Method::getName)
                .map(BeanUtil::getPropertyNameFromGetter)
                .collect(Collectors.toSet());

        Field[] fields = clazz.getFields();
        Stream.of(fields)
                .filter(f -> !getters.contains(f.getName()))
                .forEach(members::add);
        return members;
    }

    public static Set<String> findPropertyReaderNames(Class<?> clazz) {
        return findPropertyReaders(clazz).stream()
                .map(BeanUtil::getPropertyName).collect(Collectors.toSet());
    }

    public static String getPropertyName(Member m) {
        if (m instanceof Field)
            return m.getName();
        return getPropertyNameFromGetter(m.getName());
    }

    private static String getPropertyNameFromGetter(String s) {
        return StringUtil.unCapitalize(s.replaceAll("^get", "").replaceAll("^is", ""));
    }

    public static String getClassName(Object obj) {
        return obj == null ? null : obj.getClass().getName();
    }

    public static <T extends Annotation> T getAnnotation(Member m, Class<T> annotationClass) {
        if (m instanceof Field)
            return ((Field) m).getAnnotation(annotationClass);
        return ((Method) m).getAnnotation(annotationClass);
    }
}
