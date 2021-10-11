# DAL-java
[![travis-ci](https://travis-ci.com/leeonky/DAL-java.svg?branch=master)](https://travis-ci.com/github/leeonky/DAL-java)
[![coveralls](https://img.shields.io/coveralls/github/leeonky/DAL-java.svg)](https://coveralls.io/github/leeonky/DAL-java)
[![Lost commit](https://img.shields.io/github/last-commit/leeonky/DAL-java.svg)](https://github.com/leeonky/DAL-java)
[![Maven Central](https://img.shields.io/maven-central/v/com.github.leeonky/DAL-java.svg)](https://search.maven.org/artifact/com.github.leeonky/DAL-java)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/ef6f8c72b9684691b5bb9079fa7ed025)](https://app.codacy.com/project/leeonky/DAL-java/dashboard)
[![Maintainability](https://api.codeclimate.com/v1/badges/d6b15c6a8af428251d79/maintainability)](https://codeclimate.com/github/leeonky/DAL-java/maintainability)
[![Code Climate issues](https://img.shields.io/codeclimate/issues/leeonky/DAL-java.svg)](https://codeclimate.com/github/leeonky/DAL-java/maintainability)
[![Code Climate maintainability (percentage)](https://img.shields.io/codeclimate/maintainability-percentage/leeonky/DAL-java.svg)](https://codeclimate.com/github/leeonky/DAL-java/maintainability)

## 数据访问与断言
- DAL是一个比较简单的表达式语言，主要用于在自动化测试环境中对数据（Java Bean，Java Map/List，Json等）进行读取和断言。
- DAL的应用场景比较专注于在测试中操作数据，因此相较于编程语言，它的语言复杂性低，没有逻辑控制或变量系统，但能够集中语言特性以针对数据操作提供更多的便利性。
- DAL的执行总是针对一个输入的数据，通过`registerPropertyAccessor`和`registerListAccessor`可以泛化数据的形式（比如Json对象）:
``` java
        DAL dal = new DAL();
        dal.getRuntimeContextBuilder().registerPropertyAccessor(JSONObject.class, new PropertyAccessor<JSONObject>() {
            @Override
            public Object getValue(JSONObject instance, String name) {
                try {
                    return instance.has(name) ? instance.get(name) : JSONObject.NULL;
                } catch (JSONException e) {
                    throw new IllegalArgumentException(e);
                }
            }

            @Override
            public Set<String> getPropertyNames(JSONObject instance) {
                Set<String> set = new HashSet<>();
                Iterator iterator = instance.keys();
                while (iterator.hasNext())
                    set.add(iterator.next().toString());
                return set;
            }

            @Override
            public boolean isNull(JSONObject instance) {
                return instance == null || instance.equals(JSONObject.NULL);
            }
        });

        dal.getRuntimeContextBuilder().registerListAccessor(JSONArray.class, new ArrayAccessor<JSONArray>() {
            @Override
            public Object get(JSONArray jsonArray, int index) {
                try {
                    return jsonArray.get(index);
                } catch (JSONException e) {
                    throw new IllegalArgumentException(e);
                }
            }

            @Override
            public int size(JSONArray jsonArray) {
                return jsonArray.length();
            }
        });
```
- 通过如下两个API来执行代码并返回结果
``` java
<T> T evaluate(Object input, String expression)`
<T> List<T> evaluateAll(Object input, String expressions)
```

## 从对象中获取数据

## 对数据进行断言


