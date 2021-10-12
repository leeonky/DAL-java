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

- DAL是一个比较简单的表达式语言，主要用于在自动化测试环境中对数据（Java Bean，Java Map/List，Json等）进行读取和断言。
- DAL的应用场景比较专注于在测试中操作数据，因此相较于编程语言，它的语言复杂性低，没有逻辑控制或变量系统，但能够集中语言特性以针对数据操作提供更多的便利性。
- DAL的执行总是针对一个输入的数据（根数据），通过`registerPropertyAccessor`和`registerListAccessor`可以泛化数据的形式（比如Json对象）:
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


## 数据访问

### 从对象中获取数据
与很多动态语言类似，通过圆点`.` + 标识符的形式获取对象的属性，通过`[字符串]`获取含有特殊字符的属性。比如有如下的数据：
``` json
    {
        "property1": 1,
        "object value": "hello"
    }
```
那么可以通过DAL分别获取各个属性：(DAL目前不支持注释)
``` json
    .property           // 1
    ['object value']    // hello
```
开头的圆点可以省略：
``` json
    property            // 1
    ['object value']    // hello
```

#### DAL中的属性包括：
- Java Class中定义的公有的Getter
- Java Class中定义的公有的Field
- Java Class中定义的公有的无参数方法
- java.util.Map中对应的键值
- 通过registerPropertyAccessor注册的属性

### 从集合中获取数据
DAL中集合也会被当做对象对待，但DAL额外提供了一些操作集合的方法。可以通过`[]`读取集合的元素，比如有如下数据：
```json
    {
        "items": [1, 2]
    }
```

那么
``` json
    items[0]    // 1
```

如果输入给DAL的根数据就是一个集合:
``` json
    [1, 2, 3]
```
那么可以直接通过`[]`获取集合的元素（DAL不需要写`this`）
``` json
    [0]     // 1
```
可以通过 size 获取元素个数，通过负数索引从集合尾部获取元素：
``` json
    items[-1]    // last element of list: 2
    items.size   // size of list 2
```

#### DAL会把如下类型做为集合：
- java.lang.Iterable
- java.util.stream.Stream
- Array
- 通过registerListAccessor注册的集合

#### 集合元素 Mapping
假如有如下的数据：
``` json
{
    "list": [{
        "value": 1
    },
    {
        "value": 2
    }]
}
```
那么可以映射元素的某个属性，并形成新的集合：
``` json
    list.value    // [1, 2]
```
如果是一个二维集合，可以通过`.@`来映射子集合的数据：
``` json
{
    "list": [[0, 1], [1, 2, 3], [2, 3, 4, 5]]
}
```

``` json
    list.@.size     // [2, 3, 4]
    list.@[0]       // [0, 1, 2]
```

### DAL支持的一些常规运算：
| 符号        | 意义             |
| ----      | ----           |
| +         | 加              |
| -         | 减              |
| *         | 乘              |
| /         | 除              |
| && 或 and  | 逻辑与            |
| \|\| 或 or | 逻辑或            |
| >         | 大于             |
| <         | 小于             |
| >=        | 大于等于           |
| <=        | 小于等于           |
| !=        | 不等             |
| ()|括号|

## 对数据进行断言



