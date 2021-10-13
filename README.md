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

- DAL是一个比较简单的表达式语言，主要用于在自动化测试环境中对数据进行读取和断言。
- DAL的应用场景比较专注于在测试中操作数据，因此相较于编程语言，它的语言复杂性低，没有逻辑控制或变量系统，但能够集中语言特性以针对数据操作提供更多的便利性。
- DAL的执行总是针对一个输入数据（根数据）。
- DAL输入的数据是一个泛化的类型，不但可以是 Java Class，Java Map/List，还可以通过`registerPropertyAccessor`和`registerListAccessor`方法的注册而支持其他类型格式。以下代码用来支持 JsonObject 类型数据：
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

## 执行 DAL 语句
通过如下两个API来执行代码并返回结果
``` java
<T> T evaluate(Object input, String expression)`
<T> List<T> evaluateAll(Object input, String expressions)
```

evaluateAll 会执行多条语句并把多个语句的结果以集合形式返回：
``` java
new DAL().evaluate(1, "+ 1");   // return 2
new DAL().evaluate(null, "asList(1, 2)"");      //return [1, 2]
new DAL().evaluateAll(null, "asList(1, 2)"");   //return [[1, 2]]
new DAL().evaluateAll(null, "1 2");             //return [1, 2]
```
无论是访问数据还是断言数据，都推荐用以上两个API。访问数据时返回得到的数据。断言数据时，如果断言失败则直接抛出 `AssertionFailure` 异常。

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
    },{
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
    list.@[-1]       // [1, 3, 5]
```

#### 调用对象方法
如果输入数据是一个Java对象，DAL可以通过无`()`的方式调用一个无参数方法，并返回值。
``` json
"hello".length  //  "hello".length()
```

### DAL支持的运算
#### 字面值常量
目前DAL是基于Java实现的，基本的数值类型都是Java中的常用类型，但DAL添加了一些额外的字符后缀用以描述 byte，short，BigInteger，BigDecimal 字面值：
|Java 类型|DAL 代码举例|
|---------|--------|
|byte/Byte   | 100Y |
|short/Short | 100S |
|int/Integer | 100  |
|long/Long   | 100L |
|float/Float | 100F |
|double/Double| 100D |
|BigInteger   | 100BI|
|BigDecimal   | 100BD|

#### 字符串常量
DAL支持通过`''`或`""`包含的字符串常量

如果没有给定任何后缀，DAL会尝试按照 int / long / BigInteger / double / BigDecimal 顺序选择一个合适的类型

#### 运算操作符
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
| ( )|括号|
数学运算规则和Java语言中的运算规则一致，在不同类型之间的运算会涉及类型提升。

## 对数据进行断言

自动化测试的用例都应该是明确和可预知的，DAL只通符号（`=`和`:`）和`is`关键字，来对数据进行断言。
DAL可以进行与**值**、**正则匹配**、**对象**、**集合**有关的断言，也可以通过预定义具名 Scheam 来断言**数据报文格式**。

``` java
new DAL().evaluateAll("hello", "= 'hello'");    // Pass!
```

### "AssertEqual"
- #### 严格断定（ `=` ）
    判断值相等是测试中的最常见的断言。DAL通过`=`来达到此效果，它表示一种严格的断定，要求类型和值都相同。比如有如下的数据：
    ``` json
    {
        "number": 1.0
        "string": "hello"
    }
    ```
    那么如下的断言：
    ``` java
    number = 1          // 失败，类型不符合
    number = 1.0        // 如果依赖的 JSON 库将输入 JSON 数据中的1.0按 double 处理，则断言通过
    string = 'hello'    // 通过
    ```
    
- #### 不严格断定（语义断定）(`:`)
    如果从业务而非代码的视角来看待测试用例，我们可能不太关心属性的类型。比如该用 int 还是 long 类型的数字做比较。DAL 通过`:`提供语义断定。比如有如下Java数据
    ``` java
    public class Bean {
        public BigDecimal number = new BigDecimal("1.0");
        public Type type = Type.B;
        public enum Type {
            A, B
        }
    }
    ```
    那么如下的断言：
    ``` java
    number = 1      // 数值相符，通过。
    number = 1.0    // 数值相符，通过。
    type = 'B'      // 值相符，通过
    ```
    DAL在处理`:`断言时，如果比较对象都是数字，那么先提升某一个操作数的类型，然后再进行数值比较。如果是其他类型，则尝试通过内部`Converter`将输入值转换成比对值的类型，上例中将`type`属性的断言就是将`enum Type`转换成 String 类型后再比较。
    
    ##### 注：DAL目前的版本实现下，`:`不允许在`Number` `String`和`Boolean`之间自动转换
    
    ``` json
    '1': 1          //不通过
    1: '1'          //不通过
    true: 'true'    //不通过
    ```
    
### 正则匹配
DAL通过`/regex/`定义正则表达，然后结合`=`和`:`来进行匹配断言。 `=`要求输入类型必须是字符串类型，`:`则先将输入值转换成字符串，然后再进行正则匹配。
``` json
    'hello' = /hello/   // 通过
    1: /\d/             // 通过，先将1转换为'1'再比较
```

注：只有在`=`和`:`后的`/ /`才会被识别为正则表达式。

### 断言一个数据对象
常见的测试框架在对数据对象断言时，都是先定义一个新对象，然后再通过各种策略与待测对象比较。DAL不支持定义对象，但是可以用`{}`的方式达到断言对象的效果，并且将对象断言表达的更加直接和清晰。
DAL对象断言仍以`=`或`:`开始，后边跟随一个`{}`，在`{}`中阐明各子属性的断言，并且支持嵌套。比如有如下的数据：

``` json
    {
        "number": 1.0
        "string": "hello"
        "object": {
            "value": 2
        }
    }
```
那么如下的断言可以通过：
``` java
    = {
        number: 1
        string: 'hello'
        object.value: 2
    }
```
**注：这里并非定义了一个新对象，而是通过类似的形式达到对象的断言效果**
开头的`=`也是严格断言的意思，表示待断言对象中的所有属性都应该出现在`{}`中。

