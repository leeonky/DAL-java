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
DAL通过如下两个API来执行代码并返回结果
``` java
<T> T evaluate(Object input, String expression)`
<T> List<T> evaluateAll(Object input, String expressions)
```

`evaluateAll` 会执行多条语句并把多个语句的结果以集合形式返回：
``` java
new DAL().evaluate(1, "+ 1");           // return 2
new DAL().evaluateAll(null, "1 2");     //return [1, 2]
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

如果没有给定任何后缀，DAL会尝试按照 int / long / BigInteger / double / BigDecimal 顺序选择一个合适的类型

#### 字符串常量
DAL支持通过`''`或`""`包含的字符串常量

#### 运算操作符
| 符号        | 意义             |
| ----      | ----           |
| +         | 加              |
| -         | 减              |
| *         | 乘              |
| /         | 除              |
| && 或 and  | 逻辑与            |
| ,     | 逻辑与（不能用在对象/集合断言上下文中|
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
DAL可以进行与**值**、**正则匹配**、**对象**、**集合**有关的断言，也可以通过预定义具名 Scheam 来断言。

``` java
new DAL().evaluateAll("hello", "= 'hello'");    // Pass!
new DAL().evaluateAll(1+1, "= 1+1");    // Pass!
```

### "AssertEqual"
- #### 严格断定（ `=` ）
判断值相等是测试中的最常见的断言。DAL通过`=`来达到此效果，它表示一种严格的断定，要求类型和值都相同。比如有如下的数据：
``` json
{
    "number": 2.0
    "string": "hello"
}
```
那么如下的断言：
``` java
number= 2              // 失败，类型不符合
number= 2.0            // 如果依赖的 JSON 库将输入 JSON 数据中的 2.0 按 double 处理，则断言通过
string= 'hello'        // 通过
string= 'hel' + 'lo'   // 通过
```
    
- #### 不严格断定（语义断定）(`:`)
如果从业务而非代码的视角来看待测试用例，我们可能不太关心属性的类型。比如该用 int 还是 long 类型的数字做比较。DAL 通过`:`提供语义断定。比如有如下Java数据
``` java
public class Bean {
    public BigDecimal number = new BigDecimal("2.0");
    public Type type = Type.B;
    public enum Type {
        A, B
    }
}
```
那么如下的断言：
``` java
number= 2      // 数值相符，通过。
number= 2.0    // 数值相符，通过。
type= 'B'      // 值相符，通过
```
DAL在处理`:`断言时，如果比较对象都是数字，那么先提升某一个操作数的类型，然后再进行数值比较。如果是其他类型，则尝试通过内部`Converter`将输入值转换成比对值的类型，上例中将`type`属性的断言就是将`enum Type`转换成 String 类型后再比较。
    
##### 注：DAL目前的版本实现下，`:`不允许在`Number` `String`和`Boolean`之间自动转换
    
``` javascript
'1': 1          //不通过
1: '1'          //不通过
true: 'true'    //不通过
```
    
### 正则匹配
DAL通过`/regex/`定义正则表达，然后结合`=`和`:`来进行匹配断言。 `=`要求输入类型必须是字符串类型，`:`则先将输入值转换成字符串，然后再进行正则匹配。
``` javascript
    'hello' = /hello/   // 通过
    1 : /\d/            // 通过，先将1转换为'1'再比较
    1 = /\d/            // 不通过，1不是字符串
```

注：只有在`=`和`:`后的`/ /`才会被识别为正则表达式。

### 通配符匹配
如果将期望值指定为`*`，则无论是`=`还是`:`，无论输入值是任何值或类型，断言结果都为通过。`*`主要在对象或集合的断言中，起到占位符的作用。
``` javascript
1: *        // 通过
1= *        // 通过
null: *     // 通过
null= *     // 通过
```

### 和`null`比较
DAL中任何非 null 对象和 null 的`=`或`:`断言都不通过。同样的如果输入值是 null，除`*`断言外，其他任何断言也都不通过。

### 断言一个数据对象
常见的测试框架在对数据对象断言时，都是先定义一个新对象，然后再通过各种策略与待测对象比较。DAL不支持定义对象。考虑如下输入数据：
``` json
{
    message: {
        "id": 1,
        "value": "hello Tom",
        “receiver": {
            "id": "007",
            "name": "James"
        }
    }
}
```
我们可以用DAL以如下的方式断言：
``` javascript
    message.id= 1
    message.value= /^hello/
    message.receiver.id= '007'
    message.receiver.name= 'James'
```
这里会键入多次`message`。而且随着数据层级的增多，重复会越来越多。
DAL提供了`{}`用类似定义对象的方式来断言对象，并且在`{}`内可以嵌套混杂各种计算表达式、正则表达式或子对象、集合断言。将对象断言表达的更加直接和清晰。刚才的实例可以写成：
```
    message= {
        id= 1
        value= /^hello/
        receiver= {
            id= '007'
            name= 'James'
        }
    }
```
**注：这里并非定义了一个新对象。可以把`{}`理解为一个语句块，然后在不同层级的语句块中定义断言表达式。**

- #### 限定属性
DAL对象断言仍以`=`或`:`开始，后边跟随一个`{}`，在`{}`中阐明各子属性的断言。开头的`=`也是严格断言的意思，表示待断言对象中不能有`{}`中没有提及的属性。刚才的实例如果写成如下将会断言失败：
```javascript
    message= {
        id= 1
    }
    //  失败。未预期的属性：value, receiver
```
如果仅对数据中部分属性进行断言，又不想写出全部属性名。可以使用`:`断言：
```javascript
    message: {
        id= 1
        receiver.name= 'James'
    }
    //  通过。忽略其他属性，仅断言 id 和 receiver.name 属性
```
每个子属性断言语句之间如果没有歧义，可以不用写`,`。

- #### 跳过属性值
结合`*`和`= {}`可以做到期望数据必须某个属性而忽略其值：
```javascript
    message= {
        id: *
        value: *
        receiver: *
    }
    //  也可以写成 = *
```

- #### 非空对象
DAL目前没有提供否定语义的断言支持。`!=` 仅是一般的逻辑运算符，并不具有`not =`的效果。因此 `null != null` 仅返回一个false的boolean值，并不会触发断言不通过的异常。要想达到 not null 的判定可以通过`: {}`来实现：
``` javascript
null: {}    // 失败
1: {}       // 通过
"": {}      // 通过
```

### 断言一个集合
与对象断言一样，DAL不支持定义集合，但是可以用`[]`的方式达到断言集合的效果，同样使用例更加直接和清晰。比如有如下的数据：
``` javascript
    [100, "hello", "world"]
```
那么如下的断言可以通过：
``` javascript
    = [100 'hello' 'world']     // [0]= 100 and [1]= 'hello' and [2]= 'world'
    : [/100/ 'hello' 'world']   // [0]: /100/ and [1]: 'hello' and [2]: 'world'
```

- #### 元素的默认/特定断言规则
`[]`前的`=`/`:`表示每个元素和对应位置的期望值进行断言的默认规则，同时也可以单独为某一个元素指定与之不同的断言规则：
``` javascript
    : [
        100
        = 'hello'
        = 'world'
    ]
```
注意，换行并不是DAL的语句分割符，可以使用`,`分割不同元素的期望值，每个期望值之间如果没有歧义，可以不用写`,`。

- #### 部分元素断言
如果仅对集合的前 n 个元素进行断言，则可以在 n+1 的位置写入`...`来终止集合断言：
``` javascript
    = [100 'hello' ...]     // 仅断言前两个元素
```
同样可以使用`*`跳过某个元素：
``` javascript
    = [100, *, 'world']       // 跳过第二个元素，这里使用逗号分隔，否则会解释为100 乘以 'world'
```

- #### 集合类型判定
可以使用`[...]`来判断数据是否是一个集合
```
    = [...]     // 判断是否是集合
    : [...]     // 与上式等效
```

- #### 元素映射
可以通过元素映射来简化某些断言语句，考虑如下数据：
``` json
{
    "lines": [{
        "product":  {
            "name": "iPad"
        }
    }, {
        "product":  {
            "name": "iPhone"
        }
    }, {
        "product":  {
            "name": "ITouch"
        }
    }]
}
```
断言表达式可以直接写成：
``` javascript
lines.product.name: ['iPad' 'iPhone' 'ITouch']
```

### is 断言
通过 `=` 和 `:` 直接书写的断言语句其实是用例的细节实现。对于某些大粒度的验收测试，可能并不太关心具体的值是多少，而是希望某些数据是某种具有业务意义的状态。对比如下的断言：
``` javascript
order: {
    status: 'PAID'
}
```
与
``` javascript
order is 已支付的订单
```
前者面向实现细节的判断，而后者则更面向业务的表达。
- #### Scheam
DAL 通过 `is`关键字，以及预定义的 Java 类型 Scheam 来实现刚才的效果。
首先定义Java类：
``` java
public class 已支付的订单{
    public String status = 'PAID'
}
```
然后通过 `registerSchema` 方法注册到DAL中，就可以引用了。
``` java
dal.getRuntimeContextBuilder().registerSchema(已支付的订单.class);
```
同时 `is` 语句后还可以跟随一个 `which` 子句，进行其他属性的断言：
``` javascript
order is 已支付的订单 which status: 'PAID'
```

注：which后只能接一条断言语句，如果需要多条语句，请使用 `and` 或 `,` 连接。

`which`后也可以跟随`=` `:`引导的断言表达式：
``` javascript
order is 已支付的订单 which: {
    status: 'PAID'
}
```
同时在此种场景也也可以不写`which`：
``` javascrpit
order is 已支付的订单: {
    status: 'PAID'
}
```

