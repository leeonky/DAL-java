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
                return stream(spliteratorUnknownSize((Iterator<String>) instance.keys(), Spliterator.NONNULL), false)
                        .collect(Collectors.toSet());
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
<T> T evaluate(Object input, String expression)
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
        "property": 1,
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
    property            // 与.property 等效
```

#### DAL支持的属性包括：
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

#### DAL把以下类型视为集合：
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
"hello".length  // "hello".length()
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

#### 运算符
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

## 数据断言

自动化测试的用例都应该是明确和可预知的，DAL只通符号（`=`和`:`）和`is`关键字，来对数据进行断言。
DAL可以进行与**值**、**正则匹配**、**对象**、**集合**有关的断言，也可以通过预定义具名 Schema 来断言。

``` java
new DAL().evaluateAll("hello", "= 'hello'");    // Pass!
new DAL().evaluateAll(1+1, "= 1+1");    // Pass!
```

### "AssertEqual"
- #### 严格断定（ `=` ）
判断值相等是测试中的最常见的断言。DAL通过`=`来达到此效果，它表示一种严格的断定，要求类型和值都相同。比如有如下的数据：
``` json
{
    "number": 2.0,
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
number: 2      // 数值相符，通过。
number: 2.0    // 数值相符，通过。
type: 'B'      // 值相符，通过
```
DAL在处理`:`断言时，如果比较对象都是数字，那么先提升某一个操作数的类型，然后再进行数值比较。如果是其他类型，则尝试通过内部`Converter`将输入值转换成比对值的类型，上例中`type`属性的断言就是将`enum Type`转换成 String 类型后再比较。
    
##### 注：DAL目前的版本实现下，`:`不允许在`Number` `String`和`Boolean`之间自动转换
    
``` javascript
'1': 1          //不通过
1: '1'          //不通过
true: 'true'    //不通过
```

##### 特别注意：DAL中只有 `=` 和 `:` 有断言效果，断言表达式在断言通过的情况下返回 true，不通过则直接抛出异常。`> < >= <= != && and || or` 这几个逻辑运算符不具有断言效果。执行他们会返回 true 或 false 结果。不会抛出 `AssertionFailure` 异常。

``` javascript
1 > 2       // 返回 false
1 != 1      // 返回 false
```
##### 同时 `!` 也不能和 `=` 与 `:`组合使用。断言数据都应是正面给出的确切期望值。


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
DAL中任何非 null 对象和 `null` 的`=`或`:`断言都不通过。同样的如果输入值是 `null`，除`*`和 `null` 外，其他任何期望值也都不通过。
```
null: null  // 通过
null= null  // 通过
null: *     // 通过
null= *     // 通过
```

### 断言一个数据对象
常见的测试框架在对数据对象断言时，都是先定义一个新对象，然后再通过各种策略与待测对象比较。DAL不支持定义对象。考虑如下输入数据：
``` json
{
    message: {
        "id": 1,
        "value": "hello James",
        “receiver": {
            "id": "007",
            "name": "James"
        }
    }
}
```
我们当然可以用现有DAL方式断言：
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
结合`*`和`= {}`可以做到期望数据必须含有某个属性而忽略其值：
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
`[]`前的`=`和`:`表示每个元素和对应位置的期望值进行断言的默认规则。除此之外，也可以单独为某一个元素指定与之不同的断言规则：
``` javascript
    : [
        100
        = 'hello'
        = 'world'
    ]
```
注意，换行并不是DAL的语句分割符，可以使用`,`分割不同元素的期望值，期望值之间如果没有歧义，可以不用写`,`。

- #### 部分元素断言
如果仅对集合的前 n 个元素进行断言，则可以在 n+1 的位置写入`...`来终止集合断言：
``` javascript
    = [100 'hello' ...]     // 仅断言前两个元素
```
同时也可以对末尾若干个元素进行断言：
``` javascript
    = [... 'hello' 'world']     // 仅断言最后两个元素
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
通过 `=` 和 `:` 直接书写的断言语句其实是用例的细节实现。对于某些大粒度的验收测试，可能并不太关心具体的值是多少，而是希望某些数据是某种具有业务意义的状态。比如一个API返回如下的数据：
``` json
{
    "order": {
        "id": "001",
        "status": "PAID",
        "paymentData": {
            "amount": "100",
            "status": "DONE"
        }
    }
}
```
可以通过如下的断言测试其已经支付的状态和金额：
``` javascript
order: {
    status: 'PAID'
    paymentData: {
        amount: 100
        status: 'DONE'
    }
}
```
这个测试用例体现了已支付状态的细节，即 status 为 'PAID'，并且 paymentData.status 为 'DONE'。如果我们不关系这个细节或者在这层细节之上抽象出一个业务层，并将其描述“已支付的订单”：
``` javascript
order is 已支付的订单
```
如此以来，这个用例更能接近业务需求的描述，而非对程序实现的测试。

- #### Scheam
DAL 通过 `is`关键字，以及预定义的 Java 类型 `Scheam` 来实现刚才的效果。首先定义 `Schema` 类：
``` java
public class 已支付的订单{
    public String status = 'PAID';
    public 已支付的支付记录 paymentData;
}

public class 已支付的支付记录{
    public Formatters.Number amount;
    public String status = 'DONE';
}
```
然后通过 `registerSchema` 方法注册到DAL中，就可以引用了。
``` java
dal.getRuntimeContextBuilder().registerSchema(已支付的订单.class);
```
同时 `is` 语句后还可以跟随一个 `which` 子句，进一步验证其他属性：
``` javascript
order is 已支付的订单 which paymentData.amount: 100
```

注：which后只能接一条断言语句，如果需要多条语句，请使用 `and` 或 `,` 连接。当然，如果要断言多个属性，`which`后可以直接跟随`=` `:`引导的断言表达式：
``` javascript
order is 已支付的订单 which: {
    paymentData.amount: 100
}
```
同时在此种场景也也可以不写`which`：

``` javascrpit
order is 已支付的订单: {
    paymentData.amount: 100
}
```

- 属性值和类型

在开始定义 `Schema` 的例子中，`public String status = 'PAID'` 表示数据包含属性 status，其值为 String 类型的 PAID。 
属性 amount 使用了 `Formatters.Number`，表示数据会出现一个Number类型的`Formatter`，有关 `Formatter` 将在后边描述。除了 `Formatter` 之外还可以使用 `Type` 和 `Value` 两个接口：
``` java
public interface Type<T> {
    boolean verify(T value);
    String errorMessage(String field, Object actual);
}

public abstract class Value<T> {
    public abstract boolean verify(T actual);
}
```

这两个接口都是为了在Schema中描述属性的限制信息。比如：
``` java
public class 已支付的支付记录{
    public Value<Integer> amount = Value.greaterThan(0);    // 表示 amount 必须是大于 0 的 Integer
    public String status = 'DONE';
}
```
`Type` 会比较类型和值，而 `Value` 不进行类型比较。

- 部分验证

前述的 Schema 即验证数据，又验证数据格式，类似 `= {}`，要求Schema 中罗列的属性都应出现在结果中。如果不关心其他属性，可以使用 `@Partial` 注解，只验证 Schema 中出现的属性：

``` java
@Partial
public class 已支付的支付记录{
    public String status = 'DONE';
}
```
如果属性值有可能为null，可以在属性上加注 `@AllowNull` 注解

- 属性别名

刚才的实例中验证金额的部分是通过 paymentData.amount 来判断的，从某种程度上讲，这也体现了实现细节。为了能更直接表述“支付金额”这个业务名词，DAL提供了属性别名来抽象之，具体做法是在 Order 的 Schema 定义中用 `@FieldAlias` 定义别名：
``` java
@FieldAliases({
    @FieldAlias(alias = "支付金额", field = "paymentData.amount"),
})
public class 已支付的订单{
    public String status = 'PAID';
    public 已支付的支付记录 paymentData;
}
```
然后在断言中使用：
``` javascript
order is 已支付的订单 which 支付金额: 100
order is 已支付的订单: {支付金额: 100}
```
属性别名可以定在 `Schema` 实现的接口或基类中：
``` java
@FieldAliases({
    @FieldAlias(alias = "支付金额", field = "paymentData.amount"),
})
public interface 订单 {
}

public class 已支付的订单 implements 订单{
    public String status = 'PAID';
    public 已支付的支付记录 paymentData;
}
```

- #### Formatter

`is` 后除了可以使用 Schema 外还可以使用 `Formatter`，`Formatter` 在 JSON 验证中有一定作用。其主要用意是将一种输入数据，转换为另一种类型，然后对转换后的值进行断言。JSON 中的数值类型有限，如果用JSON返回一个时间数据，常用做法是用字符串描述时间：
``` json
{ "time": "2001-10-11T01:00:00" }
```
对 time 断言时，如果只匹配时间的部分值。那么只能使用比较晦涩的正则表达式。但如果使用`Formatter`，则可以先将其转换为一个时间类型（比如 LocalDateTime），然后再获取需要的属性进行断言：
``` javascript
time is LocalDateTime which year= 2001
```
DAL内置的 `Formatter` 都在 `Formatters`中定义。

- #### 在对象和集合断言中使用 `is`
在对象与集合断言时也可以通过 `is` 指定 `Schema`:
``` javascript
= {
    response is 已支付的订单: {
        支付金额: 100
    }
}
```
在集合中使用 `is`
``` javascript
: [is 已支付的订单: {
        支付金额: 100
    }]
```
在对象与集合中不能添加 `which` 关键字

