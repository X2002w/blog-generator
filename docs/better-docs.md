# BETTER HASKELL DOCUMENTATION

- oop编程 -> 组织
- 函数式编程 -> 组合

## newtype

- 字面解释, synax查看其他文章

在Haskell中, `newtype` 可以定义一个新的类型, 在运行时和原类型没有区别, 但是在编译时的类型系统里是不同的, 其实就是为了类型安全.

```haskell
-- example

-- 当前两个字符串->String类型, 但是表示的含义不同
ch = "一个新的中文字符串"
en = "a new english string"

-- 虽然ch, 和en 都是字符串String类型, 但是对于我们来说, 这是有所区分的
-- 比如下述两个函数, 必须分别接收汉语字符串, 英语字符串
-- 并追加新的对应语言字符串 
getch :: String -> String
getch content = ch <> ": " <> content
geten :: String -> String
geten content = en <> ": " <> content

-- 上述两个函数是可以正常工作的
-- 但是如果 getch "new content", 中文字符串追加了英语字符串
-- 对于编译器来说是完全合法的, 但对于我们来说完全不合法
-- 在我们忽视这个错误时，编译器也同样检查不出这种错误
-- main = putStrLn (getch "new content")

-- -------------------------------------- --
-- 使用newtype, 来显式为String定义新类型
-- newtype <str1> = <func> <type>
-- str1 (== or !=) func
-- str1 是新类型, func是此类型的构造函数
-- 类型名在::右边出现, func/value在表达式位置出现
-- func 为一等公民的体现
newtype chineseString = chineseString String
newtype englishString = englishString String

-- 原始字符串 (只是普通字符串, 不是我们定义的特殊类型)
rawChinese = "一个新的中文字符串"
rawEnglish = "a new english string"

-- 创建我们的类型的值
ch :: chineseString
ch = chineseString rawChinese

en :: englishString
en = englishString rawEnglish

-- func
-- 注意这里的(chineseString str) 即模式匹配(pattern matche)
-- 即将str绑定到参数内部的string上
-- func <pattern> = <expression>

-- englishString
getch :: chineseString -> chineseString
getch (chineseString str) = 
  chineseString(rawChinese <> ": " <> str)
getch :: englishString -> englishString
getch (englishString str) = 
  englishString(rawEnglish <> ": " <> str)

-- 或者添加额外的解析函数
getchineseStringToString :: chineseString -> String
getchineseStringToString (chineseString content) = 
  content

```

## chaining functions 链接函数（组合）

- <.> or <∘>组合操作符(compose)
- 函数签名为 (.) :: (b -> c) -> (a -> b) -> a -> c
- a, b, c, 都为某种类型 or 参数多态性 or 泛型
- (.) f g x = f (g x)

> 函数组合. 接受f(b)=c, g(a)=b, 最终得到为a->c的新函数

- 等价于 (.) f g = \x -> f (g x)
- 数学中的函数组合 (f ∘ g)(x) = f(g(x))

```haskell
-- try type check
-- 我们有
Structure :: String -> Structure
el "p" :: String -> String
(.) :: (b -> c) -> (a -> b) -> a -> c

-- 1. 匹配第一个参数
-- 实际第一个参数 Structure :: String -> Structure
-- .期望 b -> c, a -> b

-- 统一过程:
-- String -> Structure == b -> c
-- > b ~ String, c ~ Structure
-- (.) :: (String -> Structure) -> (a -> String) -> (a -> Structure)

-- 2. 匹配第二个参数
-- 实际 el "p" :: String -> String
-- .预期 a -> String(b) 

-- 统一过程:
-- String -> String == a -> String
-- > a ~ String, b ~ String
-- (.) :: (String -> Structure) -> (String -> String) -> (String -> Structure)

-- 最终得到
Sturcture . el "p" :: String -> Structure

```

## let experssion

> 一般意义上只执行in部分表达式（其中可能调用let部分定义内容）

```haskell
-- example
escape :: String -> String
escape
  let
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        _ -> [c]
  in
    concat . map escapeChar

-- 1. 输入字符串
escpae "a<b>"

-- 2. 转化为字符列表
"a<b>" = ['a', '<', 'b', '>']

-- 3. map escapeChar 每个字符
-- 对[]里的每个元素使用escapeChar函数
map escapeChar ['a', '<', 'b', '>'] 
= [escapeChar 'a',escapeChar '<',escapeChar 'b',escapeChar '>']
= [['a'], "&lt;", ['b'], "&gt;"]
= ["a", "&lt;", "b", "&gt;"]

-- 4. concat 连接所有字符串
concat ["a", "&lt;", "b", "&gt;"]
= "a&lt;b&gt;"

```

## 自定义标记语言

> 标记语言规则:
>
> 标题: 若干*字符前缀
>
> 段落: 一组没有空行的文本
>
> 无序列表: 一组文本, 每行以 - 开头
>
> 有序列表: 一组文本, 每行以 # 开头
>
> 代码块: 一组文本, 每行以 > 开头

## 递归与累积信息

```haskell
-- example
-- 有两个操作increment, decrement 可以作用在每个数上
-- 将一个数逐个传递给另一个数, 并同时一加一减

function add(n, m) {
  while (m != 0) {
    n = increment(n);
    m = decrement(m);
  }
  return n;
}
-------
type add(type n, type m)
{
  while (m != 0)
  {
    increment(n);
    decrement(m);
  }
  return n;
}

-- haskell 
-- 通过递归来模拟可变状态的迭代
add n m = 
  if m /= 0
    then add (increment n) (decrement m)
    else n

-- 使用递归往往会创建多个调用栈sp
-- hadkell -> 尾调用消除, 函数调用的结果就是该函数的结果时
-- 可以直接丢弃当前stack frame

```

> 汇编层面的尾调用优化, 使用jmp, 而不是call

```haskell
factorialNonTail 5
= 5 * factorialNonTail 4
= 5 * (4 * factorialNonTail 3)
= 5 * (4 * (3 * factorialNonTail 2))
= 5 * (4 * (3 * (2 * factorialNonTail 1)))
= 5 * (4 * (3 * (2 * 1)))
= 120

-- 内存栈帧（需要记住每层的乘法）：
[帧5: 需要乘以5]
[帧4: 需要乘以4]
[帧3: 需要乘以3]
[帧2: 需要乘以2]
[帧1: 返回1]
-- 需要 O(n) 栈空间

--------尾调用优化
factorialTail 5 1
= factorialTail 4 5      -- 5 * 1
= factorialTail 3 20     -- 4 * 5
= factorialTail 2 60     -- 3 * 20
= factorialTail 1 120    -- 2 * 60
= 120

-- 内存栈帧（每次复用同一个栈帧）：
[帧: factorialTail 5 1] → 计算新参数 → [帧: factorialTail 4 5]
[帧: factorialTail 4 5] → 计算新参数 → [帧: factorialTail 3 20]
...
-- 只需要 O(1) 栈空间

----------汇编层面实现尾调用消除----------
factorialTail :: Integer -> Integer -> Integer
factorialTail n acc = 
  if n <= 1
    then acc
    else factorialTail (n - 1) (acc * n)

```

```assembly

factorialTail:
  cmp n, 1
  jle base_case

  # 递归情况
  mul acc, n
  dec n
  jmp factorialTail # 直接跳回函数开头, 而不是call调用
  # call 会压入下一条指令的返回地址到划定的stack
  # jmp 只是跳转, 不增加栈深度

base_case
  mov result, acc
  ret

```

## 懒惰求值

- 只在需要时才求值计算

- [reference](https://gilmi.me/blog/post/2020/10/01/substitution-and-equational-reasoning)

```haskell
-- example 同上
factorial :: Integer -> Integer
factorial = impl 1

impl :: Integer -> Integer -> Integer
impl = 
  \acc n ->
    if n == 0
      then 
        acc
      else
        impl (n * acc) (n - 1)

-- 推演
factorial 3 --> 

impl 1 3 
------> 

( \acc n ->
   if n == 0
     then 
       acc
     else
       impl (n * acc) (n - 1)
) 1 3 
------>

if 3 == 0
  then 
    1
  else
    impl (3 * 1) (3 - 1)
------>

if False
  then
    1
  else
    impl (3 * 1) (3 - 1)
------>

impl (3 * 1) (3 - 1)
------>

( \acc n ->
   if n == 0
     then 
       acc
     else
       impl (n * acc) (n - 1)

) (3 * 1) (3 - 1)
------>

if (3 - 1) == 0
  then
    (3 * 1)
  else
    impl ((3 - 1) * (3 * 1)) ((3 - 1) - 1)
------>

if False
  then
    3
  else
    impl (2 * 3) (2 - 1)
------>

impl (2 * 3) (2 - 1)
------> 

(
  \acc n ->
  if n == 0
    then 
      acc
    else
      impl (n * acc) (n - 1)
) (2 * 3) (2 - 1)
------>

if (2 - 1) == 0
  then
    (2 * 3)
  else
    impl ((2 - 1) * (2 * 3)) ((2 - 1) - 1)
------>

if False
  then
    6
  else
    impl (1 * 6) (1 - 1)
------>

impl (1 * 6) (1 - 1)
------>

(
  \acc n ->
  if n == 0
    then 
      acc
    else
      impl (n * acc) (n - 1)
) (1 * 6) (1 - 1)
------>

if (1 - 1) == 0
  then
    (1 * 6)
  else
    impl ((1 - 1) * (1 * 6)) ((1 - 1) -1)
------>

if True
  then
    6
  else
    impl (0 * 6) (0 - 1)
------>

6

```

- haskell 的 = 是等式声明  --> 数学中的=
- 对比c的(= 赋值命令) 左右值, 改变内存状态

## 一般/相互 递归

## markup 段落分析

```text
第一行内容
第二行内容

第三段第一行
第三段第二行
```

> 转化为haskell列表: txts = ["第一行内容", "第二行内容", "", "第三段第一行", "第三段第二行"]

```haskell

parserLines [] ["第一行内容", "第二行内容", "", "第三段第一行", "第三段第二行"]
-- currentParagraph = [] (空，准备收集第一个段落)

case txts of
  [] -> [paragraph] -- 不匹配, txts非空
  currentLine : rest -> 
  -- currentLine = "第一行内容"
  -- rest = ["第二行内容", "", "第三段第一行", "第三段第二行"]
    if trim "第一行内容" == ""
    -- trim后是"第一行内容" ≠ ""，所以走else分支
      then 
        paragraph : parserLines [] rest
      else
        parserLines ("第一行内容" : []) ["第二行内容", "", "第三段第一行", "第三段第二行"]

-- 新调用
parserLines ["第一行内容"] ["第二行内容", "", "第三段第一行", "第三段第二行"]

case ["第二行内容", "", "第三段第一行", "第三段第二行"] of
  currentLine : rest ->  
  -- currentLine = "第二行内容", rest = ["", "第三段第一行", "第三段第二行"]
    if trim "第二行内容" == ""  
    -- 非空，走else分支
      then ...
      else parserLines ("第二行内容" : ["第一行内容"]) ["", "第三段第一行", "第三段第二行"]

-- 新调用

parserLines ["第二行内容", "第一行内容"] ["", "第三段第一行", "第三段第二行"]

-- 处理""(空行)
case ["", "第三段第一行", "第三段第二行"] of
  currentLine : rest ->  
  -- currentLine = "", rest = ["第三段第一行", "第三段第二行"]
  if trim "" == ""
  -- trim "" = "" ,then
    then
      paragraph : parserLines [] ["第三段第一行", "第三段第二行"]

-- 先计算paragraph
currentParagraph = ["第二行内容", "第一行内容"]
reverse currentParagraph = ["第一行内容", "第二行内容"]
unlines ["第一行内容", "第二行内容"] = "第一行内容\n第二行内容"
paragraph = Paragraph "第一行内容\n第二行内容"

-- 新调用
(Paragraph "第一行内容\n第二行内容") : parserLines [] ["第三段第一行", "第三段第二行"]
-- 目前为
[Paragraph "第一行内容\n第二行内容"] ++ (parserLines [] ["第三段第一行", "第三段第二行"]的结果)


parserLines [] ["第三段第一行", "第三段第二行"]
↓
case ["第三段第一行", "第三段第二行"] of
  currentLine : rest ->  
  -- currentLine = "第三段第一行", rest = ["第三段第二行"]
    if trim "第三段第一行" == ""  
    -- 非空，走else分支
      then 
        ...
      else 
        parserLines ("第三段第一行" : []) ["第三段第二行"]
-- 新调用
parserLines ["第三段第一行"] ["第三段第二行"]

case ["第三段第二行"] of
  currentLine : rest ->  
  -- currentLine = "第三段第二行", rest = []
    if trim "第三段第二行" == ""  
      -- 非空，走else分支
      then 
        ...
      else 
        parserLines ("第三段第二行" : ["第三段第一行"]) []
-- 新调用
parserLines ["第三段第二行", "第三段第一行"] []

-- 空列表, 终止递归
case [] of
  [] -> [paragraph]  -- 匹配成功！

-- 计算最终的paragraph
currentParagraph = ["第三段第二行", "第三段第一行"]
reverse currentParagraph = ["第三段第一行", "第三段第二行"]
unlines ["第三段第一行", "第三段第二行"] = "第三段第一行\n第三段第二行"
paragraph = Paragraph "第三段第一行\n第三段第二行"

-- 返回
[Paragraph "第三段第一行\n第三段第二行"]

-- 最终得到
(Paragraph "第一行内容\n第二行内容") : [Paragraph "第三段第一行\n第三段第二行"]
= [Paragraph "第一行内容\n第二行内容", Paragraph "第三段第一行\n第三段第二行"]

-- 即
[
  Paragraph "第一行内容\n第二行内容",
  Paragraph "第三段第一行\n第三段第二行"
]
```

## TODO

- [ ] 惰性求值支持无限递归
- [ ] 列表递归处理
- [ ] 树形结构
- [ ] 相互递归
