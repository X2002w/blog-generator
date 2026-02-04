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
