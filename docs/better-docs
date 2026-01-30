# BETTER HASKELL DOCUMENTATION

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
-- 注意这里的(chineseString str) 即模式匹配, 即将str绑定到参数内部的string上
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
