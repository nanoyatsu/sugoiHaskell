-- >ghci baby.hs
-- :quit で終わる
-- :l baby.hs でロードする
-- :r で今よんでるやつをリロードする

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
    then x
    else doubleMe x

lostNumbers = [4,8,15,16,23,42]
iroiroNumbers = lostNumbers ++ [1,2,3,5,6,7,31]
smallcat = 'A' : " SMALL CAT"

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

nouns = ["hobo","frog","pope"]
adjectives = ["lazy","grouchy","scheming"]

length' xs = sum [1 | _ <- xs]
removeNonUppercast st = [c | c <-st, c `elem` ['A'..'Z']]

xxs = [
    [ 1, 3, 5, 2, 3, 1, 2, 4, 5],
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9],
    [ 1, 2, 4, 2, 1, 6, 3, 1, 3, 2, 3, 6]
    ]
evenInList = [[x|x<-xs,even x]|xs<-xxs]

-- 1.6.3 直角三角形をみつける
-- * 3辺の長さはすべて整数である
-- * 各辺の長さは10以下である
-- * 周囲の長さは24に等しい
triples = [(a,b,c)|a<-[1..10],b<-[1..10],c<-[1..10],a+b+c == 24,a*a+b*b==c*c]
triples' = [(a,b,c)|c<-[1..10],a<-[1..c],b<-[1..a],a+b+c == 24,a*a+b*b==c*c]

-- 第2章
-- なんか:tをした

-- 型クラス（！）
boundLim = maxBound :: (Bool,Int,Char) 

-- `=>` 型クラス制約 `=>`の前に書いてあるインターフェースを見たすのが条件

-- *Main> :t boomBangs
-- boomBangs :: Integral a => [a] -> [[Char]]
-- *Main> :t odd
-- odd :: Integral a => a -> Bool
-- Integralなんやねん⇨整数(Int,Integer) fromIntegral

-- 第3章

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe 1 = "iti"
sayMe 2 = "ni"
sayMe 3 = "san"
sayMe x = "ikutu"

-- if的なpattern使った再帰
factorial 0 = 1
factorial n = n * factorial (n-1)

-- tuple
first (x, _, _) = x
second (_, y, _) = y
third (_, _, z) = z

-- list
head' [] = error "nanmonaiyo"
head' (x:_) = x

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
girstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
-- asパターンなるもの　-> xsを参照しながらhead::tailを使える

-- ガードなるもの
bmiTell bmi　-- ここにイコールはいらねえ
    | bmi <= 18.5 = "yaseteruyan"
    | bmi <= 25.0 = "eekarada yan"
    | bmi <= 30.0 = "hutotteru yan"
    | otherwise = "debbu..."
-- パイプを使う場合のパターンの書き方らしい いつものやんけ
-- というか元の書き方がシャドーイング(先勝ちだけど)かオーバーロード(シグネチャ同じだけど)みたいなものに見えすぎた

-- where とかいうもの
bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | bmi <= skinny = "yaseteruyan"
    | bmi <= normal = "eekarada yan"
    | bmi <= fat = "hutotteru yan"
    | otherwise = "debbu..."
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0
-- 変数宣言やんけ！（まあ構文はこうらしい、ということで）
-- スコープは関数内
            -- 「whereの束縛の中でもパターンマッチを使うことができます。BMI関数のwhere節を次のように書いてもかまいません。」⇩
            -- (skinny, normal, fat) = (18.5, 25.0, 30.0)
            -- らしいんだけど、これパターンマッチ？なの？か？

-- let
-- 俺たちのletきたな感がある
-- 「whereは関数の終わりで変数を束縛し、その変数はガードを含む関数全体から見えます。」
-- 「対してlet式は、どこでも変数を束縛でき、そしてlet自身も式になります。
--   しかし、let式が作る束縛は局所的で、ガード間で共有されません。」
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea
-- 「inとは」と思ったけど、F#のそれとは大きめに違う。ぽい。
-- `let bindings in expression`らしい。式というのも納得をした。

-- リスト内包表記でのlet
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi|(w,h) <- xs, let bmi = w/h^2, bmi > 25.0]
-- 「letで定義された名前は、出力とそのletより後ろのリスト内包表記のすべてから見えます」
-- 「`(w,h) <- xs`の部分はジェネレータと呼ばれます」（唐突）（リスト内包表記の初出の時じゃあかんかったんかなこれ）

-- case exp
-- case expression of pattern -> result
--                     pattern -> result
--                     pattern -> result
-- こっちのが冗長だしいらんくない？（過激派）
-- ⇨「引数によるパターンマッチが使えるのは関数を定義するときだけですが、case式はどこでも使えます」hai...

-- 再帰
maximum' :: Ord a => [a] -> a
maximum' [] = error "arg is empty"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
-- replicate' 0 _ = []
-- replicate' n x = x:(replicate' (n-1) x)
replicate' n x 
    | n <= 0 = []
    | otherwise = x:(replicate' (n-1) x)
-- patternで簡単に書こうとすると条件が甘くて n = 負で死ぬ

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x:(take' (n-1) xs)

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem` xs

quickSorted = [5,1,9,4,6,7,3]
quickSort' [] = []
quickSort' (x:xs) =
    let smaller = [a | a <- xs , a <= x]
        larger = [a | a <- xs , a > x]
    in (quickSort' smaller) ++ [x] ++ (quickSort' larger)

-- 第5章 : 高階関数

-- carry化
multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

-- 「カリー化された関数」引数たくさんとれるやつ側
-- 「部分適用」引数ちょっとだけ与える側

-- 高階関数
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- zipWith
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- flip
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

-- 5.3
-- map
-- filter 略
-- 「さらなる例」「10万以下の数のうち3829で割り切れる最大の数」
largestDivisivle :: Integer
largestDivisivle = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0
-- コラッツ列
korattu :: Integer -> [Integer]
korattu 1 = [1]
korattu n
    | even n = n : korattu (div n  2)
    | odd  n = n : korattu (n*3+1)

hyakuikano15yorinagaikorattu = length (filter (\elem->length elem > 15) (map korattu [1..100]))

-- 5.4
-- lambda
-- 「カリー化と部分適用の動作がよくわかってないと、必要ないところでラムダ式を使いがち」
mudanalambda  = map (+3) [1,6,3,2]
mudanalambda' = map (\x -> x + 3) [1,6,3,2]

-- 「個人的にflip関数は次のように定義するのが一番読みやすいと思います」
flipLambda :: (a -> b -> c) -> b -> a -> c
flipLambda f = \x y -> f y x

-- ここまで6/16
-- ここから8/29 2ヶ月以上空いてしまった

-- 畳み込み（わかる）
goukei xs = foldl (\acc x -> acc + x) 0 xs
-- ようしらんけど初手アキュムレータなのやたら馴染みあるな

-- 「畳み込みで実際に何が起こっているのかを～」飛ばせる内容そう むかし実装したね
-- 右畳み込み これもok

-- foldl1 foldr1 (なにそれは)
-- これはリストの最大値を選ぶやつ 端から大きい方(max)を選び続けるの図 かしこい
foldMaximum' :: (Ord a) => [a] -> a
foldMaximum' = foldl1 max
-- 初期値を先頭とか末尾とかから取るやつをそう言うらしい メチャクチャ見覚えがある（pythonとかで見たっけ）
-- 「畳込みを作る際には空リストに対する動作について考えましょう。空リストに対しては意味を成さない関数なら、foldl1やfoldr1を使える可能性があります。」

-- 5.5.4 畳み込みの例 いろいろ
foldReverse' :: [a] -> [a]
foldReverse' = foldl (\acc x -> x : acc) [] -- 次のやつをどんどん左側に付け加えていく ひっくり返るね
foldProduct' :: (Num a) => [a] -> a
foldProduct' = foldl (*) 1 -- 掛け算 初期値1 全部かける
foldFilter' :: (a -> Bool) -> [a] -> [a]
foldFilter' p = foldr (\x acc -> if p x then x : acc else acc) [] -- pの結果を見て初期値空リストに付け足していく
-- いくつか書いて（思い出しつつ）思ったけれど、登場する関数(ここだとfold系)が持ってる引数の数によって、登場しない引数に関しては暗黙的に名前もついてないのむずいな なんか理解が足りていない？
foldLast' :: [a] -> a
foldLast' = foldl1 (\_ x -> x)
-- 第一引数が何で第2引数が何なのかっていうのを手足のように感じたい

-- 5.5.5 別の視点から見た畳み込み
-- 2引数fと[3,4,5,6]と継続初期値zの右畳み込み
-- f 3 (f 4 (f 5 (f 6 z)))
-- 2引数gと[3,4,5,6]と継続初期値zの左畳み込み
-- g (g (g (g z 3) 4) 5) 6
-- らしいんだけど、この2行はかなりわかりやすいと思うんだけど、初期値の引数の順番かわるもの？？なのかしら？？ という気持ちになった 本質的な話じゃないんだけれど 習慣的にはそう？
-- g 6 (g 5 (g 4 (g 3))) -- といわけで書いてみたけれど、数字が逆並びになるしキモくない？ってレベルの話かな

-- 5.5.6　無限リストの畳み込み
-- 「畳み込みを一連の関数適用として考えると、foldrが無限リストに対して完璧に正しく動作する理由が見えてきます」

