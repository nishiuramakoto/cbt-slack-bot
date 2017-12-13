What is this?
=============

This is a simple slack bot using Yampa, a Functional
Reactive Programming library in Haskell. The idea is to
define a regex-like DSL for switching signal functions.
This allows short, readable definitions of complex
finite state machines that change their behaviors across time.

Instructions for testing
------------------------

1. Create a Slack workspace and/or channel for testing.
2. Get a [slack legacy api token](https://api.slack.com/custom-integrations/legacy-tokens) for that channel.
3. Edit `apiKey` in `src/Main.hs`.
4. `stack setup && stack build && stack exec`

YampaでFunctional Reactiveな認知行動療法ボットを書く
====================================================

これは[Haskell Advent Calendar
その3](https://qiita.com/advent-calendar/2017/haskell3 "Haskell Advent Calendar 3")、
13日目の記事です。前日の記事はmatsubara0507さんの [Haskell
Backpack覚え書き](https://matsubara0507.github.io/posts/2017-12-12-backpack-memo.html "Previous article")でした。

はじめに
--------

[Functional Reactive Programming](http://conal.net/papers/icfp97/ "Functional Reactive Animation")(FRP)
を使うと、時間軸に沿って入出力の関係が変
化するシステムの挙動を、シンボリックに記
述し、組み合わせることができます。ロボッ
ト、ゲーム、GUIなどのプロラミングに便利
です。[Yampa](https://wiki.haskell.org/Yampa#External_Links "Yampa wiki")は、
[Arrow](https://wiki.haskell.org/Arrow#External_links "Arrow wiki")の記法を
フル活用するFRPのライブラリです。

認知行動療法(Cognitive Behaviour Therapy, CBT)は、医学的に確
立された心理療法の一種です。ここでは、CBTの自己対話スキー
ムを提供するボットを、Yampaを使って書いてみます。

目標
----

時間と入力によって変化するボットの挙動を、 正規表現のようなDSL(Domain
Specific Language) で表せると便利です。状態変数を直接扱うと、
コードが見にくくなったり、少しの挙動の変更が
状態変数の意味や数に大きな変更を生じたりするからです。

Yampaでそのような正規表現的状態機械DSLを
書いて、CBTボットのコーディングに使ってみます。

## このCBTボットで何ができるか

ここで実装するCBTボットは人工知能ではなく、自己対話のための
ガイダンスが目的です。ユーザーの指示にしたがって、必要な
情報を表示し、記録し、後に分析を行う機能を持つことがゴー
ルです。FRPで定義するボットの挙動は完全にピュアでIOと独立であるため、
後の機能追加が用意であることが利点です。

YampaとArrowised Functional Reactive Programming(AFRP)
------------------------------------------------------

FRPは主に二つの概念によってなりたっています。

1.  時間tにそって連続的に変動するシステムの*挙動*
2.  *イベント*の時系列`[ev(t0), ev(t1), ... ]`

この二つをシンボリックに組み合わせ、より複雑なシステムを記述して
いく技法が、*Functional Reactive Programming*です。

FRPには次のような利点があります。

1.  単純なシステムをシンボリックに組み合わせ、より複雑なシステムを記述する、
    関数言語の方法論が使える。(例: `sf1 >>> sf2`)
2.  システムを入出力系と独立に定義できる。(e.g. stdio, http, websocket,
    GUI..)

欠点としては、特にデフォルトで遅延評価を
行うHaskellの場合、パフォーマンスやリー
クの分析が難しいことがあげられます。FRP
で起きるリークは、スペースリークとタイム
リークに分けられますが、Yampaでは、時間t
を直接露出せず、*シグナル関数*を操作する
APIを提供することによって、タイムリーク の問題を緩和しています。

Yampaを構成する概念は主に次の四つです。[^1]

1.  時間`Time`。システムが動きだした時を0とする。
2.  シグナル `Signal a = Time -> a`。値`a`を取る`Time`の関数。
3.  シグナル関数 `SF a b = Signal a -> Signal b`。
    シグナルをシグナルへ写す、高階関数。
4.  イベント `Event a = NoEvent | Event a`。`Maybe`と同型。
    シグナル関数から別のシグナル関数に*スイッチ*するときに使う。

シグナル`Time->a`の直接操作はタイムリー
クを起しやすいので、シグナル関数を`Arrow`として抽象化し、シグナルを
直接扱わないようにするというのがYampaの考え方です。
もし時間を直接操作できるようにすると、
「昔の記憶」がどこかに残っていれば、その記憶につながった他のデー
タも全てリークしてしまう可能性があります。

Yampaでは過去を参照する操作
(e.g. integral, differential, delay,
etc.)は限定され、タイムリークをおこしにくいよう設計されています。

シグナル関数は自然に`Arrow`インスタンス
を持ちます。時間とシグナルを隠すYampaの
のArrowプログラミングは、「状態」を隠す
`IO`モナドプログラミングの一般化と見るこ
とができます。(ArrowとMonadの相違については、Lugendreさんの
[Advent記事](https://qiita.com/Lugendre/items/6b4a8c8a9c85fcdcb292 "Lugendre Arrow")がわかりやす
いです)

状態機械DSLをYampaで定義する
----------------------------

基本的なYampaの使い方については、こちらの
[スライド](http://www.cs.yale.edu/homes/hudak/CS429F04/LectureSlides/YampaForCs429.ppt
"Yampa Slide")がわかりやすいと思います。

ここでは、時間そしてイベントごとに状態を変えるロボットを、簡
単に記述するためのDSLを考えてみましょう。

Yampaでシステムの状態を操作するには、次の方法があります。

1.  `loopPre :: c -> SF (a,c) (b,c) -> SF a b`

  状態変数をフィードバックする

2.  スイッチ[^2]

    イベントによってシグナル関数の変遷を定義する

         -- *Switchとd*Switchの違いはスイッチが起きた瞬間、スイッチ前と後のどちらの出力を使うか
         -- d(==delayed)はスイッチ後の出力を使う

         -- Switch once and for all
         switch   :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
         dSwitch  :: SF a (b, Event c) -> (c -> SF a b) -> SF a b

         -- Recurrent switches
         rSwitch  :: SF a b -> SF (a, Event (SF a b)) b
         drSwitch :: SF a b -> SF (a, Event (SF a b)) b

         -- Switch with continuation
         kSwitch  :: SF a b -> SF (a, b) (Event c) -> (SF a b -> c -> SF a b) -> SF a b
         dkSwitch :: SF a b -> SF (a, b) (Event c) -> (SF a b -> c -> SF a b) -> SF a b

1.の状態変数を直接使った記述は、複雑な状
態機械の記述にはあまり向きません。単純な
正規表現や生成文法から、大量の状態変数が
必要になることはよくありますし、少しの文
法の変更が状態機械の大きな変更を強いることもあります。

ここでは、スイッチを使いシグナル関数の正規
表現コンビネータを定義してみます。

    type UnitEvent = Event ()
    type ESF i o a b = i -> SF a (b, Event o)
    type UnitSF a b  = ESF () ()

    -- | Sequential associative composition along the Event timeseries (similar to regexp "ab")
    dStep :: ESF e e a b -> ESF e e a b -> ESF e e a b
    dStep x y e = x' `dSwitch` y
        where
            x' = x e >>> arr (flip (,) NoEvent) *** identity

    -- | Choice operator -- "a|b"
    dAlt  :: ESF il ol a b -> ESF ir or a b -> ESF (Either il ir) (Either ol or) a b
    dAlt x y e = case e of
        Left  il -> x il >>> identity *** arr (fmap Left)
        Right ir -> y ir >>> identity *** arr (fmap Right)

    -- | Non-empty Kleene's operator, "a+"
    dPlus :: ESF e e a b -> ESF e e a b
    dPlus x = x `dStep` dPlus x

たったこれだけのことで、ロボットの動きを正規表現的に記述できるようになりました。
("空"ロボットを許すと話は少し複雑になりますが、ここでは省き
ます。)

関数`Time -> a`のグラフをイメージしてみ
ると、シグナル関数のArrowインスタンスは、
値`a`軸の方向のデータフローを定義します。
一方、上で定義した正規表現コンビネータは、
時間軸方向のシステムの変化を定義します。
組み合わせると、複数の正規表現を並列に動
かし、どれかを取り出したり、重ね合せたり
することもできます。例えば、通常システム
と緊急時システムを同時に動かし、状況によっ
ていずれかの出力を選ぶことができます。

(以下自問です。答をお持ちの方は教えてください)

Ex.1 delayedでないバージョン `step`, `alt`, `plus` を定義してください。
どんな場合にビジーループが発生するでしょうか。

Ex.2
以下の型`A a b`は`Arrow`になるでしょうか？`SF`のArrowインスタンスとの関係は？

    type A a b i o = ESF i o a b

Ex.3
型レベルプログラミングを用いて、`dAlt`を一般の直和型に拡張してください

Ex.4 上の正規表現コンビネータを、LL(1)文法に拡張してください。

Ex.5 非決定性オートマトンを表現するにはどうすればよいでしょうか。

認知行動療法に基づく自己対話
----------------------------

認知行動療法(CBT)は、鬱病患者が一定のテ
キストを自習することでも効果があるそうです(Jamison and Scogin
[1995](#ref-JamisonScogin95))。投薬およ
び専門家によるセラピーとの組み合わせで効果があがるとの結果(F. Scogin
[1989](#ref-Scogin89))もあるそうなので、
主治医に相談されることをお勧めします。 CBTは
[保険適用されます](https://clinicalsup.jp/contentlist/shinryo/ika_2_8_1/i003-2.html "CBT health insurance")。

ここでは、スタンフォード大学名誉教授 David
Burns医師によるCBT自助テキスト Feeling Good(Burns
[1981](#ref-Burns81))にある、自己対話スキームの一つを紹介します。

邦訳は
[こちら](http://www.seiwa-pb.co.jp/search/bo05/bn798.html)
です。

### Triple-column technique

1.  「自動思考」の記述

    頭に自然に受かぶ、ネガティブな考えを書く

    例: 自分はろくなコード書けない、だめなやつだ

2.  「認知の歪み」のチェック

  自動思考を10の主な認知の歪み(Cognitive
  Distortion)のパターンと照らしあわせる

  例:

  -   All-or-Nothing(全か無か)

    完璧でないコードでも、実際使われているなら無意味とはいえない

  -   Overgeneralization(過度の一般化)

    今体調が悪いだけかもしれない。ずっといいコード書けないとは限らない
  -   Mental Filter(ネガティブの取りだし)

    今いいコードを書けないことを取り出して、ことさらに強調している。

  -   Disqualifying the Positive(ポジティブの無視)

    他にいいコードを書いてるかもしれないし、コーディング以外にうまくいってることもあるかもしれない

  -   Fortune Teller Error(占いミス)

    根拠なく、この先もいいコード書けないと予想

  -   Maginification/Minimization

    今の体調でいいコード書けなかったことを過大に考えている

  -   Emotional Reasoning(感情思考)

     気分が沈む、だからいいコード書けないだろう。論理でなく、感情的な推論

  -   Labeling/Mislabeling(レッテル)

     どんな点でもまったくだめな人間などいないから、だめなやつというレッテル貼りはほぼ常に誤り

   その他のパターンと詳細な説明は"Feeling Good" pp.50 を参照してください。

3.  理性的反応

  認知の歪みのパターンをチェックしたら、それを基に自動思考に反応してみます。

  例:

  自分は過去にはいいコード書いたこともあるし、評価もされた。
  今調子が悪いからといってずっとそうとは限らない。
  コーディング以外にも料理がおいしいと言われたし、話やすいとも言われた。
  今書いたコードは確かに完璧でないしバグもあるけど、それだけで自分がだめなやつとは言えない。
  自己管理を向上させて、体調を整えてもっと勉強して、もっといいプログラマになればいい。

同書には、10以上の自己対話スキームが紹介されています。抑鬱状態時のモチベーションの低下、
予定の引き延ばし(procrastination)、人間関係の悪化など、状況ごとに方法論が整理されています。
便利な自己診断チェックリストもあり、不調を抱える方は是非一読をお勧めします。

YampaでTriple-column Techniqueを実装
------------------------------------

正規表現コンビネータを使うと、簡単に自己対話スキームを表現することができます。

    type ESF ei eo a b = ei -> Yampa.SF a (b, Yampa.Event eo)
    type EBot i o = ESF i o BotInput [BotOutput]
    type CbtBot   = EBot CbtCommand CbtCommand

    -- 通常時の状態機械stmと緊急時状態機械abortを重ねあわせる
    cbtE :: CbtBot
    cbtE e = (stm e &&& abort e) >>^ cat
       where
         -- 通常時状態機械
         stm   = dPlus $ cbtSessionGuideE `dStep` doMethod
         -- 緊急時状態機械
         abort = cbtAbort

         -- 重ね合せ
         cat ((xs, e), (ys, _)) = (xs++ys, e)

         -- `dAlt`の変わりに、case表現を使います
         doMethod e = case e of

             -- ヘルプモード
             CbtSessionHelpStart  -> cbtSessionHelpE e

             -- Triple-column technique モード
             CbtMethodStart CbtTripleColumnTechnique
                                  -> cbtTripleColumnTechniqueE e

             -- 警告を表示
             _                    -> cbtUnimplementedE e

Haskellではユナリオペレータの定義に制限
があるため、普通の正規表現と完全に一致す
るような自然なDSLは作れませんが、かなり
直感的な表現が可能であることがわかると思 います。

Triple-column technique自体は、分岐のな
い三つの対話モードの連結で表現できます。

    -- 三つのコラムを連結
    cbtTripleColumnTechniqueE :: CbtBot
    cbtTripleColumnTechniqueE = foldl1 dStep [atMode, cdMode, rrMode]
      where
        method = CbtTripleColumnTechnique
        -- 自動思考コラム
        atMode = cbtColumnE method CbtAutomaticThought
        -- 認知の歪みコラム
        cdMode = cbtColumnE method CbtCognitiveDistortion
        -- 合理的反応コラム
        rrMode = cbtColumnE method CbtRationalResponse

以下が対話の例です。

    bot> BotStart
    you> :3
    bot> BotMethodStart CbtTripleColumnTechnique
    bot> BotMethodColumnStart   CbtStackAT
    you> I'm a dumb
    bot> BotStackPush CbtStackAT "I'm a dumb"
    you>
    bot> BotStackFreeze  CbtStackAT
    bot> BotMethodColumnFinish  CbtStackAT
    bot> BotMethodColumnStart   CbtStackCD
    you> All-or-Nothing, Labeling
    bot> BotStackPush CbtStackAT "All-or-Nothing, Labeling"
    ...

シグナル関数の実行
------------------

Yampaでは、次の「メイン関数」が用意されています。

     reactimate :: Monad m
                  -- ^ Initialization action
               => m a
                  -- ^ Input sensor
               -> (Bool -> m (DTime, Maybe a))
                  -- ^ Output processor
               -> (Bool -> b -> m Bool)
                  -- ^ Signal function
               -> SF a b
               -> m ()

任意のモナドでパラメトライズされているの
で、どのような入出力系にも適用できます。

他のライブラリとの兼ね合いでエントリーが
制限されている場合は、`IORef`を使いシグ
ナル関数をステップごとに実行するAPIも用意されています。

結論
----

Functional Reactive Programmingは関数言
語の強みをフル活用した方法論と言えます。
「システムの挙動」という高階データを、通
常の(一階の)データと同様に、引数として渡
し、あるいは合成して複雑な挙動を組み上げ
ていくことができます。本記事では、シグナ
ルの値方向への計算を定義するArrowインス
タンスと別に、時間方向の状態変動をシンボ
リックに表す正規表現DSLを定義しました。
Slack、Line、Twitterなどのウェブサービス
を利用するボットや、実際のロボットの制御などに活用できるかもしれません。

参考文献
--------

<a name="#ref-Burns81"></a>
* Burns, David D. 1981. *Feeling Good: The New Mood Therapy*. New York,
  N.Y.: Penguin Books.

::: {#ref-ElliottHudak97:Fran}
Elliott, Conal, and Paul Hudak. 1997. "Functional Reactive Animation."
In *International Conference on Functional Programming*.
<http://conal.net/papers/icfp97/>.
:::

::: {#ref-Scogin89}
F. Scogin, K. Gochneaut, C. Jamison. 1989. "The Comparative Efficacy of
Cognitive and Behavioral Bibliotherapy for Mildly and Moderately
Depressed Older Adults." *Journal of Consulting and Clinical Psychology*
57:403--7.
:::

::: {#ref-Hughes00}
Hughes, John. 2000. "Generalising Monads to Arrows." *Science of
Computer Programming* 37 (May):67--111.
:::

::: {#ref-JamisonScogin95}
Jamison, C., and F. Scogin. 1995. "Outcome of Cognitive Bibliotherapy
with Depressed Adults." *Journal of Consulting and Clinical Psychology*
63:644--50.
:::
:::

[^1]: Paul Hudakによる[Yampaスライド](http://www.cs.yale.edu/homes/hudak/CS429F04/LectureSlides/YampaForCs429.ppt "Yampa Slide")を参照。

[^2]: スイッチの[ダイアグラム](http://lambdor.net/?p=209 "Yampa Switch Diagram")がわかりやすいです。
