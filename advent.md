YampaでFunctional Reactiveな認知行動療法ボットを書く
===================================================

はじめに
--------

Functional Reactive Programming(FRP)を使うと、時間軸に沿って
入出力の関係が変化するシステムの挙動を、シンボリックに記
述し、組み合わせることができます。ロボット、ゲーム、GUI
などのプロラミングに便利です。認知行動療法(Cognitive
Behaviour Therapy, CBT)は、医学的に確立された心理療法の一種
です。ここではCBTの自己対話スキームを提供するSlack用のボッ
トを、FRPライブラリYampaを使って書いてみます。

YampaとArrowised Functional Reactive Programming(AFRP)
------------------------------------------------------

FRPは主に二つの概念によってなりたっています。

1. 時間tにそって連続的に変動するシステムの_挙動_
2. _イベント_の時系列`[ev(t0), ev(t1), ... ]`

この二つをシンボリックに組み合わせ、複雑なシステムを記述して
いく技法が、_Functional Reactive Programming_です。

FRPには次のような利点があります。

1. 単純なシステムをシンボリックに組み合わせ、より複雑なシステムを記述する、
   関数言語の方法論が使える。(例: `sf1 >>> sf2`)
2. システムを入出力と独立に定義できる。(e.g. stdio, http, websocket, GUI..)

欠点としては、特にHaskellの場合、パフォーマンスやリークの分析が難しいことが
あげられます。FRPで起きるリークは、スペースリークとタイムリークに分けられます。
Yampa[1]では、時間tを直接露出せず、_シグナル関数_を操作するAPIを提供することによって、
タイムリークの問題を緩和しています。

Yampa[1]は、イエール大Haskellグループによって開発されたFRPのライブラリです。
主に4つの概念から成りたっています。

1. 時間`Time`。システムが動きだした時を0とする。
2. シグナル `Signal a = Time -> a`。値`a`を取る`Time`の関数。
3. シグナル関数 `SF a b = Signal a -> Signal b`。
   シグナルをシグナルへ写す、高階関数(functional)です。
4. イベント `Event a = NoEvent | Event a`。`Maybe`と同型。
   シグナル関数から別のシグナル関数に_スイッチ_するときに使う。

シグナル`Time->a`の直接操作はタイムリークを起しやすい、というのが
Yampaの主張です。遅延評価の下、「昔の記憶」がどこかに残っていれば、
その記憶につながった記憶も全てリークしてしまいます。

Yampaではシグナルでなく、シグナル関数を操作します。過去の`Time`を参照する
操作(e.g. integral, differential, delay..)は限定され、リークをおこさないように
設計されています。

シグナル関数は自然に`Arrow`インスタンスを定義できます。「時間」を隠すYampaの
のArrowプログラミングは、「状態」を隠す`IO`モナドプログラミングと類似があります。
(ArrowとMonadの類似と違いについては、Lugendreさんの記事[2]を参照して下さい)

状態機械DSLをYampaで定義する
----------------------------

Yampaでシステムの状態を操作するには、次の方法があります。

1. `loopPre :: c -> SF (a,c) (b,c) -> SF a b`

2. スイッチ:

        -- *とd*の違いはスイッチが起きた瞬間、スイッチ前と後のどちらの出力を使うか
        -- d(==delayed)はスイッチ後の出力を使う
        switch   :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
        dSwitch  :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
        rSwitch  :: SF a b -> SF (a, Event (SF a b)) b
        drSwitch :: SF a b -> SF (a, Event (SF a b)) b
        kSwitch  :: SF a b -> SF (a, b) (Event c) -> (SF a b -> c -> SF a b) -> SF a b
        dkSwitch :: SF a b -> SF (a, b) (Event c) -> (SF a b -> c -> SF a b) -> SF a b

1.の状態変数を直接使った記述は、複雑な状態機械の記述にはあまり向きません。
単純な正規表現や生成文法から、大量の状態変数が必要になることはよくありますし、
少しの文法の変更が状態機械の大きな変更を強いることもあります。

ここでは、スイッチを使い以下のシグナル関数正規表現コンビネータを定義してみます。


    type UnitEvent = Event ()
    type ESF i o a b = i -> SF a (b, Event o)
    type UnitSF a b  = ESF () ()

    -- | Sequential associative composition along the Event sequence ("ab" in regexp)
    dStep :: ESF e e a b -> ESF e e a b -> ESF e e a b
    -- | Choice -- as in "a|b"
    dAlt  :: ESF il ol a b -> ESF ir or a b -> ESF (Either il ir) (Either ol or) a b
    -- | Loop, "a*"
    dLoop :: ESF e e a b -> ESF e e a b


Ex.1
以下の型`A a b`は`Arrow`になるでしょうか？`SF`のArrowインスタンスとの関係は？

    type A a b i o = ESF i o a b

Ex.2
型レベルプログラミングを用いて、`dAlt`を一般の直和型に拡張する

認知行動療法に基づく自己対話
----------------------------

認知行動療法(CBT)は、鬱病患者が一定のテキストを自習することでも効果があるとの報告[1]があります。
投薬および専門家によるセラピーとの組み合わせで効果があがるとの結果もある[2]ので、主治医に相談されることを
お勧めします。CBTは保険適用範囲です。

以下の自助テキストに基づく、いくつかの自己対話スキームを紹介します。

Feeling Good
David Burns, M.D.

1. Triple-column technique
   1. 自動思考の記述
      頭に自然に受かぶ、ネガティブな考えをかく
   2. 認知の歪みのパターンに照らす
      
