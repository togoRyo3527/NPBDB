# 概要
NPBのデータをスクレイピングするRscriptです。  
成績は*BASEBALL REFERENCE*( https://www.baseball-reference.com/ )、  
選手情報は*NPB.jp 日本野球機構*( https://npb.jp/ )より取得します。

成績は1950年以降の基本的な打撃・投球・守備指標です。 



選手情報は、コード実行時前年度の氏名・name・投打・身長・体重・生年月日・経歴・ドラフト等です。  
ただし、選手によってNAが多々存在しています。

# ファイル

|  |  | 
| :-: | - |
| *NPB.R* | 選手情報の取得 |
| *reading.R* | 成績の取得<br>年度/チームごとにcsvを作成 |
| *data_mining.R* | 打撃・投球・守備ごとにcsvを結合 |