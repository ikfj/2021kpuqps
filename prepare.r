# ZIPファイルをダウンロードして解凍
download_and_extract <- function(url) {
  tmp <- tempfile()
  download.file(url, tmp)
  files <- unzip(tmp)
  unlink(tmp)
  return(files)
}

# テキストファイルのエンコーディングをShift_JISからUTF-8に変換する
to_utf8 <- function(filename) {
  f <- file(filename, open="r", encoding="SHIFT-JIS")
  lines <- readLines(f)
  close(f)
  f <- file(filename, open="wb", encoding="UTF-8")
  writeLines(lines, f)
  close(f)
}

# 『Rで学ぶデータサイエンス 14 計量政治分析』（共立出版）
# に掲載されているRコードをダウンロードする
# https://www.kyoritsu-pub.co.jp/bookdetail/9784320019249
files <- download_and_extract("https://www.kyoritsu-pub.co.jp/app/file/goods_contents/1730.zip")
for (filename in files) {
  to_utf8(filename)
}

# 「2003年衆院選-04年参院選-05年衆院選世論調査」（東京大学谷口研究室・朝日新聞共同調査）
# のExcelデータをダウンロードする
# http://www.masaki.j.u-tokyo.ac.jp/utas/utasv.html
files <- download_and_extract("http://www.masaki.j.u-tokyo.ac.jp/utas/utas060104xls.zip")

# Excelデータから q010601 列を切り出して cabsup.csv を作る
library(readxl)
utasdata <- read_excel("utas060104.xls")
data <- utasdata['q010601']
names(data) <- c("cabsup")
write.csv(data, "cabsup.csv", row.names=FALSE)

# 利用するいつものデータ
#trialdata.xlsという新しいデータを秦のサーバ（遊戯王で言えばデッキ？）からfiles2という名前で入れ込む（遊戯王で言えば手札？）に！
trialdata <- download_and_extract("http://hatam.sakura.ne.jp/trialdata.zip")
#trialdataという名前のファイルをhatadataという愛称をつけて呼び出す（召喚！）
hatadata <- read_excel("trialdata.xls")
#これ以降，hatadata以外のデータは受け付けないという魔法効果的やおまじない
attach(hatadata)
