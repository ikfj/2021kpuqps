#いつものデータを入れる
data2020<-read_excel("trialdata.xls")
#以下の分析では"data2020"でお願いね!
attach(data2020)

#まずは変数処理をしましょう
#教育程度（説明変数側）
#欠損値の処理
edu<- q2_2
edu[q2_2== 6] <- NA

#単純度数をedu1で格納
edu1 <- table(edu)
#パーセントで見てみる（パーセンタイルなので100倍にする）
edu_per <- prop.table(edu1)*100
edu_per

#支持政党（アウトカム側）の処理
#まずは簡単に政党支持者の割合を見てみよう
table(q11_1)

#欠損値(10と11）の処理
pid <- q11_1
pid[q11_1== 10|q11_1== 11] <- NA

#与党:自民+公明を"与党"
pid[q11_1== 1|q11_1== 5] <- "1"
#野党:自民+公明以外を"2"
pid[q11_1== 2|q11_1== 3|q11_1== 4|q11_1== 6|q11_1== 7|q11_1== 8] <- "2"
#無党派:「9.支持する政党はない」を"3"
pid[q11_1== 9] <- "3"

#単純度数をpid1で格納
pid1 <- table(pid)
#パーセントで見てみる（パーセンタイルなので100倍にする）
pid_per <- prop.table(pid1)*100
pid_per
#まとめてみたい
cbind(pid1, pid_per)

#さて，クロス表を見てみよう！
prop.table(table(edu,pid),margin = 1)
#うーんわかりずらい！
prop.table(table(edu,pid),margin = 1)*100


#数字じゃわかりずれぇなぁ
pid_n <- pid
#与党:自民+公明を"与党"
pid_n[pid== 1] <- "government_party"
#野党:自民+公明以外を"2"
pid_n[pid== 2] <- "opposit_party"
#無党派:「9.支持する政党はない」を"3"
pid_n[pid== 3] <- "independent"


#少数桁も多いのでだるい．2桁までにしてクロス表を見てみよう！
round(prop.table(table(edu, pid_n),1)*100,2)


#教育程度も3段階でラベル付けしてみよう
edu_n <- edu
#1:中卒・高卒
edu_n[edu== 1|edu==2] <- "中/高卒"
edu_n[edu== 3] <- "専門卒"
edu_n[edu== 4|edu ==5] <- "大卒以上"

#少数桁も多いのでだるい．次は1桁までにしてクロス表を見てみよう！
round(prop.table(table(edu_n, pid_n),1)*100,1)
#ちなみに逆にしてみると？
round(prop.table(table(pid_n,edu_n),1)*100,1)

###############################################################################
###############################################################################
###############################################################################
#年齢
age <- age_raw
summary(age)

#ちなみに図示もできます
hist(age)

#年齢とクロス
round(prop.table(table(age,pid_n),1)*100,1)

#世代
generation <- age
generation[age>=18 &age<30] <- "20代"
generation[age>=30 &age<40] <- "30代"
generation[age>=40 &age<50] <- "40代"
generation[age>=50 &age<60] <- "50代"
generation[age>=60 &age<70] <- "60代"
generation[age>=70] <- "70代"
table(generation)
#世代と支持政党のクロス
round(prop.table(table(generation, pid_n),1)*100,1)

#########################################################################
#########################################################################
#########################################################################
#ここから11/09日分

#性別ごとの平均値の比較

#性別
gender <- sex
gender[sex==1] <- "male"
gender[sex==0] <- "female"

table(gender)

#無党派変数
#数字じゃわかりずれぇなぁ
pid2 <- pid
#どこかの政党を支持していたら（pid変数が1もしくは2）は"party supporter"
pid2[pid == 1|pid == 2] <- "party supporter"
#無党派はindependent
pid2[pid== 3] <- "independent"

#クロス表
round(prop.table(table(gender,pid2),1)*100,1)
table(gender,pid2)

########################################################
#標準正規分布に従うランダムデータからカイ二乗分布を見てみる
kai2 <- function(df) {
  sum(rnorm(df, mean=0, sd=1)**2)
}

#自由度=1
chi1 <- sapply(c(1:10000), function(x) { kai2(1)} )
hist(chi1, freq=FALSE, breaks=30)
curve(dchisq(x, 1), 0, 8, add=TRUE)

#自由度=2
df <- 2
chi2 <- sapply(c(1:10000), function(x) { kai2(df)} )
hist(chi2, freq=FALSE, breaks=30, ylim=c(0, 0.5))
curve(dchisq(x, df), 0, 8, add=TRUE)

#自由度=5
df <- 5
chi5 <- sapply(c(1:10000), function(x) { kai2(df)} )
hist(chi5, freq=FALSE, breaks=30, ylim=c(0, 0.2))

#自由度=10
df <- 10
chi10 <- sapply(c(1:10000), function(x) { kai2(df)} )
hist(chi10, freq=FALSE, breaks=30, ylim=c(0, 0.2))
curve(dchisq(x, df), 0, 8, add=TRUE)


############################################################

#カイ二乗検定
chisq.test(gender,pid2,correct=FALSE)
chisq.test(gender,pid2,correct=FALSE)$observed

