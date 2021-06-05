#いつものデータを入れる
library(readxl)
library(gplots)

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
#ここから6/1分

#性別ごとのむとうはりつの比較

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
curve(dchisq(x, df), 0, 8, add=TRUE)

#自由度=10
df <- 10
chi10 <- sapply(c(1:10000), function(x) { kai2(df)} )
hist(chi10, freq=FALSE, breaks=30, ylim=c(0, 0.2))
curve(dchisq(x, df), 0, 8, add=TRUE)


############################################################
##ここから6/8分
############################################################

#カイ二乗検定
chisq.test(gender,pid2,correct=FALSE)
chisq.test(gender,pid2,correct=FALSE)$observed

############################################################
#比率の差の検定

#たとえば，朝日新聞世論調査の内閣支持率と
table(gender,pid2)
#朝日新聞：https://digital.asahi.com/articles/ASNCJ5WYMNCJUZPS008.html?iref=pc_extlink
#読売新聞：https://www.yomiuri.co.jp/election/yoron-chosa/20201124-OYT1T50021/
#c（朝日の支持者数, 読売支持者数），c(朝日調査全体N，読売調査全体N)

prop.test(c(866, 1289), c(1547, 1953))

#では，朝日10月調査（https://digital.asahi.com/articles/ASNBL77D9NBLUZPS006.html）と比べると？
prop.test(c(***, 866), c(***, 1547))


############################################################
#平均値の検定
#平均値の検定の場合，カテゴリ間の対応関係を見極める必要がある（スライド参照）

#感情温度
自民党 <- q4_2_1
立憲民主党 <- q4_2_2
共産党 <- q4_2_3
社民党 <- q4_2_4
国民民主党 <- q4_2_5

#Tukeyの多重検定
TukeyHSD(aov(自民党~pid_n)) 
TukeyHSD(aov(立憲民主党~pid_n)) 
TukeyHSD(aov(国民民主党~pid_n)) 

#韓国 (q4_3_4)  日本 (q4_3_6)*18-39歳（若年層）と40歳−59歳（中年世代）と60歳以上（高齢世代）
#で日本と韓国の世代別好感度に差はあるかTurkeyの方法で検定してみましょう

#世代
gen3 <- age
gen3[age>=18 &age<40] <- "young"
gen3[age>=40 &age<60] <- "middle"
gen3[age>=60] <- "old"
table(gen3)

gen3f <- factor(gen3, levels = c("young", "middle", "old"))

#好感度
US <- as.numeric(q4_3_1)
Korea <- as.numeric(q4_3_4)
JPN <- as.numeric(q4_3_6)

TukeyHSD(aov(US~gen3)) 
TukeyHSD(aov(Korea~gen3)) 
TukeyHSD(aov(JPN~gen3))

#平均値自体も調べてみよう（いろんな方法があるので書きやすいのでどうぞ）
aggregate(JPN, by=list(gen3), FUN=mean, na.rm=TRUE)
by(US, INDICES = gen3, FUN=mean, na.rm=TRUE)

##############################################################
#お絵かき
library(gplots)
plotmeans(US ~ gen3)
plotmeans(JPN ~ gen3)
plotmeans(Korea ~ gen3)

#ファイルに保存
png('US.png' , pointsize = 13, width = 600, height = 500)
plotmeans(US ~ gen3)
dev.off()


##############################################################
#単回帰分析
#installed.packages("ggplot2")
#installed.packages("coefplot")

library(ggplot2)
library(coefplot)

#年齢と自民感情温度の散布図を見てみよう
LDP <- 自民党

#プロットをggplotで示してみる
g <- ggplot(data2020, aes(x = age, y = LDP))+scale_x_continuous(breaks = seq(18,79,by=5),limits=c(18,79)) 
g <- g + geom_point()
g <- g + geom_smooth(method = "lm")
plot(g)
ggsave("plotlm.png")

#自民党の感情温度に与える年齢の効果を検証
result1 <- lm(LDP ~ age, data = data2020)                   
summary(result1)

#ビジュアライズしようぜ！
coefplot(result1, intercept = FALSE)
#ggplotだと簡単に保存できる！
ggsave("reg1.png")


##############################################################
#重回帰分析
#たとえば，政党支持別に見てみると…？
g1 <- ggplot(data2020, aes(x = age, y = LDP, color = pid_n))
g1 <- g1 + geom_smooth(method = "lm")
plot(g1)
ggsave("pid.png")

#教育程度ごとには？
edu_f <- factor(edu)

g2 <- ggplot(data2020, aes(x = age, y = LDP, color = edu_f))
g2 <- g2 + geom_smooth(method = "lm")
plot(g2)
ggsave("edu.png")

#性別ごと
g3 <- ggplot(data2020, aes(x = age, y = LDP, color = gender))
g3 <- g3 + geom_smooth(method = "lm")
plot(g3)
ggsave("gend.png")

#都市規模ごと
citysize <- factor(q2_4)
g4 <- ggplot(data2020, aes(x = age, y = LDP, color = citysize))
g4 <- g4 + geom_smooth(method = "lm")
plot(g4)
ggsave("cs.png")

#自民党の感情温度に与える年齢の効果を[厳密に]検証
#収入
income <- q2_3
income[q2_3==21] <- NA

#都市規模
citysize <- factor(q2_4)

#イデオロギー
ideology <- q12_4
ideology1 <- factor(ideology, levels = c("Most Liberal","1", "2", "3","4","Neutral", "6","7","8","9","Most Conservative"))


result2 <- lm(LDP ~ age + sex + edu + income + citysize + ideology + pid_n, data = data2020)                   
summary(result2)

#ビジュアライズしようぜ！2
reg2 <- coefplot(result2, intercept = FALSE,lwdOuter = 1,
                  newNames = c(edu = "EDUCATION",
                               age = "AGE", 
                               sex = "MALE(vs female)"))
reg2
ggsave("reg2.png")

#############################################################
#予測値の計算

r3 <- lm(LDP ~ age + gender + edu + income + citysize + ideology + pid_n, data = data2020)                   
summary(r3)

#年齢
p2 <- ggplot(data2020, aes(x = age, y = LDP))+scale_x_continuous(breaks = seq(18,79,by=5),limits=c(18,79)) 
p2 <- p2 + geom_smooth(method = 'lm')
#p2 <- p2 + geom_point()
p2 <- p2 + labs(x = 'age', y = 'feeling:LDP')
print(p2 + ggtitle('Age Effect on Feelings to LDP'))
ggsave("age.png")

#性別
p1 <- ggplot(data2020, aes(x = gender, y = LDP)) + scale_y_continuous(breaks=seq(50,60,by=1),limits=c(50,60)) +
  stat_summary(fun.data = "mean_se")+xlab('gender') + ylab('feeling:LDP')
print(p1+ ggtitle('Gender Effect on Feelings to LDP'))
ggsave("gender.png")

#イデオロギー
p3 <- ggplot(data2020, aes(x = ideology, y = LDP)) 
p3 <- p3 + geom_smooth(method = 'lm')
p3 <- p3 + labs(x = 'ideology', y = 'feeling:LDP')
print(p3 + ggtitle('Political Ideology Effect on Feelings to LDP'))
ggsave("ideology.png")

################################################################################
