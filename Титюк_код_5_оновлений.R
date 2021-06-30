library(readr)
library(tidyverse)
library(scales)


#завантажуємо дані за 2019 і 2020 роки з dropbox
url1 <- "https://www.dropbox.com/s/ehiww8h1ted4quz/Odata2019File.csv?dl=1"
download.file(url1, destfile = "./Odata2019File.csv")

url2 <- "https://www.dropbox.com/s/j1ijefd67w51sfq/Odata2020File.csv?dl=1"
download.file(url2, destfile = "./Odata2020File.csv")

#завантажуємо файли даних в R за 2019 і 2020 роки з українською локалізацією
Sys.setlocale("LC_CTYPE", "ukrainian")
zno_2019 <- read.csv("Odata2019File.csv", sep=";", encoding="Cyrillic")
zno_2020 <- read.csv("Odata2020File.csv", sep=";", encoding="Cyrillic")

#перевіряємо, чи збігаються змінні в масивах
summary(colnames(zno_2019) == colnames(zno_2020))

#додаємо в файли даних колонку з роком і об'єднуємо їх  
zno_2019 <- cbind(zno_2019, year=rep("2019", times=length(zno_2019$REGTYPENAME)))
zno_2020 <- cbind(zno_2020, year=rep("2020", times=length(zno_2020$REGTYPENAME)))
zno_full <- rbind(zno_2019, zno_2020)

#залишаємо в масиві лише потрібні нам змінні, залишаємо лише випускників шкіл, 
#заміняємо для числових змінних null на NA, кому на крапку, конвертуємо числові змінні в numeric, категоріальні - в factor
zno_light <- zno_full %>% 
  select(REGTYPENAME, TerTypeName, EONAME, UkrTestStatus, UkrBall100, UkrPTName,
         histTestStatus,histBall100, histPTName, mathTestStatus, mathBall100, 
         mathPTName, engTestStatus, engBall100, engPTName, year) %>% 
  filter(REGTYPENAME == "Випускник закладу загальної середньої освіти 2019 року" | 
           REGTYPENAME == "випускник закладу загальної середньої освіти 2020 року") %>%
  mutate_all(~na_if(., "null")) %>% 
  mutate(UkrBall100 = sub(",", ".", UkrBall100), histBall100 = sub(",", ".", histBall100), 
         mathBall100 = sub(",", ".", mathBall100), engBall100 = sub(",", ".", engBall100),
         UkrBall100 = as.numeric(UkrBall100), histBall100 = as.numeric(histBall100), mathBall100 = as.numeric(mathBall100), 
         engBall100 = as.numeric(engBall100), REGTYPENAME=as.factor(REGTYPENAME), TerTypeName = as.factor(TerTypeName), 
         UkrTestStatus = as.factor(UkrTestStatus), histTestStatus = as.factor(histTestStatus),
         mathTestStatus=as.factor(mathTestStatus), engTestStatus=as.factor(engTestStatus), year=as.factor(year)) 

#додаємо для кожного предмета по колонці, яка відображатиме вступників, які складили іспити в своєму навчальному закладі
zno_light <- zno_light %>% 
  mutate(Ukr_same = EONAME == UkrPTName, hist_same = EONAME == histPTName, 
         math_same =  EONAME == mathPTName, eng_same =  EONAME == engPTName) %>% 
  mutate(Ukr_same = as.factor(Ukr_same), hist_same = as.factor(hist_same), 
         math_same = as.factor(math_same),eng_same = as.factor(eng_same))

#"подовжуємо" масив - групуємо дані по кожному предмету в спільні для всіх колонки, 
#розбивши перед цим на окремі масиви і відфільтрувавши тих, хто не реєструвався на ЗНО з конкретного предмету або чиї результати було анульовано
ukr_data <- zno_light %>% 
  select(TerTypeName, UkrTestStatus, UkrBall100, Ukr_same, year) %>% 
  mutate(subject = "Українська мова") %>% 
  rename(TestStatus = UkrTestStatus, Ball = UkrBall100, same_school = Ukr_same) %>% 
  filter(TestStatus %in% c("Зараховано", "Не з’явився", "Не подолав поріг"))

math_data <- zno_light %>% 
  select(TerTypeName, mathTestStatus, mathBall100, math_same, year) %>% 
  mutate(subject = "Математика") %>% 
  rename(TestStatus = mathTestStatus, Ball = mathBall100, same_school = math_same) %>% 
  filter(TestStatus %in% c("Зараховано", "Не з’явився", "Не подолав поріг"))

hist_data <- zno_light %>% 
  select(TerTypeName, histTestStatus, histBall100, hist_same, year) %>% 
  mutate(subject = "Історія") %>% 
  rename(TestStatus = histTestStatus, Ball = histBall100, same_school = hist_same) %>% 
  filter(TestStatus %in% c("Зараховано", "Не з’явився", "Не подолав поріг"))

eng_data <- zno_light %>% 
  select(TerTypeName, engTestStatus, engBall100, eng_same, year) %>% 
  mutate(subject = "Англійська мова") %>% 
  rename(TestStatus = engTestStatus, Ball = engBall100, same_school = eng_same) %>% 
  filter(TestStatus %in% c("Зараховано", "Не з’явився", "Не подолав поріг"))

zno_long <- rbind(ukr_data, math_data, hist_data, eng_data) %>% mutate(subject = as.factor(subject))
levels(zno_long$same_school) <-  c("В іншому закладі", "В своєму закладі")

#перевіряємо, чи є значуща різниця між частками випускників шкіл, 
#які складали ЗНО в своєму закладі освіти, в 2019 і 2020 роках  

#агрегуємо дані в таблицю для проведення тестів на різницю в пропорціях між 2019 і 2020 роками
prop_same_school <- zno_long %>%
  group_by(subject, year, same_school) %>%
  summarise(counts=n()) %>%
  mutate(n_total = sum(counts)) %>%
  filter(same_school=="В своєму закладі") %>%
  rename(n_same_school = counts)

#тести на різницю в пропорції тих, хто складав ЗНО в своєму закладі, для кожного предмета 
prop_hist <- prop.test(prop_same_school$n_same_school[c(1,2)], prop_same_school$n_total[c(1,2)]) #Історія
prop_eng <- prop.test(prop_same_school$n_same_school[c(3,4)], prop_same_school$n_total[c(3,4)]) #Англійська мова
prop_math <- prop.test(prop_same_school$n_same_school[c(5,6)], prop_same_school$n_total[c(5,6)]) #Математика
prop_ukr <- prop.test(prop_same_school$n_same_school[c(7,8)], prop_same_school$n_total[c(7,8)]) #Українська мова
#ВСІ ТЕСТИ ПОКАЗАЛИ НАЯВНІСТЬ СТАТИСТИЧНО ЗНАЧУЩОЇ РІЗНИЦІ

#зводимо результати тестів в один датафрейм для наглядності
options(scipen=999) #прибирає експоненційний запис
prop_tests <- t(data.frame(unlist(prop_hist[c("estimate", "p.value")]),
                           unlist(prop_eng[c("estimate", "p.value")]),
                           unlist(prop_math[c("estimate", "p.value")]),
                           unlist(prop_ukr[c("estimate", "p.value")])))

#видаляємо зайві об'єкти
rm(list=c("prop_hist", "prop_eng", "prop_math", "prop_ukr"))

write.csv(prop_tests, file="prop_tests.csv")

#перевіряємо, чи є значуща різниця між часткою тих, хто не з'явився на ЗНО,
#між випускниками, які складали тест в своєму закладі освіти, та тими, хто складав його в іншому закладі

#агрегуємо дані в таблицю для проведення тестів на різницю в пропорціях між своїм та чужим закладами
prop_presence <- zno_long %>% 
  filter(year=="2020") %>% 
  group_by(same_school, subject, TestStatus) %>% 
  summarise(counts=n()) %>% 
  mutate(total = sum(counts)) %>% 
  filter(TestStatus == "Не з’явився") %>%
  arrange(subject)

#тести на різницю в пропорції тих, хто не з'явився на ЗНО, залежно від свого/не свого закладу
#в розрізі кожного предмета (використовується "less", адже значення для своєї школи в таблиці йдуть після чужої школи
presence_hist <- prop.test(prop_presence$counts[c(1,2)], prop_presence$total[c(1,2)], alternative = "less") #Історія
presence_eng <- prop.test(prop_presence$counts[c(3,4)], prop_presence$total[c(3,4)], alternative = "less") #Англійська мова
presence_math <- prop.test(prop_presence$counts[c(5,6)], prop_presence$total[c(5,6)], alternative = "less") #Математика
presence_ukr <- prop.test(prop_presence$counts[c(7,8)], prop_presence$total[c(7,8)], alternative = "less") #Українська мова

#зводимо результати тестів в один датафрейм для наглядності
presence_tests <- t(data.frame(unlist(presence_hist[c("estimate", "p.value")]),
                               unlist(presence_eng[c("estimate", "p.value")]),
                               unlist(presence_math[c("estimate", "p.value")]),
                               unlist(presence_ukr[c("estimate", "p.value")])))

#видаляємо зайві об'єкти
rm(list=c("presence_hist", "presence_eng", "presence_math", "presence_ukr"))
write.csv(presence_tests, file="presence_tests.csv")


#перевіряємо, чи є значуща різниця між часткою тих, хто не з'явився на ЗНО, 
#між випускниками залежно від закладу (свій чи чужий) в розрізі міста та села

prop_presence_set <-zno_long %>% 
  filter(year=="2020") %>% 
  group_by(same_school, subject, TerTypeName, TestStatus) %>% 
  summarise(counts=n()) %>% 
  mutate(total = sum(counts)) %>% 
  filter(TestStatus == "Не з’явився") %>%
  arrange(subject, TerTypeName, same_school)


#проводимо тести на наявність значимої різниці між часткою тих, хто не з'явився на ЗНО
#залежно від закладу (свій чи ні) та в розрізі типу населеного пункту
hist_urban <- prop.test(prop_presence_set$counts[c(1,2)], 
                        prop_presence_set$total[c(1,2)], alternative = "less") #Історія в містах
hist_rural <- prop.test(prop_presence_set$counts[c(3,4)], 
                        prop_presence_set$total[c(3,4)], alternative = "less") #Історія в селах
eng_urban <- prop.test(prop_presence_set$counts[c(5,6)], 
                       prop_presence_set$total[c(5,6)], alternative = "less") #Англійська мова в містах
eng_rural <- prop.test(prop_presence_set$counts[c(7,8)], 
                       prop_presence_set$total[c(7,8)], alternative = "less") #Англійська мова в селах
math_urban <- prop.test(prop_presence_set$counts[c(9,10)], 
                        prop_presence_set$total[c(9,10)], alternative = "less") #Математика в містах
math_rural <- prop.test(prop_presence_set$counts[c(11,12)], 
                        prop_presence_set$total[c(11,12)], alternative = "less") #Математика в селах
ukr_urban <- prop.test(prop_presence_set$counts[c(13,14)], 
                       prop_presence_set$total[c(13,14)], alternative = "less") #Українська мова в містах
ukr_rural <- prop.test(prop_presence_set$counts[c(15,16)], 
                       prop_presence_set$total[c(15,16)], alternative = "less") #Українська мова в селах

presence_set_tests <- t(data.frame(unlist(hist_urban[c("estimate", "p.value")]),
                                   unlist(hist_rural[c("estimate", "p.value")]),
                                   unlist(eng_urban[c("estimate", "p.value")]),
                                   unlist(eng_rural[c("estimate", "p.value")]),
                                   unlist(math_urban[c("estimate", "p.value")]),
                                   unlist(math_rural[c("estimate", "p.value")]),
                                   unlist(ukr_urban[c("estimate", "p.value")]),
                                   unlist(ukr_rural[c("estimate", "p.value")])))

#видаляємо зайві об'єкти
rm(list=c("hist_urban", "hist_rural", "eng_urban", "eng_rural", "math_urban", "math_rural", "ukr_urban", "ukr_rural"))
write.csv(presence_set_tests, file="presence_set_tests.csv")


#перевіряємо, чи відрізняються середні значення балу ЗНО залежно від того, чи здавав
#випускин ЗНО в своє закладі освіти або в чужому окремо для 2019 і 2020 років
test_hist_2019 = t.test(Ball ~ same_school, data = hist_data %>% 
                          filter(TerTypeName == "місто", TestStatus == "Зараховано", year=="2019"), alternative = "less") #Історія в 2019
test_hist_2020 = t.test(Ball ~ same_school, data = hist_data %>% 
                          filter(TerTypeName == "місто", TestStatus == "Зараховано", year=="2020"), alternative = "less") #Історія в 2020
test_eng_2019 = t.test(Ball ~ same_school, data = eng_data %>% 
                         filter(TerTypeName == "місто", TestStatus == "Зараховано", year=="2019"), alternative = "less") #Англійська мова в 2019
test_eng_2020 = t.test(Ball ~ same_school, data = eng_data %>% 
                         filter(TerTypeName == "місто", TestStatus == "Зараховано", year=="2020"), alternative = "less") #Англійська мова в 2020
test_math_2019= t.test(Ball ~ same_school, data = math_data %>% 
                         filter(TerTypeName == "місто", TestStatus == "Зараховано", year=="2019"), alternative = "less") #Математика в 2019
test_math_2020 = t.test(Ball ~ same_school, data = math_data %>% 
                          filter(TerTypeName == "місто", TestStatus == "Зараховано", year=="2020"), alternative = "less") #Математика в 2020
test_ukr_2019 = t.test(Ball ~ same_school, data = ukr_data %>% 
                         filter(TerTypeName == "місто", TestStatus == "Зараховано", year=="2019"), alternative = "less") #Українська мова в 2019
test_ukr_2020 = t.test(Ball ~ same_school, data = ukr_data %>% 
                         filter(TerTypeName == "місто", TestStatus == "Зараховано", year=="2020"), alternative = "less") #Українська мова в 2020

#зводимо результати тестів в один датафрейм для наглядності
test_results <- t(data.frame(unlist(test_hist_2019[c("estimate", "p.value")]),
                             unlist(test_hist_2020[c("estimate", "p.value")]), 
                             unlist(test_eng_2019[c("estimate", "p.value")]), 
                             unlist(test_eng_2020[c("estimate", "p.value")]),
                             unlist(test_math_2019[c("estimate", "p.value")]),
                             unlist(test_math_2020[c("estimate", "p.value")]), 
                             unlist(test_ukr_2019[c("estimate", "p.value")]),
                             unlist(test_ukr_2020[c("estimate", "p.value")])))

#видаляємо зайві об'єкти
rm(list=c("test_hist_2019", "test_hist_2020", "test_eng_2019", "test_eng_2020", 
          "test_math_2019", "test_math_2020", "test_ukr_2019", "test_ukr_2020"))
write.csv(test_results, file="test_results.csv")


#ВІЗУАЛІЗАЦІЇ

#візуалізація % випускників шкіл, які складали ЗНО в 2019 і 2020 роках в своєму або іншому закладі освіти
jpeg(file="Графік2.jpeg", width=1000, height=600)
zno_long %>% 
  group_by(subject, year, same_school) %>% 
  summarise(counts=n()) %>% 
  mutate(pct= counts/sum(counts)) %>% 
  ggplot(aes(subject, pct, fill=same_school)) +
  geom_bar(position = "fill", stat="summary") + 
  geom_text(aes(label = percent(pct, accuracy=0.1), digits=1), size=5, vjust = 0) +
  scale_y_continuous(labels = percent) +
  labs(x="Предмет ЗНО", y="% випукників шклі, зареєстрованих на іспит", fill = "") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 14), 
        legend.position = "bottom", legend.text=element_text(size = 14),
        strip.text.y = element_text(size = 14))+
  scale_fill_brewer(palette="Paired") +
  facet_grid(year ~ .)
dev.off()

#візуалізація % зареєстрованих на ЗНО серед випускників шкіл, які не з’явилися на іспит
jpeg(file="Графік3.jpeg", width=1000, height=600)
zno_long %>% 
  filter(year=="2020") %>% 
  group_by(same_school, subject, TestStatus) %>% 
  summarise(counts=n()) %>% 
  mutate(pct= counts/sum(counts)) %>% 
  filter(TestStatus == "Не з’явився") %>% 
  ggplot(aes(subject, pct, fill=same_school)) +
  geom_bar(position = "dodge", stat="identity") + 
  geom_text(aes(label = percent(pct)), position=position_dodge(width=1), vjust = -0.5, size=5) +
  scale_y_continuous(labels = percent) +
  labs(x="Предмет ЗНО", y="% зареєстрованих, які не з'явилися на іспит", fill = "") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 14), 
        legend.position = "bottom", legend.text=element_text(size = 14),
        strip.text.x = element_text(size = 14), strip.text.y = element_text(size = 14))+
  scale_fill_brewer(palette="Paired")
dev.off()

#візуалізація % зареєстрованих на ЗНО серед випускників шкіл, які не з’явилися на іспит в розрізі міста та села
jpeg(file="Графік4.jpeg", width=1000, height=600)
zno_long %>% 
  filter(year=="2020") %>% 
  group_by(same_school, subject, TerTypeName, TestStatus) %>% 
  summarise(counts=n()) %>% 
  mutate(pct= counts/sum(counts)) %>% 
  filter(TestStatus == "Не з’явився") %>% 
  ggplot(aes(subject, pct, fill=same_school)) +
  geom_bar(position = "dodge", stat="identity") + 
  geom_text(aes(label = scales::percent(pct, accuracy=0.1)), position=position_dodge(width=1), vjust = -0.3, size=5) +
  scale_y_continuous(labels = percent) +
  labs(x="Предмет ЗНО", y="% зареєстрованих, які не з'явилися на іспит", fill = "") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 14), 
        legend.position = "bottom", legend.text=element_text(size = 14),
        strip.text.x = element_text(size = 14), strip.text.y = element_text(size = 14))+
  scale_fill_brewer(palette="Paired") +
  facet_grid(. ~ TerTypeName)
dev.off()

#візуалізація середньогой балу ЗНО серед учасників, які складали його в своєму або в іншому закладі освіти
jpeg(file="Графік1.jpeg", width=1000, height=900)
zno_long %>% 
  filter(TestStatus == "Зараховано", TerTypeName == "місто") %>% 
  group_by(subject, TerTypeName, year, same_school) %>% 
  summarise(meanscore = mean(Ball)) %>% 
  ggplot(aes(subject, meanscore, fill=subject)) +
  geom_bar(position = "dodge", stat = "summary") +
  coord_cartesian(ylim=c(130,160)) +
  geom_text(aes(label=round(meanscore, digits=1)), size=5, vjust = 2)+
  labs(x="Предмет ЗНО", y="Cередній бал випускників, які склали іспит", fill = "") +
  theme_classic() +
  theme(axis.text.x = element_text(size=0), axis.title.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 14), 
        legend.position = "bottom", legend.text=element_text(size = 14),
        strip.text.x = element_text(size = 14), strip.text.y = element_text(size = 14))+
  scale_fill_brewer(palette="Paired") +
  facet_grid(year ~ same_school)
dev.off()  

