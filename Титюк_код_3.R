library(readr)
library(tidyverse)
library(scales)

#обираємо робочою папкою папку з файлами даних по ЗНО
setwd("C:/Users/Serhii.Tytiuk/Desktop/zno_data")

#завантажуємо файли даних за 2019 і 2020 роки з українською локалізацією
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



