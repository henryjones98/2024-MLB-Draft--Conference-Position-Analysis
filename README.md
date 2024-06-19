### Main Analysis- R Code ###
D1 <- filter(X2023_Draft_Dataset, TYPE == "4YR")
D1_Reg_1 <- lm(BONUS~SEC+Power_Five_Non_SEC+SS+OF+Pitcher, data = D1)
summary(D1_Reg_1)
D1_Reg_2 <- lm(BONUS~SEC+Mid_Major+SS+OF+Other, data = D1)
summary(D1_Reg_2)
D1_Reg_3 <- lm(BONUS~Power_Five_Non_SEC+Mid_Major+SS+Pitcher+Other, data = D1)
summary(D1_Reg_3)
D1_Reg_4 <- lm(BONUS~SEC+Power_Five_Non_SEC+OF+Pitcher+Other, data = D1)
summary(D1_Reg_4)
D1_Reg_5 <- lm(BONUS~SEC+Power_Five_Non_SEC+Mid_Major, data = D1)
summary(D1_Reg_5)

huxreg(D1_Reg_5, D1_Reg_1, D1_Reg_2, D1_Reg_3, D1_Reg_4)

### Other analyses- R Code ###
Region_woS <- lm(BONUS~RegNE+RegMW+RegW, data = X2023_Draft_Dataset)
summary(Region_woS)
Region_woW <- lm(BONUS~RegNE+RegMW+RegS, data = X2023_Draft_Dataset)
summary(Region_woW)
Region_woMW <- lm(BONUS~RegNE+RegW+RegS, data = X2023_Draft_Dataset)
summary(Region_woS)
Region_woNE <- lm(BONUS~RegMW+RegW+RegS, data = X2023_Draft_Dataset)
summary(Region_woS)
huxreg(Region_woMW, Region_woNE, Region_woW, Region_woS)

library(huxtable)
huxreg(Region_woMW, Region_woNE, Region_woS, Region_woW)

Level <- lm(BONUS~SEC+Power_Five_Non_SEC+Mid_Major+JC+HS, data = X2023_Draft_Dataset)
summary(Level)

Position <- lm(BONUS~SS+OF+Pitcher, data = X2023_Draft_Dataset)
summary(Position)

Day_One <- filter(X2023_Draft_Dataset, PICK <= 70)
summary(Day_One)

Level_Day_One <- lm(BONUS~SEC+Power_Five_Non_SEC+Mid_Major+JC+HS, data = Day_One)
summary(Level_Day_One)

Position_Day_One <- lm(BONUS~SS+OF+Pitcher, data = Day_One)
summary(Position_Day_One)

Pitchers <- filter(X2023_Draft_Dataset, Pitcher == 1)
Reg_Pitchers <- lm(BONUS~SEC+Power_Five_Non_SEC+Mid_Major+JC+HS, data = Pitchers)
summary(Reg_Pitchers)
