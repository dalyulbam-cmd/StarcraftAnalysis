library(readxl)
star <- read_xlsx("starcraftUnitStatistics.xlsx") 
star <- rename(star, Population = ...4, Mineral = ...5, Gas = ...6,
               AttackType_G = Type...13, 
               Cooldown_G = Cooldown...14, Upgrade_G = Upgrade...15,
               AttackType_A = Type...17,
               Cooldown_A = Cooldown...18, Upgrade_A = Upgrade...19) 
star <- rename(star, BuildTime = Build.Time, GroundAttack = Ground.Attack, AirAttack = Air.Attack)
str(star) # Shield, AttackType_G,  Cooldown_G, AttackType_A, Cooldown_A 는 chr -> num으로 바꾸어야함.

library(dplyr)
star <- data.frame(star)
check = c("Shield","Cooldown_G","Cooldown_A")
list = colnames(star) 
checkOrder = which(list %in% check) # 10 14 18

# dbl 형임에도 "-" 수치에 의해서 숫자 데이터가 chr로 인식됨. 
# 대처 1 : "-"을 0으로 만들어줌. 
# (단, 주소로 접근하므로 for문으로 작동이 어려운것으로 판단.)
star <- star %>%
  mutate(Shield = ifelse(Shield == "-","0",Shield)) %>%
  mutate(Cooldown_G = ifelse(Cooldown_G == "-","0",Cooldown_G)) %>%
  mutate(Cooldown_A = ifelse(Cooldown_A == "-","0",Cooldown_A)) 
# 대처 2 : 숫자형 데이터로 바꿈.
# index 접근은 for문으로 얼마든지 가능함. 
for (dataset in check){
  star[,dataset] = as.numeric(star[,dataset])
}

# Notes (유닛의 캐릭터 성) "-"을 결측치 NA로 수정 
star[,"Notes"] <- ifelse(star[,"Notes"] == "-", NA, star[,"Notes"])


# Cooldown 수치는 1/24 초를 기준으로 하고 있으므로 초단위로 바꾸어준다. 
star[,"Cooldown_G"] <- (star[,"Cooldown_G"]/24) %/% 0.001 / 1000
star[,"Cooldown_A"] <- (star[,"Cooldown_A"]/24) %/% 0.001 / 1000


# dataframe star에 Unit이 공중에 떠있는지 아닌지 데이터 정보를 주어야 한다. 
onAirUnit = c(
  "BattleCruiser", "Dropship", "Science Vessel", "Wraith", "Valkyrie",
  "Arbiter", "Carrier", "Observer", "Scout", "Shuttle", "Corsair", 
  "Guardian", "Mutalisk", "Overlord", "Queen", "Scourge", "Devourer"
)
star <- star %>%
  mutate(OnAir = ifelse(Unit %in% onAirUnit, TRUE, FALSE))

write.csv(star,file = "starModified01.csv")


###############################################################################

# test
star <- read.csv(file = "starModified01.csv", header = TRUE)
star <- data.frame(star)

# 질문 1 : 지상 공격만 가능한 유닛에는 어떤 것이 있는가?
condition01 = star[,"GroundAttack"] > 0 & star[,"AirAttack"] == 0
test01 <- star[condition01,"Unit"]; test01

# 질문 2 : 스플래시 데미지를 입히는 유닛에는 어떤 것이 있는가?
splashOnGround  <- grep("s",star[,"AttackType_G"])
splashOnAir <- grep("s",star[,"AttackType_A"])
splash <- union(splashOnGround,splashOnAir)
test02 <- star[splash,"Unit"]; test02

# 질문 3 : 진동형 데미지를 가져, 중형 유닛에게 50%, 대형 유닛에게 25%의 데미지를 주는 유닛은?
concussiveGround <- grep("c",star[,"AttackType_G"])
concussiveAir <- grep("c",star[,"AttackType_A"])
concussive <- union(concussiveGround,concussiveAir)
test03 <- star[concussive,"Unit"]; test03

# 질문 4 : 폭발형 데미지를 가져, 소형 유닛에게 50%, 중형 유닛에게 75%, 대형 유닛에게 100% 데미지를 주는 유닛은?
explosiveGround <- grep("e", star[,"AttackType_G"])
explosiveAir <- grep("e", star[,"AttackType_A"])
explosive <- union(explosiveAir,explosiveGround)
test04 <- star[explosive,"Unit"]; test04

# 질문 5 : 모든 유닛에 대해 드라군 한 마리가 죽이는데 이론적으로 걸리는 시간을 구하시오. 
# 데미지 계산 함수를 만든다. : 사이즈 SML와 진동형, 폭발형 데미지를 고려.
library(stringr)
damagePercentage <- function(targetSize, damageType){

  if (is.na(damageType)) return(1)
  #  TRUE/FALSE가 필요한 곳에 값이 없습니다. 에러 방지.
  
  # (grep("e", damageType) 
  # Error in if (grep("e", damageType)) { : 인자의 길이가 0입니다.
  # 따라서 stringr :: str_detect를 쓰기로 한다. 
  
  if (str_detect(damageType,"e")){
    if (targetSize == 'S') return (0.5)
    else if (targetSize == 'M') return (0.75)
    else return (1)
  } else if (str_detect(damageType,"c")) {
    if (targetSize == 'S') return (1)
    else if (targetSize == 'M') return (0.5)
    else return (0.25)    
  } else return (1)

}


# attackTime(unit,num1 = 1,targer,num2 = 1) 함수를 만든다. 
attackTime1by1 <- function(unit, target, armorUp = 0, shieldUp = 0){
  
  unit <- which(star$Unit == unit) # attacking unit Unit의 행 번호 
  target <- which(star$Unit == target) # target Unit의 행 번호 
  
  
  # 못잡는 유닛의 경우 Inf의 시간이 걸린다. 
  if ((star[target,"OnAir"] & star[unit,"AirAttack"] == 0) |
      (!star[target,"OnAir"] & star[unit,"GroundAttack"] == 0)){
    return (Inf)
  }
  
  # 공중에 있을 땐 공중 데미지 적용, 지상에 있을 땐 지상 데미지 적용 
  damage <- ifelse(star[target,"OnAir"], star[unit,"AirAttack"], star[unit,"GroundAttack"])
  # 사이즈와 데미지 타입을 고려한 데미지 감소를 고려 
  damage <- damage * damagePercentage(
    star[target,"Size"],
    star[unit,ifelse(star[target,"OnAir"],"AttackType_A","AttackType_G")])
  # 데미지 
  damage <- damage - star[target,"Armor"]
  
  # 공중 쿨 타임? 지상 쿨 타임?
  cooltime <- star[unit,ifelse(star[target,"OnAir"],"Cooldown_A","Cooldown_G")]
  
  # 쉴드 회복과 체력 회복 
  hpRecoverTime <- ifelse(star[unit,"Race"] == "Zerg", 2.5, 0)
  sdRecoverTime <- ifelse(star[unit,"Race"] == "Protoss", 1.5, 0)
  hpRecoverCycle <- 0;
  sdRecoverCycle <- 0;
  
  time <- 0
  hp <- star[target,"HP"]
  sd <- star[target,"Shield"]

  while(hp > 0){
    # 시간 
    time <- time + cooltime;
    
    # --체력
    sd <- sd - damage + shieldUp # 데미지는 쉴드부터 깎음.
    if (sd < 0){ # 쉴드를 관통해서 데미지가 들어감. 
      hp <- hp + sd + armorUp # 깎인 만큼 hp에 더함.
      sd <- 0 # 쉴드는 0
    } 
    
    # 회복 
    if (time >= (hpRecoverCycle + 1) * hpRecoverTime ) {
      hp <- hp + 1;
      hpRecoverCycle <- hpRecoverCycle + 1;
    }
    if (time >= (sdRecoverCycle + 1) * sdRecoverTime ) {
      sd <- sd + 1;
      sdRecoverCycle <- sdRecoverCycle + 1;
    }   
  }
  return (time)
}


time01 <- attackTime1by1("Dragoon","Marine"); time01 # 6.245
time02 <- attackTime1by1("Dragoon","Valkyrie"); time02 # 16.237
time03 <- attackTime1by1("Dragoon","Egg"); time03 # 31.225
time04 <- attackTime1by1("Dragoon","Overlord"); time04 # 13.739
time05 <- attackTime1by1("Dragoon","Zergling"); time05 # 6.245 
# 저글링이랑 마린 죽는 시간 같아?
time06 <- attackTime1by1("Dragoon","Scourge"); time06 # 3.747
time07 <- attackTime1by1("Dragoon","SCV"); time07 # 9.992 


attackTime <- function(unit, target, num1 = 1 , num2 = 1, armorUp = 0, shieldUp = 0,
                           attackType = "focusing", splashIndex = 2){
  
  unit <- which(star$Unit == unit) # attacking unit Unit의 행 번호 
  target <- which(star$Unit == target) # target Unit의 행 번호 
  
  
  # 못잡는 유닛의 경우 Inf의 시간이 걸린다. 
  if ((star[target,"OnAir"] & star[unit,"AirAttack"] == 0) |
      (!star[target,"OnAir"] & star[unit,"GroundAttack"] == 0)){
    return (Inf)
  }
  
  # 공중에 있을 땐 공중 데미지 적용, 지상에 있을 땐 지상 데미지 적용 
  damage <- ifelse(star[target,"OnAir"], star[unit,"AirAttack"], star[unit,"GroundAttack"])
  # 사이즈와 데미지 타입을 고려한 데미지 감소를 고려 
  damage <- damage * damagePercentage(
    star[target,"Size"],
    star[unit,ifelse(star[target,"OnAir"],"AttackType_A","AttackType_G")])
  # 데미지 
  damage <- damage - star[target,"Armor"]
  # 공격하는 유닛의 수 (focusing)
  damage <- damage * num1 
  
  # 공중 쿨 타임? 지상 쿨 타임?
  cooltime <- star[unit,ifelse(star[target,"OnAir"],"Cooldown_A","Cooldown_G")]
  
  # 쉴드 회복과 체력 회복 
  hpRecoverTime <- ifelse(star[unit,"Race"] == "Zerg", 2.5, 0)
  sdRecoverTime <- ifelse(star[unit,"Race"] == "Protoss", 1.5, 0)
  hpRecoverCycle <- 0;
  sdRecoverCycle <- 0;
  
  time <- 0
  hp <- star[target,"HP"]
  sd <- star[target,"Shield"]
  hps <- data.frame(hp = rep(hp,num2), sd = rep(sd,num2))
  
  
  while(TRUE){
    # 시간 
    time <- time + cooltime;
    
    # --체력
    hps[1,"sd"] <- hps[1,"sd"] - damage # 데미지는 쉴드부터 깎음.
    if (hps[1,"sd"] < 0){ # 쉴드를 관통해서 데미지가 들어감. 
      hps[1,"hp"] <- hps[1,"hp"] + hps[1,"sd"] # 깎인 만큼 hp에 더함.
      hps[1,"sd"] <- 0 # 쉴드는 0
    }
    
    # 회복 
    if (time >= (hpRecoverCycle + 1) * hpRecoverTime ) {
      hps[,"hp"] <- hps[,"hp"] + 1;
      hpRecoverCycle <- hpRecoverCycle + 1;
    }
    if (time >= (sdRecoverCycle + 1) * sdRecoverTime ) {
      hps[,"sd"] <- hps[,"sd"] + 1;
      sdRecoverCycle <- sdRecoverCycle + 1;
    }   
    # 회복하더라도 최고 체력, 최고 실드를 초과할 수는 없다. 
    for (i in 1:nrow(hps)){
      if (hps[i,"hp"] > hp) hps[i,"hp"] <- hp
      if (hps[i,"sd"] > sd) hps[i,"sd"] <- sd
    } 
   
    # hp = 0 이 된 유닛은 죽은 유닛이므로 dataFrame에서 잘라낸다. 
    # 모든 유닛이 죽고 dataFrame의 길이가 1이라면 반복문에서 탈출한다. 
    if (hps[1,"hp"] <= 0 & nrow(hps) > 1) hps = slice(hps,2:nrow(hps))
    else if (hps[1,"hp"] <= 0 & nrow(hps) == 1) break;
    
  }
  return (time)
}

time08 <- attackTime("Dragoon","Marine",2, 1); time08 # 3.747 # 세방 
time09 <- attackTime("Dragoon","Marine",3, 1); time09 # 2.498 # 두 방 
time10 <- attackTime("Dragoon","Marine",4, 1); time10 # 2.498 # 두 방 
time11 <- attackTime("Dragoon","Marine",5, 1); time11 # 1.249 # 한 방 
time12 <- attackTime("Dragoon","Overlord",2, 1); time12 # 7.494 
time13 <- attackTime("Dragoon","Overlord",3, 1); time13 # 4.996 
time14 <- attackTime("Dragoon","Hydralisk",3, 2); time14 # 4.996 
time15 <- attackTime("Dragoon","Hydralisk",5, 10); time15 # 24.98
time16 <- attackTime("Hydralisk","Dragoon",10, 5); time16 # 9.36

whoWin01 <- function(unit1,unit2,num1=1,num2=1){
  time01 <- attackTime(unit1,unit2,num1,num2)
  time02 <- attackTime(unit2,unit1,num2,num1)
  if (time01 > time02) return (unit2)
  else if (time01 < time02) return (unit1)
  else return (NULL)
}

whoWin01("Dragoon","Hydralisk",3,6) # "Hydralisk"
whoWin01("Dragoon","Hydralisk",3,5) # "Hydralisk"
whoWin01("Dragoon","Hydralisk",3,4) # "Dragoon"
whoWin01("Dragoon","Hydralisk",2,4) # "Hydralisk"
whoWin01("Dragoon","Hydralisk",2,3) # "Hydralisk"
whoWin01("Dragoon","Hydralisk",2,2) # "Dragoon"
whoWin01("Dragoon","Hydralisk",1,2) # "Hydralisk"
whoWin01("Dragoon","Siege Tank (Siege)",5,2) # Dragoon
whoWin01("Dragoon","Siege Tank (Siege)",5,7) # Dragoon # Splash가 전혀 고려되지 않은 경우.
whoWin01("Dragoon","Marine",1,2) # Dragoon
whoWin01("Dragoon","Marine",1,3) # Marine
whoWin01("Dragoon","Marine",3,5) # Dragoon
whoWin01("Dragoon","Firebat",1,2) # Dragoon
whoWin01("Dragoon","Firebat",1,3) # Firebat
whoWin01("Dragoon","Firebat",1,2) # Dragoon 
whoWin01("Dragoon","Zealot", 1,1) # Zealot
whoWin01("Dragoon","Zealot", 2,1) # Dragoon
whoWin01("Dragoon","Zealot", 3,2) # Dragoon
whoWin01("Dragoon","Zealot", 4,3) # Zealot
whoWin01("Dragoon","Zergling", 1,2) # Zergling  
whoWin01("Dragoon","Zergling", 2,3) # Zergling 
whoWin01("Dragoon","Reaver", 2,1) # Dragoon 
whoWin01("Dragoon","Dark Templar", 2,1) # Dragoon 
whoWin01("Dragoon","Guardian", 1,1) # Dragoon
whoWin01("Dragoon","Battlecruiser", 2,1) # NULL
whoWin01("Dragoon","Battlecruiser", 3,1) # Dragoon 
whoWin01("Dragoon","Siege Tank (Tank)", 1,1) #  "Siege Tank (Tank)"
whoWin01("Dragoon","Siege Tank (Tank)", 2,1) # Dragoon 
whoWin01("Dragoon","Mutalisk", 1,1) # Dragoon
whoWin01("Dragoon","Mutalisk", 1,2) # Mutalisk
whoWin01("Dragoon","Mutalisk", 5,6) # Dragoon
whoWin01("Dragoon","Ultralisk", 3,1) # Dragoon

###########################################################################



# 표 만들기 
# 1. 드라군 < 유닛 일때, 유닛을 제압하기 위해 필요한 드라군의 마리수
data01 <- data.frame(
  Unit = c("Dark Templar","Reaver","Battlecruiser","Zealot","Ultralisk"),
  Count = c(2,2,3,1.5,3))
data02 <- data.frame(
  Unit = c("Hydralisk","Siege Tank (Tank)","Mutalisk","Marine","Zergling"),
  Count = c(3/4,2,5/6,3/5,2/3)
)
data03 <- rbind(data01,data02); data03

barplot(data03$Count, main = "노업 드라군 몇 마리로 제압하는가?", 
        xlab = "유닛", ylab = "드라군 수", names = data03$Unit)


# 2. 드라군의 가성비, 이대로 좋은가?
# 비교 대상이 되는 유닛 서브셋을 만들고, (가스 + 미네랄)인 "cost" 열을 만든다.
starSub01 <- star %>%
  filter(Unit %in% data03$Unit) %>%
  mutate(cost = Mineral + Gas)

# subset을 만드는 대표적인 두 가지 방법 
# row 개수, 단위를 data03와 맞춰준다. 
costy01 <- data.frame(Unit = starSub01$Unit ,Cost = starSub01$cost)
costy02 <- subset(starSub01,select = c("Unit","cost"))
?subset

# data03 에 소비된 드라군의 생산자원량, dragoonCost를 집어넣는다. 
# 1. 해당 유닛 cost를 data03에 붙인다. 
# 2. 소모된 드라군 수에 따른 자원량을 적는다. 
# 3. CostRate를 구한다. 
data03 <- left_join(data03,costy01,by = "Unit")
data03 <- data03 %>% 
    mutate(DragoonCost = (star[star$Unit == "Dragoon","Mineral"]
         +star[star$Unit == "Dragoon","Gas"]) * Count) %>%
      mutate(CostRate = Cost/DragoonCost)
data03[data03$Unit == "Siege Tank (Tank)","Unit"] <- "Tank"
barplot(data03$CostRate, main = "드라군 효율비", xlab = "다른 유닛", ylab = "드라군 소모 자원 대비 유닛 소모 자원", names = data03$Unit, ylim = c(0,1.5))

