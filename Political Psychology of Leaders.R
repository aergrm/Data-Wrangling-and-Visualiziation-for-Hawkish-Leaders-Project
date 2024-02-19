library(tidyverse)

gmean<-c(0.360475,	0.15265,	0.258709579,	1.360470999,	0.840644907,	0.431294947,	0.013930976,	0.235009576,	0.185312878,	0.118614389,	0.235009576,	0.19455,	0.035191667,	0.189357234,	0.142895737,	0.972732308,0.857104263)
shmean<-c(0.4227,	0.174,	0.288686785,	0.48639165,	0.470610331,	0.488360618,	0.080363352,	0.092758754,	0.142546412,	0.488360618,	0.092758754,	0.165309091,	0.030154546,	0.151912024,	0.111643044,	0.986167351,0.888356956)
small<-cbind(gmean,shmean)

small<-as_tibble(small,digits=2,nsmall=2)
small$Index<-c('I-1 Strategic Approach to Goals (Cooperative/Conflictual)'
               ,'I-2 Tactical Pursuit of Goals (Cooperative/Conflictual'
               ,'I-3 Risk Orientation (Averse/Acceptant)'
               ,'I-4 Timing of Action: a. Cooperation/Conflict'
               ,' b.Words/Deeds'
               ,'I-5 Utility of Means: a. Reward'
               ,' b. Promise'
               ,' c. Appeal/Support'
               ,' d. Oppose/resist'
               ,' e. Threaten'
               ,' f. Punish',
               'P-1 Nature of the Political Universe (Friendly/Hostile)'
               ,'P-2 Realization of Political Values (Pessimistic/Optimistic)'
               ,'P-3 Predictability of Political Future (Low/High)'
               ,'P-4 Self Control Over Historical Development (Low/High)'
               ,'P-5 Role of Chance','P-4b Other Control Over Historical Development (Low/High)'
)
norming<-c(0.533074276,	0.238940296,	0.244714337,	0.464919035,	0.557420548,	0.54038325,	0.043841553	,0.182312335	,0.10864493,	0.024916251,	0.099901681,	0.346644398,	0.160756315,	0.120346185,	0.198843717,	0.975803279,	0.801156283
)
small$Norming<-norming


nsdev<-c(0.167816973,	0.097085053,	0.101770276,	0.161252799,	0.140246564,	0.10588391,	0.029398396	,0.065384759,	0.047606497,	0.025283859	,0.043850886,	0.124657729,	0.088876338,	0.026901789,	0.078159064	,0.012164573	,0.078159064)
small$Normdev<-nsdev
small<-small%>%relocate(Index,.before = gmean)
colnames(small)<-c("Index","Gurion","Sharett","Norming Group","Norming Sd")

z_scoresgur <- ((small$Gurion-small$`Norming Group`)/small$`Norming Sd`)
z_scoressh <- ((small$Sharett-small$`Norming Group`)/small$`Norming Sd`)
z_scoressh[c(1,12,15,17)]

z_scoresgur<-c(-0.094244186,	-0.090535713,	-0.300370578,	2.73079936,	1.211076872,	-0.160284074,	-0.521957466,	0.435807687,	0.171108621,	1.322099823,	0.842531343,	-0.372202797,	-0.52003876,	0.768850466,	-0.638616243,	0.127900229,	0.638616243)
z_scoressh<-c(0.050465116,	-0.014285716,	-0.177513177,	-0.052892835,	0.021255084,	0.088910997	,0.036735291,	-0.224619743,	-0.053272502,	6.310564144,	-0.151505877,	-0.474443738,	0,	0,	-0.884700441,	0,	0.884700441)
I1g<-0.360
P1g<- 0.195
P4g<- 1.43
P4bg<- -1.43

I1s<-0.423
P1s<- 0.165
P4s<- 0.112
P4bs<- -0.627

# a data frame with the coordinates
I1gur<-z_scoresgur[c(1)]
P1gur<-z_scoresgur[c(12)]
P4gur<-z_scoresgur[c(15)]
P4bgur<--z_scoresgur[c(15)]


I1sh<-z_scoressh[c(1)]
P1sh<-z_scoressh[c(12)]
P4sh<-z_scoressh[c(15)]
P4bsh<--z_scoressh[c(15)]

data <- data.frame(x = c(P4gur, P4sh, P4bgur, P4bsh), y = c(I1gur, I1sh, P1gur, P1sh), label = c("Gurion's Self", "Sharett's Self", "Gurion's Other", "Sharett's Other"))
data
z_scoresgur[c(1,12,15,15)]
z_scoressh[c(1,12,15,15)]

names(wf[wf=="TT Times New Roman"])
library(extrafont)
font_import()
loadfonts(device = "win")
windowsFonts(sans = windowsFont("Times"))

# the plot with the points and the origin line
ggplot(data, aes(x = x, y = y)) + 
  geom_point(size = 2, color = "blue") +
  xlim(-1, 1) + ylim(-1, 1) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_text(aes(label = label), hjust = 0.6, vjust = -0.4) +
  labs(x = "I1 - P1", y = "P4 - P4b", #title = "Leadership Typologies"
       )+
  annotate("text", x = -1, y = 1, label = "Type A - Idealist", size = 4, hjust = 0) +
  annotate("text", x = -1, y = -1, label = "Type DEF - Realist", size = 4, hjust = 0) +
  annotate("text", x = 1, y = 1, label = "Type C - Idealist", size = 4, hjust = 1) +
  annotate("text", x = 1, y = -1, label = "Type D - Realist", size = 4, hjust = 1)+
  theme_minimal() 

datao
datao <- data.frame(x = c(P4bgur, P4bsh), y = c(P1gur, P1sh), label = c("Gurion's Other (0.6, -0.3)", "Sharett's Other (0.8, -0.47)"))

ggplot(datao, aes(x = x, y = y)) + 
  geom_point(size = 1, color = "blue") +
  xlim(-1, 1) + ylim(-1, 1) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_text(aes(label = label), hjust = 1.04, vjust = 0.2) +
  labs(x = "I1 - P1", y = "P4 - P4b") +
       #title = "Leadership Typologies",
      
  annotate("text", x = -1, y = 1, label = "Type A - Idealist", size = 4, hjust = 0) +
  annotate("text", x = -1, y = -1, label = "Type DEF - Realist", size = 4, hjust = 0) +
  annotate("text", x = 1, y = 1, label = "Type C - Idealist", size = 4, hjust = 1) +
  annotate("text", x = 1, y = -1, label = "Type D - Realist", size = 4, hjust = 1)+
  theme_minimal()

datas <- data.frame(x = c(P4gur, P4sh), y = c(I1gur, I1sh), label = c("Gurion's Self (-0.6, -0.9)", "Sharett's Self (-0.8, 0.05)"))
datas
ggplot(datas, aes(x = x, y = y)) + 
  geom_point(size = 1, color = "blue") +
  xlim(-1, 1) + ylim(-1, 1) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_text(aes(label = label), hjust = 0.3, vjust = -0.3) +
  labs(x = "I1 - P1", y = "P4 - P4b")+ 
       #title = "Leadership Typologies",
      
  annotate("text", x = -1, y = 1, label = "Type A - Idealist", size = 4, hjust = 0) +
  annotate("text", x = -1, y = -1, label = "Type DEF - Realist", size = 4, hjust = 0) +
  annotate("text", x = 1, y = 1, label = "Type C - Idealist", size = 4, hjust = 1) +
  annotate("text", x = 1, y = -1, label = "Type D - Realist", size = 4, hjust = 1)+
  theme_minimal()
small

#F Welch Test
ggbetweenstats(
  data = sm,
  x = I1,
  y = shmean,
  type = "parametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",ylab = "Low - High",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,results.subtitle = FALSE,violin.args = list(width = 0),
  bf.message = FALSE,xlab = 'Leaders',var.equal = TRUE,#title ="Belief of Control over Historical Flow Results (P4)"
)
ncol(datas)
meang<-pivot_wider(small, names_from = Index, values_from = c("gmean","shmean"))

names(gmean)<-c("I1",    "I2" ,   "I3new", "I4a",   "I4b",   "I5ap" , "I5pr" , "I5re" ,
                    "I5op",  "I5th",  "I5pu",  "P1",    "P2",    "P3",    "P4" ,   "P5" ,  
                    "P.4b")

colnamsenames(shmean)<-c("I1",    "I2" ,   "I3new", "I4a",   "I4b",   "I5ap" , "I5pr" , "I5re" ,
                            "I5op",  "I5th",  "I5pu",  "P1",    "P2",    "P3",    "P4" ,   "P5" , 
                 "P.4b")  
smean<-data.frame(x=shmean, y=indexs)
gmeans<-data.frame(x=gmean,y=indexs)
gmean<-as.data.frame(gmean)
shmean<-as.data.frame(shmean)
t.test(gmeans,shmean)
library(tidyverse)
smallnew<-t(small)
sm <- as.data.frame(t(small))

gsmeans<-pivot_wider(gmeans,names_from = y, values_from = x)

smeans<-pivot_wider(smean, names_from = y, values_from = x)


t.test(gsmeans$I1,smeans$I1)
