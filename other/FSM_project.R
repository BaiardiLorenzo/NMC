library(gRbase)
library(gRain)
library(gRim)

rm(list=ls(all=TRUE))
setwd("./DataSpellProjects/NMC-Baiardi_Lorenzo")

#NMC
#study the effect of personal activity to prevent cardiovascular diseas events; see Sjolander et al. (2009)
load("nmc.RData")

bmi_bin = nmc$bmi = as.numeric(nmc$bmi>=30)
str(nmc)

#cvd: primary outcome of interest, having/not having at least 1 event of cardiovascular events
#pa: treatment about self-reported personal activities, 1: low-level exerciser, 0: high-level exerciser 
#bmi: body mass index
#bmi_bin: dichotomized bmi
#sexMale <- ifelse(nmc$sex == 'Male', 1, 0)
#sexFemale <- ifelse(nmc$sex == 'Female', 1, 0)

#ordinal variable
#fit_ord <- as.numeric(ordered(nmc$fitness, c('Much Worse', 'Little Worse', 'Just as good', 'A bit better', 'Much better')))
#smoke_ord <- as.numeric(ordered(nmc$smoke, c('NO', 'Former', 'Current')))
#alc_ord <- as.numeric(ordered(nmc$alc, c('Never', 'Low', 'Medium', 'High')))

#nmc$sex = ifelse(nmc$sex=="Male", " Male", " Female")

#show cvd based on age
plot(nmc$age, nmc$cvd, xlab="Age", ylab="CVD")

#values for every variable in glm
valuesAge <- glm(nmc$cvd ~ nmc$age, family = binomial)
summary(valuesAge)
#la variabile età sembrerebbe essere significativa

valuesSex <- glm(nmc$cvd ~ nmc$sex, family = binomial)
summary(valuesSex)
#la variabile sex sembrerebbe essere significativa, dove masdchio vale 0.91 in positivo, quindi il maschio più propenso alla cvd

valuesBmi_bin <- glm(nmc$cvd ~ nmc$bmi, family = binomial)
summary(valuesBmi_bin)
#la variabile bmi sembra essere significativa influendo negativa alla fuoriuscita di cvd di 0.25

valuesFitness <- glm(nmc$cvd ~ nmc$fitness, family = binomial)
summary(valuesFitness)
#la variabile fitness sembra significativa

valuesPa <- glm(nmc$cvd ~ nmc$pa, family = binomial)
summary(valuesPa)
#la variabile pa non è significativa

valuesSmoke <- glm(nmc$cvd ~ nmc$smoke, family = binomial)
summary(valuesSmoke)
#la variabile smoke sembra poco significativa unicamente per SmokeFormer, la variabile sembra non significativa

valuesAlc <- glm(nmc$cvd ~ nmc$alc, family = binomial)
summary(valuesAlc)
#la variabile alcol sembra significativa

#glm for all variable
#Verifichiamo il modello con tutte le variabili
valuesAll <- glm(nmc$cvd ~ nmc$sex+nmc$age+nmc$bmi+nmc$fitness+nmc$pa+nmc$smoke+nmc$alc, family = binomial)
summary(valuesAll)
#le variabili non significative osservate sono: alcool e pa. Eccezzione per fitness Much Better con pvalue circa 0.056, rientrando nel significativo

#variabili con interazione doppia
valuesInteraction <- glm(nmc$cvd ~ nmc$sex+nmc$age+nmc$bmi+nmc$fitness+nmc$pa+nmc$smoke+nmc$alc+
                           nmc$sex*nmc$age+nmc$sex*nmc$bmi+nmc$sex*nmc$fitness+nmc$sex*nmc$pa+nmc$sex*nmc$smoke+nmc$sex*nmc$alc+
                           nmc$age*nmc$bmi+nmc$age*nmc$fitness+nmc$age*nmc$pa+nmc$age*nmc$smoke+nmc$age*nmc$alc+
                           nmc$bmi*nmc$fitness+nmc$bmi*nmc$pa+nmc$bmi*nmc$smoke+nmc$bmi*nmc$alc+
                           nmc$fitness*nmc$pa+nmc$fitness*nmc$smoke+nmc$fitness*nmc$alc+
                           nmc$pa*nmc$smoke+nmc$pa*nmc$alc+
                           nmc$smoke*nmc$alc, family = binomial)
summary(valuesInteraction)
#correlation between smoke and bmi: Sembrerre esserci una minima correlazione tra questi valori// Ma non ha alcun valore significativo
valuesSMOKE_BMI <- glm(nmc$cvd ~ nmc$sex+nmc$age+nmc$bmi+nmc$fitness+nmc$smoke+nmc$smoke*nmc$bmi, family = binomial)
summary(valuesSMOKE_BMI)
pstimaSMOKE_BMI <- valuesSMOKE_BMI$fitted.values
plot(nmc$age, pstimaSMOKE_BMI, xlab="Age", ylab="Pstima",col="blue")

#LEGENDA:
#fitness: 1-MUCH WORSE, 2-LITTLE WORSE, 3-JUST AS GOOD, 
#         4-A BIT BETTER, 5-MUCH BETTER
#smoke: 1-NO, 2-FORMER, 3-CURRENT
#alc: 1-NEVER, 2-LOW, 3-MEDIUM, 4-HIGH

ord_fit = c('Much Worse', 'Little Worse', 'Just as good',
            'A bit better', 'Much better')
ord_smoke = c('NO', 'Former', 'Current')
ord_alc =  c('Never', 'Low', 'Medium', 'High')

#ordinal variable
nmc$fitness <- as.numeric(ordered(nmc$fitness, ord_fit))
nmc$smoke <- as.numeric(ordered(nmc$smoke, ord_smoke))
nmc$alc <- as.numeric(ordered(nmc$alc, ord_alc))

str(nmc)
@
  
  Modello con differenti tipi di fitness.
<<>>=
  #Modello nel caso di Fitness: Much Worse, Little Worse, 
  #                             Just as good, A bit better, 
  #                             Much better.
  #Fitness: MuchWorse
  fitFitMW <- glm(nmc$cvd[nmc$fitness==1] ~ 
                    nmc$sex[nmc$fitness==1]+
                    nmc$age[nmc$fitness==1]+
                    nmc$bmi[nmc$fitness==1]+
                    nmc$smoke[nmc$fitness==1], 
                  family = binomial)
summary(fitFitMW)
pstimaFitMW <- fitFitMW$fitted.values

#Fitness: Little Worse
fitFitLW <- glm(nmc$cvd[nmc$fitness==2] ~ 
                  nmc$sex[nmc$fitness==2]+
                  nmc$age[nmc$fitness==2]+
                  nmc$bmi[nmc$fitness==2]+
                  nmc$smoke[nmc$fitness==2], 
                family = binomial)
summary(fitFitLW)
pstimaFitLW <- fitFitLW$fitted.values

#Fitness: Just as Good
fitFitJG <- glm(nmc$cvd[nmc$fitness==3] ~ 
                  nmc$sex[nmc$fitness==3]+
                  nmc$age[nmc$fitness==3]+
                  nmc$bmi[nmc$fitness==3]+
                  nmc$smoke[nmc$fitness==3], 
                family = binomial)
summary(fitFitJG)
pstimaFitJG <- fitFitJG$fitted.values

#Fitness: A bit better
fitFitBB <- glm(nmc$cvd[nmc$fitness==4] ~ 
                  nmc$sex[nmc$fitness==4]+
                  nmc$age[nmc$fitness==4]+
                  nmc$bmi[nmc$fitness==4]+
                  nmc$smoke[nmc$fitness==4], 
                family = binomial)
summary(fitFitBB)
pstimaFitBB <- fitFitBB$fitted.values

#Fitness: Much Better
fitFitMB <- glm(nmc$cvd[nmc$fitness==5] ~ 
                  nmc$sex[nmc$fitness==5]+
                  nmc$age[nmc$fitness==5]+
                  nmc$bmi[nmc$fitness==5]+
                  nmc$smoke[nmc$fitness==5], 
                family = binomial)
summary(fitFitMB)
pstimaFitMB <- fitFitMB$fitted.values

#Plot
plot(nmc$age[nmc$fitness==1], nmc$cvd[nmc$fitness==1], 
     xlab="Age", ylab="CVD", col="blue")
points(nmc$age[nmc$fitness==2], nmc$cvd[nmc$fitness==2], col="red")
points(nmc$age[nmc$fitness==3], nmc$cvd[nmc$fitness==3], col="green")
points(nmc$age[nmc$fitness==2], nmc$cvd[nmc$fitness==4], col="purple")
points(nmc$age[nmc$fitness==3], nmc$cvd[nmc$fitness==5], col="orange")
points(nmc$age[nmc$fitness==1], pstimaFitMW, col="blue")
points(nmc$age[nmc$fitness==2], pstimaFitLW, col="red")
points(nmc$age[nmc$fitness==3], pstimaFitJG, col="green")
points(nmc$age[nmc$fitness==4], pstimaFitBB, col="purple")
points(nmc$age[nmc$fitness==5], pstimaFitMB, col="orange")
legend(x = "left", 
       legend = c("Much Worse", "Little Worse", "Just as Good", 
                  "A bit better", "Much Better"), 
       lty=c(1, 1, 1, 1, 1),
       col=c("blue","red", "green", "purple", "orange"), lwd = 1)

#selection based on p-value for significant variables
#selezioniamo solo le variabili significative dal precedente modello
#ed eseguiamo il plot del modello selezionato
valuesSignificant <- glm(nmc$cvd ~ nmc$sex+nmc$age+nmc$bmi+nmc$fitness+nmc$smoke, family = binomial)
summary(valuesSignificant)
pstima <- valuesSignificant$fitted.values
plot(nmc$age, nmc$cvd, xlab="Age", ylab="CVD")
points(sort(nmc$age), pstima[order(nmc$age)], col="blue")

#Visualizzazione di differenze
#divided by sex ~ Exponential
valuesSelectedMale <- glm(nmc$cvd[nmc$sex=="Male"] ~ nmc$age[nmc$sex=="Male"]+nmc$bmi[nmc$sex=="Male"]+nmc$fitness[nmc$sex=="Male"]+nmc$smoke[nmc$sex=="Male"], family = binomial)
summary(valuesSelectedMale)
pstimaMale <- valuesSelectedMale$fitted.values

valuesSelectedFemale <- glm(nmc$cvd[nmc$sex=="Female"] ~ nmc$age[nmc$sex=="Female"]+nmc$bmi[nmc$sex=="Female"]+nmc$fitness[nmc$sex=="Female"]+nmc$smoke[nmc$sex=="Female"], family = binomial)
summary(valuesSelectedFemale)
pstimaFemale <- valuesSelectedFemale$fitted.values

plot(nmc$age[nmc$sex=="Male"], nmc$cvd[nmc$sex=="Male"], xlab="Age", ylab="CVD", col="blue")
points(nmc$age[nmc$sex=="Female"], nmc$cvd[nmc$sex=="Female"], col="red")
points(nmc$age[nmc$sex=="Male"], pstimaMale, col="blue")
points(nmc$age[nmc$sex=="Female"], pstimaFemale, col="red")


#divide by smoke ~ SIMILI, il fumo non incide nella variabile
valuesSMOKE_NO <- glm(nmc$cvd[nmc$smoke=="NO"] ~ nmc$sex[nmc$smoke=="NO"]+nmc$age[nmc$smoke=="NO"]+nmc$bmi[nmc$smoke=="NO"]+nmc$fitness[nmc$smoke=="NO"], family = binomial)
summary(valuesSMOKE_NO)
pstimaSmoke_NO <- valuesSMOKE_NO$fitted.values

valuesSMOKE_FORMER <- glm(nmc$cvd[nmc$smoke=="Former"] ~ nmc$sex[nmc$smoke=="Former"]+nmc$age[nmc$smoke=="Former"]+nmc$bmi[nmc$smoke=="Former"]+nmc$fitness[nmc$smoke=="Former"], family = binomial)
summary(valuesSMOKE_FORMER)
pstimaSmoke_Former <- valuesSMOKE_FORMER$fitted.values

valuesSMOKE_CURRENT <- glm(nmc$cvd[nmc$smoke=="Current"] ~ nmc$sex[nmc$smoke=="Current"]+nmc$age[nmc$smoke=="Current"]+nmc$bmi[nmc$smoke=="Current"]+nmc$fitness[nmc$smoke=="Current"], family = binomial)
summary(valuesSMOKE_CURRENT)
pstimaSmoke_Current <- valuesSMOKE_CURRENT$fitted.values

plot(nmc$age[nmc$smoke=="NO"], pstimaSmoke_NO, xlab="Age", ylab="CVD", col="blue")
points(nmc$age[nmc$smoke=="Former"], pstimaSmoke_Former, col="red")
points(nmc$age[nmc$smoke=="Current"], pstimaSmoke_Current, col="green")

#divide by pa ~ SIMILI, il PA non incide nella variabile
valuesPA_0 <- glm(nmc$cvd[nmc$pa=="0"] ~ nmc$sex[nmc$pa=="0"]+nmc$age[nmc$pa=="0"]+nmc$bmi[nmc$pa=="0"]+nmc$fitness[nmc$pa=="0"]+nmc$smoke[nmc$pa=="0"], family = binomial)
summary(valuesPA_0)
pstimaPA_0 <- valuesPA_0$fitted.values

valuesPA_1 <- glm(nmc$cvd[nmc$pa=="1"] ~ nmc$sex[nmc$pa=="1"]+nmc$age[nmc$pa=="1"]+nmc$bmi[nmc$pa=="1"]+nmc$fitness[nmc$pa=="1"]+nmc$smoke[nmc$pa=="1"], family = binomial)
summary(valuesPA_1)
pstimaPA_1 <- valuesPA_1$fitted.values

plot(nmc$age[nmc$pa=="0"], pstimaPA_0, xlab="Age", ylab="CVD", col="blue")
points(nmc$age[nmc$pa=="1"], pstimaPA_1, col="red")

#SELEZIONE DEL MODELLo

#BACKWARD
#AIC
back_aic <- step(valuesAll, direction="backward", trace=0, k=2)
formula(back_aic)
summary(back_aic)
#BIC 
back_bic <- step(valuesAll, direction="backward", k=log(length(nmc$cvd)))
formula(back_bic)
summary(back_bic)

#FORWARD
fit0 <- glm(nmc$cvd ~ 1, family= "binomial")
#AIC 
for_aic <- step(fit0, scope=formula(valuesAll), direction="forward", trace=0, k=2)
formula(for_aic)
summary(for_aic)
#BIC
for_bic <- step(fit0, scope=formula(valuesAll), direction="forward", trace=0, k=log(length(nmc$cvd)))
formula(for_bic)
summary(for_bic)

#BOTH
#AIC 
both_aic <- step(fit0, scope=formula(valuesAll), direction="both", trace=0, k=2)
summary(both_aic)
#BIC
both_bic <- step(fit0, scope=formula(valuesAll), direction="both", trace=0, k=log(length(nmc$cvd)))
summary(both_bic)

#GRAPHIC
#TABLE VISUALIZATION
#Change fitness to ordinal: 1-Much Worse, 2-Little Worse, 3-Just as good, 4-A bit better, 5-Muche Better
temp <- nmc$fitness
nmc$fitness <- as.numeric(ordered(nmc$fitness, c('Much Worse', 'Little Worse', 'Just as good', 'A bit better', 'Much better')))
ftable(nmc$sex+nmc$bmi+nmc$fitness ~ nmc$pa+nmc$smoke+nmc$alc, nmc)
as.data.frame(nmc)


nmc$fitness <- temp


sex.male <- (nmc$sex=='Male')
sex.female <- (nmc$sex=='Female')
male.data <- nmc[sex.male, c(2:8)]
female.data <- nmc[sex.female, c(2:3, 5:8)]

maleplot <- dmod(~.^., data=male.data)
malestep <- stepwise(maleplot, type='unrestricted')
plot(malestep)
                                     

#variabili con interazione doppia
fit.int <- glm(nmc$cvd ~ nmc$sex+nmc$age+nmc$bmi+
                 nmc$fitness+nmc$pa+nmc$smoke+
                 nmc$alc+nmc$sex*nmc$age+
                 nmc$sex*nmc$bmi+nmc$sex*nmc$fitness+
                 nmc$sex*nmc$pa+nmc$sex*nmc$smoke+
                 nmc$sex*nmc$alc+nmc$age*nmc$bmi+
                 nmc$age*nmc$fitness+nmc$age*nmc$pa+
                 nmc$age*nmc$smoke+nmc$age*nmc$alc+
                 nmc$bmi*nmc$fitness+nmc$bmi*nmc$pa+
                 nmc$bmi*nmc$smoke+nmc$bmi*nmc$alc+
                 nmc$fitness*nmc$pa+nmc$fitness*nmc$smoke+
                 nmc$fitness*nmc$alc+nmc$pa*nmc$smoke+
                 nmc$pa*nmc$alc+nmc$smoke*nmc$alc,
               family=binomial)
summary(fit.int)

#correlation between smoke and bmi: Sembrerre esserci una 
#minima correlazione tra questi valori, ma non ha alcun 
#valore significativo
fit.smokebmi <- glm(nmc$cvd ~ nmc$sex+nmc$age+nmc$bmi+
                      nmc$fitness+nmc$smoke+nmc$smoke*nmc$bmi,
                    family=binomial)
summary(fit.smokebmi)
pstima.smokebmi <- fit.smokebmi$fitted.values
plot(nmc$age, pstima.smokebmi, xlab="Age", ylab="Pstima",col="blue")
@                                                     
  
  
  
  
  Analizziamo ora il modello nel caso di differenti PA.

<<>>=
  #Modello nel caso di PA: 0, 1.
  #PA: 0
  fit.pa0 <- glm(nmc$cvd[nmc$pa==0] ~ 
                   nmc$sex[nmc$pa==0]+
                   nmc$age[nmc$pa==0]+
                   nmc$bmi[nmc$pa==0]+
                   nmc$fitness[nmc$pa==0]+
                   nmc$smoke[nmc$pa==0], 
                 family=binomial)
summary(fit.pa0)
pstima.pa0 <- fit.pa0$fitted.values

#PA: 1
fit.pa1 <- glm(nmc$cvd[nmc$pa==1] ~ 
                 nmc$sex[nmc$pa==1]+
                 nmc$age[nmc$pa==1]+
                 nmc$bmi[nmc$pa==1]+
                 nmc$fitness[nmc$pa==1]+
                 nmc$smoke[nmc$pa==1], 
               family=binomial)
summary(fit.pa1)
pstima.pa1 <- fit.pa1$fitted.values

#Plot
plot(nmc$age[nmc$pa==0], nmc$cvd[nmc$pa==0], 
     xlab="Age", ylab="CVD", col="blue")
points(nmc$age[nmc$pa==1], nmc$cvd[nmc$pa==1], col="red")
points(sort(nmc$age[nmc$pa==0]), pstima.pa0[order(nmc$age[nmc$pa==0])], col="blue")
points(sort(nmc$age[nmc$pa==1]), pstima.pa1[order(nmc$age[nmc$pa==1])], col="red")
legend(x="left", 
       legend=c("High-level exerciser", "Low-level exerciser"),
       lty=c(1, 1), col=c("blue","red"), lwd = 1)
@
  
  Il grafico ci mostra come ci sia una concentrazione maggiore di casi negli
individui più anziani che praticano esercizi di alto livello rispetto a quelli
che praticano esercizi di basso livello.

nmc.nocvd <- nmc[c(1:3, 5:8)]
str(nmc)
str(nmc.nocvd)


<<Grafo per generi>>=
  #Considero grafici in base al genere
  #Male
  male.data <- nmc[(nmc$sex=='Male'), c(2:8)]
nmc.male <- dmod(~.^., data=male.data)
m.male <- stepwise(nmc.male, type='unrestricted')
plot(m.male)

#Female
female.data <- nmc[(nmc$sex=='Female'), c(2:8)]
nmc.female <- dmod(~.^., data=female.data)
m.female <- stepwise(nmc.female, type='unrestricted')
plot(m.female)
@
  
  
  <<Grafo decomposto e unrestricted>>=
  m.decomposable <- stepwise(ind.nmc, k=log(length(nmc$cvd)),
                             type="decomposable",
                             direction="forward",
                             details=0)
m.unrestricted <- stepwise(ind.nmc, k=log(length(nmc$cvd)),
                           type="unrestricted",
                           direction="forward",
                           details=0)
plot(m.decomposable)
plot(m.unrestricted)
@
  
  <<Grafo BIC>>=
  #BIC
  m.sat.BIC <- stepwise(sat.nmc, k=log(length(nmc$cvd)))
plot(m.sat.BIC)
@
  
  <<Grafo AIC>>=
  #AIC
  m.sat.AIC <- stepwise(sat.nmc)
plot(m.sat.AIC)
@
  
  
  
  \subsection{PA e Age}
<<Modello con interazione PA e Age>>=
  #Modello con interazione PA e Age
  fit.sexsmoke <- glm(nmc$cvd ~ nmc$sex+nmc$age+nmc$bmi+
                        nmc$fitness+nmc$smoke+
                        nmc$pa*nmc$age,
                      family=binomial)
summary(fit.sexsmoke)
@
  
  Non è verificata l'interazione fra le variabili PA e Age.
  
  \subsection{PA e Fitness}
  <<Modello con interazione PA e Fitness>>=
  #Modello con interazione PA e Fitness
  fit.sexsmoke <- glm(nmc$cvd ~ nmc$sex+nmc$age+nmc$bmi+
                      nmc$fitness+nmc$smoke+
                      nmc$pa*nmc$fitness,
                      family=binomial)
  summary(fit.sexsmoke)
  @
  
  Non c'è interazione fra le variabili PA e Fitness.

Come per il sesso maschile e il sesso femminile, verifichiamo adesso se ci
siano delle differenze anche all'interno delle varie categorie di fumatori.
    
    <<Analisi_ModelloSmokeNO>>=
    #Modello nel caso di Smoke: NO, Former e Current.
    #Smoke: NO
    fit.smokeNO <- glm(nmc$cvd[nmc$smoke=="NO"] ~ 
                         nmc$sex[nmc$smoke=="NO"]+
                         nmc$age[nmc$smoke=="NO"]+
                         nmc$bmi[nmc$smoke=="NO"]+
                         nmc$fitness[nmc$smoke=="NO"], 
                         family=binomial)
    summary(fit.smokeNO)
    pstima.smokeNO <- fit.smokeNO$fitted.values
    @
    
    <<Analisi_ModelloSmokeFormer>>=
    #Smoke: Former
    fit.smokeFormer <- glm(nmc$cvd[nmc$smoke=="Former"] ~ 
                             nmc$sex[nmc$smoke=="Former"]+
                             nmc$age[nmc$smoke=="Former"]+
                             nmc$bmi[nmc$smoke=="Former"]+
                             nmc$fitness[nmc$smoke=="Former"],
                             family=binomial)
    summary(fit.smokeFormer)
    pstima.smokeFormer <- fit.smokeFormer$fitted.values
    @
    
    <<Analisi_ModelloSmokeCurrent>>=
    #Smoke: Current
    fit.smokeCurrent <- glm(nmc$cvd[nmc$smoke=="Current"] ~ 
                              nmc$sex[nmc$smoke=="Current"]+
                              nmc$age[nmc$smoke=="Current"]+
                              nmc$bmi[nmc$smoke=="Current"]+
                              nmc$fitness[nmc$smoke=="Current"], 
                              family=binomial)
    summary(fit.smokeCurrent)
    pstima.smokeCurrent <- fit.smokeCurrent$fitted.values
    @
    
    <<Plot_Analisi_ModelloSmoke>>=
    #Plot
    plot(nmc$age[nmc$smoke=="NO"], nmc$cvd[nmc$smoke=="NO"], 
         xlab="Age", ylab="CVD", col="blue")
    points(nmc$age[nmc$smoke=="Former"], 
           nmc$cvd[nmc$smoke=="Former"], 
           col="red")
    points(nmc$age[nmc$smoke=="Current"], 
           nmc$cvd[nmc$smoke=="Current"], 
           col="green")
    points(sort(nmc$age[nmc$smoke=="NO"]), 
           pstima.smokeNO[order(nmc$age[nmc$smoke=="NO"])], 
           col="blue")
    points(sort(nmc$age[nmc$smoke=="Former"]), 
           pstima.smokeFormer[order(nmc$age[nmc$smoke=="Former"])], 
           col="red")
    points(sort(nmc$age[nmc$smoke=="Current"]), 
           pstima.smokeCurrent[order(nmc$age[nmc$smoke=="Current"])], 
           col="green")
    legend(x="left", legend=c("NO", "Former", "Current"), 
           lty=c(1, 1, 1), col=c("blue","red", "green"), lwd=1)
    @
    
    Anche se meno visibile dal grafico, anche in questo caso i fumatori della
    categoria "Current", rispetto alle altre, mostra una maggior probabilità 
    nell'insorgenza di un problema cardiovascolare. \par
Un'altra osservazione che può emergere è quella che non ci siano grosse
    differenze tra le categorie Smoke:Former e Smoke:NO. Questa considerazione
    la valuteremo una volta calcolata la probabilità per questi soggetti.
    
      \subsubsection{Dato di esempio}
        Verifichiamo anche in questo caso se due individui con le
        solite caratteristiche ma di differente categoria di fumatori hanno 
        diverse probabilità e quanto tra queste categorie è la più soggetta
        ad avere problemi. \par
        
        Calcoliamo la probabilità di un Uomo (Sex 1) di 84 anni,
        con BMI pari a 28 (BMI 0), in pessima salute (Fitness 1) sia nel caso
        che questo sia un non fumatore, un ex-fumatore o un fumatore.
      
        <<Dato_fumatore>>=
        #Dato
        #Intercetta: 1, Sex: 1, Age: 84, BMI: 0, Fitness: 1, 
        man.smoke <- c(1, 1, 84, 0, 1)
        @
        
        <<Stima non fumatore>>=
        #Stima per un uomo Non Fumatore
        stima.smokeno <- exp(coef(fit.smokeNO)%*%man.smoke)/
                            (1+exp(coef(fit.smokeNO)%*%man.smoke))
        stima.smokeno
        @
        
        <<Stima per ex-fumatore>>=
        #Stima per l'Uomo Ex-Fumatore
stima.smokeformer <- exp(coef(fit.smokeFormer)%*%man.smoke)/
  (1+exp(coef(fit.smokeFormer)%*%man.smoke))
stima.smokeformer
@
  
  <<Stima per fumatore>>=
  #Stima per l'Uomo Fumatore
  stima.smokecurrent <- exp(coef(fit.smokeCurrent)%*%man.smoke)/
  (1+exp(coef(fit.smokeCurrent)%*%man.smoke))
stima.smokecurrent
@
  
  \begin{itemize}
\item Probabilità per un non fumatore: $\hat{\pi}$ = 0.5872
\item Probabilità per un ex-fumatore: $\hat{\pi}$ = 0.6170
\item Probabilità per un fumatore: $\hat{\pi}$ = 0.7732
\end{itemize}

In questo caso possiamo vedere come la probabilità tra un non fumatore e
un ex-fumatore sono abbastanza simili tra di loro, mentre un fumatore
ha una probabilità molto più alta rispetto al non fumatore.



<<RLS_F>>=
  #Fitness per categorie
  fit.fitness.muchworse <- glm(nmc$cvd[fitness=="Much Worse"]~nmc$age[fitness=="Much Worse"])
fit.fitness.littleworse <- glm(nmc$cvd[fitness=="Little Worse"]~nmc$age[fitness=="Little Worse"])
fit.fitness.justasgood<- glm(nmc$cvd[fitness=="Just as good"]~nmc$age[fitness=="Just as good"])
fit.fitness.abitbetter <- glm(nmc$cvd[fitness=="A bit better"]~nmc$age[fitness=="A bit better"])
fit.fitness.muchbetter <- glm(nmc$cvd[fitness=="Much better"]~nmc$age[fitness=="Much better"])
pstima.fit.fitness.muchworse <- fit.fitness.muchworse$fitted.values
pstima.fit.fitness.littleworse <- fit.fitness.littleworse$fitted.values
pstima.fit.fitness.justasgood <- fit.fitness.justasgood$fitted.values
pstima.fit.fitness.abitbetter <- fit.fitness.abitbetter$fitted.values
pstima.fit.fitness.muchbetter <- fit.fitness.muchbetter$fitted.values

plot(nmc$age, nmc$cvd, xlab="Age", ylab="CVD")
lines(nmc$age, pstima.fit.fitness.muchworse, col="purple")
lines(nmc$age, pstima.fit.fitness.littleworse, col="red")
lines(nmc$age, pstima.fit.fitness.justasgood, col="orange")
lines(nmc$age, pstima.fit.fitness.abitbetter, col="yellow")
lines(nmc$age, pstima.fit.fitness.muchbetter, col="green")
@
  
  
  
  Effetuiamo ora il plot del dataset tra i casi di CVD e l'età.

  <<Plot_Dataset>>=
  #Plot
  plot(nmc$age, nmc$cvd, xlab="Age", ylab="CVD")
  @
  
  \section{Modelli Differenti}
  Una volta che abbiamo selezionato il nostro modello e le nostre variabili, 
  verifichiamo il comportamento di quest'ultime solo su determinate categorie 
di invidividui.

\subsection{Maschio e Femmina} 
Nei precedenti capitoli, durante l'analisi delle singole variabili e dei vari 
    modelli, abbiamo notato come ci siano delle differenze all'interno delle varie
categorie. \par
Un primo esempio potrebbe essere quello nel verificare la diversa probabilità
presente tra il sesso maschile e il sesso femmile.\par
Valutiamo all'interno di un grafico se questa nostra ipotesi si verifica
    ancora in un modello più complesso di quello marginale.
    
    <<Analisi_ModelloSex_Maschio>>=
    #Modello Sesso Maschile
    #Sex: Male
    fit.male <- glm(nmc$cvd[nmc$sex=="Male"] ~ 
                      nmc$age[nmc$sex=="Male"]+
                      nmc$bmi[nmc$sex=="Male"]+
                      nmc$fitness[nmc$sex=="Male"]+
                      nmc$smoke[nmc$sex=="Male"], 
                      family=binomial)
    summary(fit.male)
    pstima.male <- fit.male$fitted.values
    @
    
    <<Analisi_ModelloSex_Femmina>>=
    #Modello Sesso Femminile
    #Sex: Female
    fit.female <- glm(nmc$cvd[nmc$sex=="Female"] ~ 
                        nmc$age[nmc$sex=="Female"]+
                        nmc$bmi[nmc$sex=="Female"]+
                        nmc$fitness[nmc$sex=="Female"]+
                        nmc$smoke[nmc$sex=="Female"], 
                        family=binomial)
    summary(fit.female)
    pstima.female <- fit.female$fitted.values
    @
    
    <<Plot_Analisi_ModelloSex>>=
    #Plot
    plot(nmc$age[nmc$sex=="Male"], nmc$cvd[nmc$sex=="Male"], 
         xlab="Age", ylab="CVD", col="blue")
    points(nmc$age[nmc$sex=="Female"], nmc$cvd[nmc$sex=="Female"], 
           col="red")
    points(sort(nmc$age[nmc$sex=="Male"]), 
          pstima.male[order(nmc$age[nmc$sex=="Male"])], 
          col="blue")
    points(sort(nmc$age[nmc$sex=="Female"]), 
          pstima.female[order(nmc$age[nmc$sex=="Female"])], 
          col="red")
    legend(x="left", legend=c("Male", "Female"), lty=c(1, 1),
           col=c("blue","red"), lwd=1)
    @
    
    Visualizzando il grafico, si nota ancora come il sesso maschile rimanga il 
    soggetto che statisticamente ha più rischi di CVD rispetto al genere 
    femminile.
  
  \subsection{Attività Fisica}
    Analizziamo se all'interno del nostro modello la variabile PA possa portare
a qualche risultato.
<<Analisi_ModelloPA_0>>=
  #Modello PA 0
  fit.pa.0 <- glm(nmc$cvd[nmc$pa==0] ~ nmc$sex[nmc$pa==0] +
                    nmc$age[nmc$pa==0] + nmc$bmi[nmc$pa==0] +
                    nmc$fitness[nmc$pa==0] + nmc$smoke[nmc$pa==0],
                  family=binomial)
summary(fit.pa.0)
pstima.pa.0 <- fit.pa.0$fitted.values
@
  
  <<Analisi_ModelloPA_1>>=
  #Modello PA 1
  fit.pa.1 <- glm(nmc$cvd[nmc$pa==1] ~ nmc$sex[nmc$pa==1] + 
                    nmc$age[nmc$pa==1] + nmc$bmi[nmc$pa==1] +
                    nmc$fitness[nmc$pa==1] + nmc$smoke[nmc$pa==1], 
                  family=binomial)
summary(fit.pa.1)
pstima.pa.1 <- fit.pa.1$fitted.values
@
  
  <<Plot_Analisi_ModelloPA>>=
  #Plot
  plot(nmc$age[nmc$pa==0], nmc$cvd[nmc$pa==0], xlab="Age", ylab="CVD", col="red")
points(nmc$age[nmc$pa==1], nmc$cvd[nmc$pa==1], col="blue")
points(nmc$age[nmc$pa==0], pstima.pa.0, col="red")
points(nmc$age[nmc$pa==1], pstima.pa.1, col="blue")
@
  
  Il grafico non sembra mostrarci particolari differenze, se non la poca presenza
di persone anziane con PA=0. \par
Vediamo se questa cosa è presente all'interno del dataset.
    
    <<Valutazione Anziani>>=
    #Tabella
    @
    
    <<Tabella_Frequenza_AnzianiPA0>>=
    ftable(pa+cvd ~ age, nmc)