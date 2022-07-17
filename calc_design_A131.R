# calculation of WWTP design regarding DWA A 131

library("plyr")
library("dplyr")
#library("rmarkdown")

source("design_parameters_A131.R") # Abrufen der Daten


options(scipen=999)                           # Ausgabe der Daten in Exponentialdarstellung f?r 0 oder als Reelle Zahl mit 999

# Fraktionierung CSB mit Vorwahl VH_VD_VBB
    # Ermittlung der abbaubaren CSB-Konzentration C_CSB_abb_ZB

C_CSB_ZB = (B_d_CSB_ZB / Q_T_aM) * 1000                                                     # [mg/L] Konzentration an CSB im Zulauf
S_CSB_inert_ZB = f_S * C_CSB_ZB                                                             # [mg/L] (Kap. 4.2, S. 25) geloester inerter CSB
X_TS_ZB = (B_d_AFS_ZB / Q_T_aM) * 1000                                                      # [mg/L] Konzentration an abfiltrierbaren Stoffen im Zulauf
X_orgTS_ZB = (1 - f_B) * X_TS_ZB                                                            # [mg/L] (Kap. 4.2, Gl. [8], S. 26) Konzentration an abfiltrierbaren organischen Stoffen im Zulauf
X_CSB_ZB = 1.6 * X_orgTS_ZB                                                                 # [mg/L] (Kap. 4.2, Gl. [10], S. 26) Konzentration an partikulaerem CSB im Zulauf
X_CSB_inert_ZB = f_A * X_CSB_ZB                                                             # [mg/L] (Kap. 4.2, Gl. [4], S. 25) Konzentration an partikulaerem inertem CSB im Zulauf
C_CSB_abb_ZB = C_CSB_ZB - S_CSB_inert_ZB - X_CSB_inert_ZB                                   # [mg/L] (Kap. 4.2, Gl. [5], S. 26) Konzentration an abbaubarem CSB im Zulauf
X_anorgTS_ZB = f_B * X_TS_ZB                                                                # [mg/L] (Kap. 4.2, Gl. [8], S. 26) Konzentration an anorganischen Stoffen im Zulauf

#X_CSB_BM = (C_CSB_abb_ZB * Y_CSB_abb + C_CSB_dos * Y_CSB_dos) * (1/(1 + b * t_TS_Bem * F_T))# [mg/L] (Kap. 5.2.2, Gl. [21], S. 34)
#X_CSB_inert_BM = 0.2 * X_CSB_BM * t_TS_Bem * b * F_T                                        # [mg/L] (Kap. 5.2.2, Gl. [23], S. 34)

# Stickstoff-Bilanz mit Vorwahl VH_VD_VBB
    # Ermittlung der im Tagesmittel zu denitrifizierenden Nitratstickstoffkonzentration

C_N_ZB = (1.12 * B_d_TKN_ZB / Q_T_aM) * 1000                   # [mg/L] Konzentration an TKN im Zulauf, hier kann ebenfalls mit einem Vorfaktor die Stickstoffkonzentration f?r eine etwaige R?ckbelastung (hier:12 %) angepasst werden!
#X_orgN_BM = 0.07 * X_CSB_BM                             # [mg/L] (Kap. 5.2.3, S. 35) in Biomasse eingebaute Stickstoffkonzentration
#X_orgN_inert = 0.03 * (X_CSB_inert_BM + X_CSB_inert_ZB) # [mg/L] (Kap. 5.2.3, S. 35) an inerte partikulaere Fraktionen gebundener Stickstoff
S_anorgN_UeW = AbwV_df$Nges[GK]

S_NO3_AN = f_SNO3_AN * S_anorgN_UeW                     # [mg/L] (Kap. 5.2.3, S. 35) Ablaufkonzentration an Nitratstickstoff
#S_NO3_D = C_N_ZB - S_orgN_AN - S_NH4_AN - S_NO3_AN - X_orgN_BM - X_orgN_inert # [mg/L] (Kap. 5.2.3, S. 34) im Tagesmittel zu denitrifizierende Nitratstickstoffkonzentration

# Iteration V_D/V_BB

x = 0.00001

while(x < 1) 
  
{
  t_TS_Bem = t_TS_aerob_Bem * 1/((1 - VH_VD_VBB))  
  
  X_CSB_BM = (C_CSB_abb_ZB * Y_CSB_abb + C_CSB_dos * Y_CSB_dos) * (1/(1 + b * t_TS_Bem * F_T))      # [mg/L] (Kap. 5.2.2, Gl. [21], S. 34)
  X_CSB_inert_BM = 0.2 * X_CSB_BM * t_TS_Bem * b * F_T                                              # [mg/L] (Kap. 5.2.2, Gl. [23], S. 34)
  X_orgN_BM = 0.07 * X_CSB_BM                                                                       # [mg/L] (Kap. 5.2.3, S. 35) in Biomasse eingebaute Stickstoffkonzentration
  X_orgN_inert = 0.03 * (X_CSB_inert_BM + X_CSB_inert_ZB)                                           # [mg/L] (Kap. 5.2.3, S. 35) an inerte partikulaere Fraktionen gebundener Stickstoff  
  
  S_NO3_D = C_N_ZB - S_orgN_AN - S_NH4_AN - S_NO3_AN - X_orgN_BM - X_orgN_inert                     # [mg/L] (Kap. 5.2.3, S. 34)im Tagesmittel zu denitrifizierende Nitratstickstoffkonzentration
  
  OV_C = C_CSB_abb_ZB + C_CSB_dos - X_CSB_BM - X_CSB_inert_BM                                       # Gl. 27 (Kap. 5.2.4)
  OV_C_la_vorg = f_CSB * C_CSB_abb_ZB * (1-Y_CSB_abb) + C_CSB_dos * (1 - Y_CSB_dos)                 # Gl. 28 (Kap. 5.2.4) Anteil des Sauerstoffbedarfs aus leicht abbaubarem CSB und extern dosiertem CSB
  OV_C_D = 0.75 * ( OV_C_la_vorg + ( OV_C - OV_C_la_vorg) * (VH_VD_VBB)^0.68)                       # Gl. 30 (Kap. 5.2.4) gesamte Sauerstoffverbrauch in der Denitrifikationszone fuer vorg. Deni
  
  x = OV_C_D / (2.86 * S_NO3_D)                                                                     # Gl. 34 (Kap. 5.2.5) Sauerstoffverbrauch der Denitrifikationszone
  
  VH_VD_VBB = VH_VD_VBB + 0.001
  
} 

# Phosphorelimination

C_P_ZB = (B_d_P_ZB / Q_T_aM) * 1000                   # [mg/L] Konzentration an Phosphor im Zulauf Belebung
C_P_UeW = AbwV_df$Pges[GK]                            # GK-abhaengige Fallunterscheidung fuer Ueberwachungswert Pges

C_P_AN = f_C_P_AN * C_P_UeW                           # [mg/L] (Kap. 5.3.1, S. 37) Konzentration an Phosphor im Ablauf Belebung
X_P_BM = 0.005 * C_CSB_ZB                             # [mg/L] (Kap. 5.3.1, S. 37) zum Zellaufbau heterotropher Biomasse benoetigter Phosphor
X_P_BioP = f_X_P_BioP * C_CSB_ZB * 0                      # [mg/L] (Kap. 5.3.1, S. 37) biologische Phosphorelimination im Falle eines anaeroben Beckens, hier im Moment mit 0 als Faktor
X_P_Faell = C_P_ZB - C_P_AN - X_P_BM - X_P_BioP       # [mg/L] (Kap. 5.3.1, S. 37) Ermittlung des zu faellenden Phosphats

# Berechnung der Schlammmasse

UeS_d_C = Q_T_aM * (((X_CSB_inert_ZB / 1.33) + ((X_CSB_BM + X_CSB_inert_BM)/(0.92 * 1.42)) + X_anorgTS_ZB) / 1000) # [kg/d] (Kap. 5.2.2, Gl. [24] o. Gl. [25], S. 34) taeglicher Ueberschussschlamm aus Organikabbau CSB
    # Wenn chemische P-Elimination: Wert fuer X_P_Faell_Al oder X_P_Faell_Fe wird in if-Bedingung ueberschrieben

X_P_Faell_Fe=0
X_P_Faell_Al=0
if (Faellmittel =="Eisen"){X_P_Faell_Fe = X_P_Faell} else if (Faellmittel =="Aluminium"){X_P_Faell_Al = X_P_Faell}
UeS_d_P = Q_T_aM * ((3 * X_P_BioP + 6.8 * X_P_Faell_Fe + 5.3 * X_P_Faell_Al) / 1000) # [kg/d] (Kap. 5.3.2, Gl. [36], S. 38)
UeS_d = UeS_d_C + UeS_d_P                                                            # [kg/d] (Kap. 5.4, S. 38)

M_TS_BB = t_TS_Bem * UeS_d                                                           # [kg] (Kap. 5.4, Gl. [39], S. 38) Masse der Feststoffe im Belebungsbecken


# Nachweis Saeurekapazitaet

MMV_Fe = 0
MMV_Al = 0
S_NH4_ZB = f_NH4 * C_N_ZB # Berechnung des Ammoniums im Zulauf zur Belebung mit Faktor unter der Annahme C_N = C_TKN
if (Faellmittel =="Eisen"){MMV_Fe = 2.7 * X_P_Faell} else if (Faellmittel =="Aluminium"){MMV_Al = 1.3 * X_P_Faell}
S_KS_AN = S_KS_ZB - (0.07 * (S_NH4_ZB - S_NH4_AN - S_NO3_ZB + S_NO3_AN) + 0.06 * MMV_Fe + 0.11 * MMV_Al - 0.03 * X_P_Faell) # (Kap. 7.4, Gl. [65], S. 53)
if (S_KS_AN > 1.5) {print("Saeurekapazitaet eingehalten")} else {print("Saeurekapazitaet nicht eingehalten")}             # (Kap. 7.4, S. 53)

# Bemessung Nachklaerbecken
    # Berechnung der erforderlichen Trockensubstanz

TS_BS = (1000/ISV) * t_E^(1/3)                          # [kg/m?] (Kap. 6.3, Gl. [40], S. 40) TS-Gehalt im Bodenschlamm / Raeumvolumenstrom 
TS_RS = f_TS_RS * TS_BS                                 # [kg/m?] (Kap. 6.3, S. 41) TS-Gehalt des Ruecklaufschlammes
TS_BB = (RV_max * TS_RS) / (1 + RV_max)                 # [kg/m?] (Kap. 6.4, Gl. [41], S. 41) TS-Gehalt Zulauf NKB

    # Bemessung der Nachklaerbeckenflaeche

q_A = q_SV_max/(ISV * TS_BB)                                                                # [m/h] (Kap. 6.5, Gl. [42]) Tatsaechliche Flaechenbeschickung
if (q_A < q_A_max) {print("Flaechenbeschickung eingehalten")} else {print("Flaechenbeschickung nicht eingehalten")}                 # [m/h] Wird die vorgegebene Flaechenbeschickung eingehalten vgl. Vorgaben df
A_NKB_ges = Q_M / (q_A * 24)                                                                # [m?] (Kap. 6.6, Gl. [43]) Gesamtflaeche aller Nachklaerbecken rechnerisch
A_NKB = A_NKB_ges / n_NKB                                                                   # [m?] Flaeche eines Nachklaerbeckens
d_NKB = sqrt((4 * A_NKB)/pi)                                                                # [m] Durchmesser eines Nachklaerbeckens

if (d_NKB > 10) {d_NKB = round_any(d_NKB, 10, f = ceiling)} else {d_NKB = round_any(d_NKB, 1, f = ceiling)}       # [m] Tatsaechlicher Durchmesser eines NKB, Rundung zwecks baulicher Umsetzbarkeit, ggf. komplexere Funktion einbauen
q_SV = q_A * ISV * TS_BB                                                                                          # [L/m?/h] Tatsaechliche Schlammvolumenbeschickung

    # Es wird erneut mit den gerundeten Werten gerechnet

A_NKB = (d_NKB)^2 * (pi/4)                                    # [m?] Tatsaechliche Flaeche eines Nachklaerbeckens
A_NKB_ges = A_NKB * n_NKB                                     # [m?] Tatsaechliche Gesamtflaeche eines Nachklaerbeckens
q_A = Q_M / (A_NKB_ges * 24)                                  #[m/h] Tatsaechliche Flaechenbeschickung
if (q_A < q_A_max) {print("Flaechenbeschickung eingehalten")} else {print("Flaechenbeschickung nicht eingehalten")}
q_SV = q_A * ISV * TS_BB                                      # [L/m?/h] Tatsaechliche Schlammvolumenbeschickung

    # Bemessung der Nachklaerbeckentiefe 

h_23 = q_A * (1 + RV_max) * ((500/(1000 - VSV)) + (VSV/1100))    # [m] (Kap. 6.7, Gl. [44]) 3-Schichtmodell, Uebergangs- und Pufferzone
h_4 = (TS_BB * q_A * (1 + RV_max) * t_E)/TS_BS                   # [m] (Kap. 6.7, Gl. [45]) 3-Schichtmodell, Eindick- und Raeumzone
h_gesamt = h_1 + h_23 + h_4                                      # [m] Gesamttiefe des Nachklaerbeckens auf 2/3 des Fliessweges, h_1 Hoehe der Klarwasserzone aus Vorgaben

if (h_gesamt >= 3) {print("Gesamthoehe eingehalten")}     else {print("Gesamthoehe nicht eingehalten")}  # Kontrollausgabe der NKB-Bemessungsvorgaben fuer die Gesamthoehe
if (h_1 + h_23 >= 2.5) {print("Randhoehe eingehalten")} else {print("Randhoehe nicht eingehalten")}                          # Kontrollausgabe der NKB-Bemessungsvorgaben fuer die Gesamthoehe

    # Berechnung des Einlaufbauwerks


Q_M_NKB = (Q_M / 24) * (1 / n_NKB)             # [m?/h] Maximaler Abwasserstrom aus einem Nachklaerbecken
A_Dueker = Q_M_NKB / v                          # [m?] Fliessflaeche des Duekers
d_Dueker = 2 * sqrt(A_Dueker / pi)              # [m] Fliessflaeche des Duekers
A_ZD = (pi / 4) * (d_Dueker)^2                  # [m?] Fliessflaeche des Zuklaufduekers

v_E = (((Q_M_NKB / 3600) * (1 + RV_max))/A_ZD)           # [m/s] (Kap. 6.8, Gl. [49]) Eintrittsgeschwindigkeit in das Einlaufbauwerk
V_EBW_min = (Q_M_NKB * (1 + RV_max) * t_EBW) / 60        # [m?] Minimales Volumen des Einlaufbauwerks
V_EBW_min = round_any(V_EBW_min, 10, f = ceiling)

P_EBW = 0.5 * rho_O * (v_E^2) * (Q_M_NKB / 3600) * (1 + RV_max)                   # [Nm/s] (Kap. 6.8, Gl. [48]) Eingetragene Leistung in das Einlaufbauwerk
G = sqrt((P_EBW)/(mu * V_EBW_min))                                                # [1/s] (Kap. 6.8, Gl. [47]) Mass fuer die turbulente Scherbeanspruchung
if (G >= 40 & G <= 80) {print("Scherbeanspruchung im Normbereich")} else {print("Scherbeanspruchung au?erhalb Normbereich")}   # Vorgaben nach A131 Kap. 6.8, S.47 
h_EBW = h_gesamt + 2/3 * (d_NKB / 24) - 1.5                                       # [m] Hoehe des Einlaufbauwerks
d_EBW_innen = sqrt ((4 * V_EBW_min)/(h_EBW * pi))                                 # [m] Innendurchmesser des Einlaufbauwerks
d_EBW_innen = round_any(d_EBW_innen, 0.1, f = ceiling)
V_EBW = (pi / 4) * (d_EBW_innen^2) * h_EBW                                        # [m?] Volumen des Einlaufbauwerks
d_EBW_aussen = d_EBW_innen + 0.5                                                  # [m] Aussendurchmesser des Einlaufbauwerks

t_EBW_tat = (V_EBW * 60)/(Q_M_NKB * (1 + RV_max))                                 # [min] Tatsaechliche Durchflusszeit durch das Einlaufbauwerk                    
if (t_EBW_tat > t_EBW) {print("Durchflusszeit im Einlaufbauwerk eingehalten")} else {print("Durchflusszeit im Einlaufbauwerk nicht eingehalten")}
u = ((Q_M_NKB / 3600) * (1 + RV_max)) / (pi * d_EBW_aussen * h_EBW_ein)           # [m/s] Horizontale Eintrittsgeschwindigkeit 
if (u * 100 < 7) {print("Horizontale Str?mungsgeschwindigkeit eingehalten")} else {print("Horizontale Str?mungsgeschwindigkeit nicht eingehalten")}         # Vorgaben nach A131 Kap. 6.8, S.47 
F_D = (u)/(sqrt((rho_O - 1000) / (1000) * 9.81 * h_EBW_ein))                      # [-] (Kap. 6.8, Gl. [46]) Densimetrische Froudezahl
if (F_D < 1) {print("Densimetrische Froudezahl eingehalten")} else {print("RV anpassen")} 

# Bemessung Belebungsbecken
    # Geometrische Abmessungen

V_BB = M_TS_BB / TS_AB                              # [m?] (Kap. 7.1, Gl. [50]) Volumen des Belebungsbecken
V_BB = round(V_BB, digits = - 3)                    # Ausgabe in Konsole
V_D = VH_VD_VBB * V_BB                              # [m?] Volumen der Denitrifikation 
V_N = V_BB - V_D                                    # [m?] Volumen der Nitrifikation
A_BB = V_BB / (n_BB * h_BB)                         # [m?] Flaeche eines Belebungsbeckens
l_BB = A_BB / b_BB                                  # [m] Laenge eines Belebungsbeckens

# Rueckfuehrung fuer vorgeschaltete Denitrifikation

RF = S_NO3_D / S_NO3_AN                     # [-] (Kap. 7.2, Gl. [51]) rechnerisch erforderliches Rueckfuehrverhaeltnis fuer vorgeschaltete Deni
Q_RZ = Q_T_2h_max * (RF - RV_max)           # [m?/h] (Kap. 7.2, Gl. [52]) interne Rezirkulation bei vorgeschalteter Denitrifikation

# Belueftungsbemessung

    # Platzhaltervariablen fuer die Werte aus der Iteration !

OV_d_C = Q_T_aM * (OV_C / 1000)                                   # [kg/d] (Kap. 7.3, Gl. [58]) Sauerstoffverbrauch fuer die Kohlenstoffelimination
OV_d_N = Q_T_aM * 4.3 *  (S_NO3_D - S_NO3_ZB + S_NO3_AN)/ 1000    # [kg/d] (Kap. 7.3, Gl. [59]) Sauerstoffverbrauch fuer die Nitrifikation
OV_d_D = Q_T_aM * 2.86 * S_NO3_D / 1000                           # [kg/d] (Kap. 7.3, Gl. [60]) Sauerstoffverbrauch fuer die Denitrifikation

Stofak_df <- data.frame(t_TS = c(4,6,8,10,15,25), f_C = c(1.3, 1.25, 1.2, 1.2, 1.15, 1.1), f_N_kg = c(NA, NA, NA, 2.4, 2.0, 1.5), f_N_gr = c(NA, NA, 2.0, 1.8, 1.5, NA)) # Erstellen der Tabelle aus Anhang 1 AbwV
Stofak_df = t(Stofak_df)
Stofak_df

# Anlegen der Vektoren zur Ermittlung der Stossfaktoren

f_x = c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)                                                          # Vektor fuer das Schlammalter in d
f_y <- c(1.3,1.275,1.25,1.235,1.2,1.2,1.2,1.19,1.18,1.17,1.16,1.15,1.14,1.14,1.135,1.13,1.125,1.12,1.105,1.11,1.105,1.1)      # Vektor fuer den Stossfaktor fuer die Kohlenstoffatmung
f_z <- c(2.4,2.4,2.4,2.4,2.4,2.4,2.4,2.32,2.24,2.16,2.08,2,1.95,1.9,1.85,1.8,1.75,1.7,1.65,1.6,1.55,1.5)                      # Vektor fuer den Stossfaktor fuer die Stickstoffatmung mit einer Fracht kleiner gleich 2400 kg CSB pro Tag 
f_k <- c(2,2,2,2,2,1.9,1.8,1.74,1.68,1.62,1.56,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5)                                   # Vektor fuer den Stossfaktor fuer die Stickstoffatmung mit einer Fracht groesser 12000 kg CSB pro Tag


t_TS <- round(t_TS_Bem, digits=0)                             # Runde das Bemessungsschlammalter auf eine ganze Zahl und weise diese Zahl einer neuen Variable zu
xk <- which(f_x == t_TS, arr.ind = FALSE, useNames = TRUE)    # Gebe den Index / die Position des Bemessungsschlammalters im Vektor f_x wieder
f_C <- f_y[xk]                                                # Weise den Wert der Variable fuer die Kohlenstoffatmung zu
if(B_d_CSB_ZB <= 2400){f_N <- f_z[xk]} else if (B_d_CSB_ZB > 12000){f_N <- f_k[xk] } else {print("Achtung Stossfaktor haendisch waehlen")}                                                             # Wenn-Bedingung zur Unterscheidung welcher Vektor fuer die Ermittlung des 
                                                              # Faktors der Stickstoffatmung verwendet werden soll und Zuweisung zum Stickstofffaktor

  # Lastfallbetrachtungen

OV_h_aM = ((OV_d_C - OV_d_D)+ OV_d_N) / 24                        # [kg/h] (Kap. 7.3, Gl. [61]) Lastfall 1: Durchschnittlicher Sauerstoffverbrauch im Ist-Zustand
OV_h_max = (f_C * (OV_d_C - OV_d_D) + f_N * OV_d_N)/ 24           # [kg/h] (Kap. 7.3, Gl. [62]) Lastfall 2: Maximaler Sauerstoffverbrauch im Ist-Zustand
OV_h_min = OV_d_C / ((3.92 / (t_TS_Bem / 24) * F_T + 1.66) * 24)  # [kg/h] (Kap. 7.3, Gl. [63]) Lastfall 3: Minimaler Sauerstoffverbrauch im Ist-Zustand

save.image(file="results_A131.RData") #saving workspace for use in further scripts
