# design parameters an inputs for DWA A 131

options(scipen=999)                 # Ausgabe der Daten in Exponentialdarstellung f?r 0 oder als Reelle Zahl mit 999

#Bemessungseingangsdaten laden

load("eval_results_A198.RData")

# Eingangsparameter/Vorgaben fuer DWA-A 131 (2016)

# Bemessungsgrundlagen Kapitel (Kap.) 4.1 
T_Bem = 12                # [?C] (Kap. 5.1.5) Bemessungstemperatur 

Q_T_aM = Q_d_Konz            # [m?/d] langjaehriges Mittel Trockenwetterzufluss
Q_T_2h_max = Q_d_Konz         # [m?/h] max. TW-Zufluss als 2-Stunden-Mittel ACHTUNG HILFSWEISE Q_d_Konz
#Q_M aus eval_results.RData              # [m?/d] Bemessungszufluss


B_d_CSB_ZB = Bd_CSB_85        # [kg/d] taegliche CSB-Fracht Zulauf Belebung
B_d_AFS_ZB = Bd_CSB_85*70/120 # [kg/d] taegliche AFS-Fracht Zulauf Belebung ACHTUNG HILFSWEISE ÜBER VH
B_d_TKN_ZB = Bd_KN_85         # [kg/d] taegliche TKN-Fracht Zulauf Belebung
B_d_P_ZB = Bd_P_85            # [kg/d] taegliche Phosphor-Fracht Zulauf Belebung

b_CSB_85 = 0.09           # [kg/(EW * d)] fuer t_R_T von 0.5 h bis 1.0 h (ATV-DVWK A 198 (2003), Tabelle 1)

EW = B_d_CSB_ZB / b_CSB_85    # Ermittlung Einwohnerwerte (EW) fuer massgebende Ueberwachungswerte (UeW)
EW = round(EW, digits = -2)   # Runden der berechneten EW fuer massgebende UeW: 0 = exakt, -1 = auf 10 EW genau, 1 = auf 0,1 EW genau...)

AbwV_df <- data.frame(GK = c(1,2,3,4,5), EW = c(1000, 5000, 10000, 100000, 100001), CSB = c(150 ,110, 90, 90, 75),BSB5 = c(40, 25, 20, 20, 15), NH4N = c(NA, NA, 10, 10, 10), Nges = c(NA, NA, NA, 18, 13),Pges = c(NA, NA, NA, 2, 1))                
            # Erstellen der Tabelle aus Anhang 1 AbwV

if (EW < AbwV_df$EW[1]){GK = 1} else if (EW < AbwV_df$EW[2]){GK = 2} else if (EW < AbwV_df$EW[3]){GK = 3} else if (EW < AbwV_df$EW[4]){GK = 4} else if (EW > AbwV_df$EW[5]){GK = 5}
AbwV_df[GK,] # Auslesen der UeW fuer eine KA der GK 1 (Auslesen der 1. Zeile)

# Wahl Bemessungsparameter A 131
    # Dimensionslose Faktoren

f_S = 0.05                 # [-] (Kap. 4.2, Gl. [3]); [0.05 ... 0.1, wobei 0.05 = kommunales Abwasser] Anteil inerter, geloester CSB an Gesamt-CSB
f_A = 0.3                  # [-] (Kap. 4.2, Gl. [4]); [0.2 ... 0.35, wobei 0.3 = kommunales Abwasser] abhaengig von Aufenthaltszeit in Vorklaerung
f_CSB = 0.2                # [-] (Kap. 4.2, Gl. [6]); [0.15 ... 0.25] fuer durchschnittlich zusammenges. Abwasser] Anteil leichtabbaubarer CSB an Gesamt-CSB
f_B = 0.2                  # [-] (Kap. 4.2, Gl. [8]); [0.2 ... 0.3, wobei 0.2 = vorgeklaertes Abwasser & 0.3 = Rohabwasser] Anteil anorganischer Stoffe an abfiltrierbaren Stoffen (1 - GV)
F_T = 1.072^(T_Bem - 15)   # [-] (Kap. 5.2.2, Gl. [22]) Temperaturfaktor fuer endogene Veratmung 

f_SNO3_AN = 0.8             # (Kap. 5.2.3, S. 35) [0.8 ... 0.6] Faktor zur Ermittlung der Nitratkonzentration im Ablauf Nachkl?rung
f_NH4 = 0.75                # Anteil von Ammonium am gesamten Stickstoff
f_C_P_AN = 0.6              # (Kap. 5.3.1, S. 37) [0.7 ... 0.6] Faktor zur Ermittlung der Phosphorkonzentration im Ablauf Nachkl?rung
f_X_P_BioP = 0.006          # (Kap. 5.3.1, S. 37) [0.005... 0.007] FUER VORGESCHALTETE DENI! Faktor f?r biologische Phosphorelimination

# Aerobes Bemessungsschlammalter t_TS_aerob_Bem

mu_A_max = 0.47                                             # [1/d] (Kap. 5.1.3, Gl. [13]) maximale Wachstumsrate Nitrifikanten bei 15 ?C
SF = 1.6                                                    # [-] (Kap. 5.1.3, Gl. [13]) -> Sicherheitsfaktor

print(if (B_d_CSB_ZB <= 2400){PF = 2.1} else if (B_d_CSB_ZB > 12000) {PF = 1.5} else {PF = (B_d_CSB_ZB-2400)*(-0.0000625)+2.1} )     # [-] (Kap. 5.1.3, S. 30)

t_TS_aerob_Bem = PF * (1/mu_A_max) * SF * 1.103^(15 - T_Bem)# [d] (Kap. 5.1.3, Gl. [12])

# Externe Kohlenstoffquellen

Y_CSB_abb = 0.67                                                                                                                      # [g gebildete Biomasse pro g abgebautem CSB] (Kap. 5.2.2, unter Gl. [22])
print(Y_CSB_dos_df <- data.frame(C_Quelle = c("Methanol", "Ethanol", "Essigsaeure", "ohne"), Yield = c(0.45, 0.42, 0.42, 0)))         # [g CSB_BM / g CSB_abb] (Kap. 4.2, Tab. 1)
Y_CSB_dos_df[2,]
print(C_CSB_dos_df <- data.frame(C_Quelle = c("Methanol", "Ethanol", "Essigsaeure", "ohne"), Konzentration = c(1185, 1630, 1135, 0))) # [g/L] (Kap. 4.2, Tab. 1)
C_CSB_dos_df[1,]

# Konkrete Wahl der Kohlenstoffquelle

Kohlenstoffquelle = "ohne" # (wenn keine externe C-Dosierung: Eintragen "ohne")
if (Kohlenstoffquelle =="Methanol"){Y_CSB_dos = Y_CSB_dos_df$Yield[1]}         else if (Kohlenstoffquelle =="Ethanol"){Y_CSB_dos = Y_CSB_dos_df$Yield[2]} else if (Kohlenstoffquelle =="Essigsaeure"){Y_CSB_dos = Y_CSB_dos_df$Yield[3]} else if (Kohlenstoffquelle =="ohne"){Y_CSB_dos = Y_CSB_dos_df$Yield[4]}
if (Kohlenstoffquelle =="Methanol"){C_CSB_dos = C_CSB_dos_df$Konzentration[1]} else if (Kohlenstoffquelle =="Ethanol"){C_CSB_dos = C_CSB_dos_df$Konzentration[2]} else if (Kohlenstoffquelle =="Essigsaeure"){C_CSB_dos = C_CSB_dos_df$Konzentration[3]} else if (Kohlenstoffquelle =="ohne"){C_CSB_dos = C_CSB_dos_df$Konzentration[4]}
print(Y_CSB_dos)
print(C_CSB_dos)

b = 0.17            # [1/d] (Kap. 5.2.2, unter Gl. [22]) Zerfallskoeffizient bei 15 ?C

VH_VD_VBB = 0.01     # [-] Anteil Denitrifikationszone an gesamtem BB-Volumen 

# Vorgaben Stickstoffbilanz 

S_orgN_AN = 2         # [mg/L] (Kap. 5.2.3, S. 35) Konzentration an organischem Stickstoff im Ablauf Nachklaerung
S_NO3_ZB = 0          # [mg/L] (Kap. 5.2.3, S. 35) Nitratstickstoffkonzentration im Zulauf Belebung
S_NH4_AN = 0          # [mg/L] (Kap. 5.2.3, S. 35) Konzentration an Ammonium im Ablauf Nachklaerung

# Nachweis Saeurekapazitaet

S_KS_ZB = 8.2         # [mmol/L] (Kap. 7.4, S. 52) Saeurekapazitaet im Zulauf zum Belebungsbecken !Selbstgewaehlt!

# Bemessung Nachklaerbecken
    # Annahmen

VSV = 325                                                                                               # [L/m?] (Kap. 6.1, S. 39) -> Vergleichsschlammvolumen 
TS_AB = 2.5                                                                                             # [kg/m?] (Kap. 6.1, S. 39) -> Trockensubstanzgehalt Zulauf Nachklaerbecken
print(q_A_df <- data.frame(NKB_Art = c("horizontal d.", "vertikal d."), Fl.beschickung = c(1.6, 2.0)))  # [m/h] (Kap. 6.5, S. 42) Flaechenbeschickung
print(q_SV_df <- data.frame(NKB_Art = c("horizontal d.", "vertikal d."), SV.beschickung = c(500, 650))) # [m/h] (Kap. 6.5, S. 42) Schlammvolumenbeschickung
ISV = 125                                                                                               # [L/kg] (Kap. 6.1, S. 39 & Kap. 6.2, S. 40) [50 ... 200] Schlammvolumenindex
t_E = 2.0                                                                                               # [h] (Kap. 6.2, S. 40) -> Eindickzeit
print(RV_df <- data.frame(NKB_Art = c("horizontal d.", "vertikal d."), RV = c(0.55, 1.0)))              # (Kap. 6.1, S. 39) Ruecklaufverhaeltnis

NKB_Durchstroemung = "horizontal"
if (NKB_Durchstroemung =="horizontal"){q_A_max = q_A_df$Fl.beschickung[1]}   else if (NKB_Durchstroemung =="vertikal"){q_A_max = q_A_df$Fl.beschickung[2]}
if (NKB_Durchstroemung =="horizontal"){q_SV_max = q_SV_df$SV.beschickung[1]} else if (NKB_Durchstroemung =="vertikal"){q_SV_max = q_SV_df$SV.beschickung[2]}
if (NKB_Durchstroemung =="horizontal"){RV_max = RV_df$RV[1]}                 else if (NKB_Durchstroemung =="vertikal"){RV_max = RV_df$RV[2]}
if (NKB_Durchstroemung =="horizontal"){f_TS_RS = 0.7}                        else if (NKB_Durchstroemung =="vertikal"){f_TS_RS = 1} # (Kap. 6.3, S. 41) [horizontal durchstr?mt: Saugr?umer [0.5 ... 0.7], Schildr?umer [0.7 ... 0.8]]

# Nachweisfuehrung 

n_NKB = 3         # Anzahl Nachklaerbecken
h_1 = 0.5         # [m] (Kap. 6.7, S. 44) -> Klarwasserzone
rho_O = 1016      # [kg/m?] (Kap. 6.8, S. 46) -> Dichte des belebten Schlammes
mu = 0.00125       # [Ns/m?] (Kap. 6.8, S. 47) -> dynamische Viskositaet des belebten Schlammes
h_EBW_ein = 0.60  # [m] (Kap. 6.8, S. 47) [0.3 ... 0.6] -> Hoehe Einlauf Nachklaerbecken
t_EBW =  2        # [min] Durchflusszeit durch das Einlaufbauwerk
v = 3600          # [m/h] (VL F. 51) Mindestflie?geschwindigkeit bei Bemessungszufluss
# Bemessung Belebungsbecken

n_BB = 8          # Anzahl Belebungsbecken
h_BB = 7.0        # [m] Tiefe Belebungsbecken
b_BB = 25         # [m] Breite Belebungsbecken

# Wahl des Fällmittels

Faellmittel = "Eisen"   # (Hilfsfunktion. Default-Wert = 0, wenn keine chemische P-Faellung: Eintragen "ohne")


# Stossfaktoren Initialisierung

f_C = 0           # Stossfaktor fuer die Kohlenstoffatmung
f_N = 0           # Stossfaktor fuer die Stickstoffatmung


