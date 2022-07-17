# evaluation parameter for ATV-DVWK A198

Tbem = 12 # Bemessungstemperatur
EW = 75400 # angeschlossene EW
qS_EW = 150 # [L/(EW*d)]
Q_Ind = 391000 # [m³/a] aus Gewerbe und Industrie

Q_S_d_aM = EW*qS_EW/1000+Q_Ind/365 # [m³/d] mittl. Schmutzwasserabfluss

Q_F_mM_Wahl = 120 # [L/s] aus den Auswertungen der maximalen Monatsmittelwerte gewählter maximaler Fremdwasserzufluss

f_S_QM = 3.5 # [-] Mischwasserfaktor
