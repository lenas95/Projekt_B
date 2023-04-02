### Ubereinstimmung von historischen CMIP6-Simulationen mit sechs verschiedenen Observationsdatensätzen hinsichtlich Intensität und Tag des jährlichen maximalen Tagesniederschlags

## Einleitung und Motivation

Eine der prognostizierten Folgen der ansteigenden globalen Temperaturen ist die Häufung
von starken Nierderschlagsereignissen über den regionalen Abschnitten in den mittleren
Breitengraden. Die Folgen solcher extremen Ereignisse haben Teils schwerwiegende Fol-
gen für die betroffenen Bevölkerungsgruppen und Ökosysteme. Genaue Projektionen
bezüglich des Auftretens solcher Events, sind daher von großer Wichtigkeit um die sozial-
ökologischen Folgen abzulindern bzw. entsprechende Anpassungsmaßnahmen ergreifen zu
können.
Um solche klimatisch-bedingten Projektionen zu erstellen, wird in den entsprechenden
wissenschaftlichen Disziplinen auf Simulationen gekoppelter Klimamodelle zurückgegriffen.
Eines der Werkzeuge zur Auswertung, Handhabung und Validierung solcher Klimamodelle und Prognosen ist die sechste Phase des gekoppelten Modellvergleich Projekts (Engl:Phase 6 of the Coupled Model Intercomparison Project; CMIP6 ). Im
Rahmen dieses Projekts werden Übereinstimmungen zwischen CMIP6-Modellen und Observationsdatensätze weiter
untersucht. Hierfür werden 48 historische Simulationen des CMIP6 und 6 Observationsdatensätze hinsichtlich der Projektion der Intensität des maximalen Tagesniederschlags (Rx1day) pro Jahr, ebenso wie des Tages des Rx1day miteinander verglichen.

## Methoden

Die Codes zum Projekt finden sich im Ordner [Codes](https://github.com/lenas95/Projekt_B/tree/main/Codes).

Um eine Vergleichbarkeit zwischen den Datensätzen zu ermöglichen wurden die Datensätze der CMIP6-Modelle und der Observationen vor den jeweiligen
Berechnungen auf ein einheitliches globales Gitter (1° x 1°) interpoliert. Zunächst wurden jeweils die gemittelte Niederschlagsmenge des Rx1day in allen  
Gitterpunkten für die 48 Modelle und für die sechs Observationsdatensätze berechnet. Die Berechnungen bei den
Modellen wurden  ̈über den  Zeitraum von 65 Jahre (1950-2015) und über alle EnsembleMember erstellt. Für die Modelle mit mehreren Ensemble Membern wurde die
Zeitreihe künstlich verlängert. In diesem Fall ist die Länge der neuen Zeitreihe das Produkt der Anzahl der Jahre mit der Anzahl der Ensemble Member. Im
Fall der sechs Observationsdatensätze, wurde der Mittelwert und die Standardabweichung des Rx1day in allen
Gitterpunkten ebenfalls für die Jahre 1950-2015 berechnet um die Vergleichbarkeit zwischen den Modellen und den Observationsdatensätzen zu gewährleisten.
Gleiches wurde für den Tag des Rx1day gemacht. Die Berechnung des Mittelwerts der Tage des Rx1day
sind hierbei zyklischer Natur da die Berechnungsgrundlage Jahre sind. Anschließend wurde die Standardabweichung der Datens ̈atze der beiden
Niederschlagsgrößen für die Observationsdatensätze berechnet. Weiterhin wurden für alle 48 CMIP6-Modelle sowohl
für die Intesität des Rx1day als auch für den Tag des Rx1day die dimensionslose, durch
die Standardabweichung normierte Differenz zwischen den Mittelwerten der historischen Simulationen und jedem Observationsdatensatz berechnet.

## Resultate und Diskussion

Die Resultate und die dazugehörige Diskussion können dem beigefügten ([Report](https://github.com/lenas95/Projekt_B/blob/main/Project_22_Prmax.pdf)) entnommen werden.
