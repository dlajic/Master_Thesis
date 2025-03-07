# Master_Thesis

## Allgemein: 

**Thema:** Einfluss von (Online-) Nachrichtenkonsum (Fake vs. conventional news) auf Einstellung gegenüber Migranten und bestimmten Parteien. 

**Daten:** Web Tracking Daten (kompletter Internetverlauf) von Teilnehmern der Studie werden analysiert, sowie Umfragedaten dieser Personen während dieses Zeitraums (6 Monate).

**Achtung! :** Diese Daten kann ich aufgrund von Datenschutzrichtlinien nicht veröffentlichen. Hier teile ich nur die Analysescripts: 
Analysis_final: 0_1_webtracking_mig - 0-4-survey_analyis_mig

**Classifier:** Vor Analyse dieser Daten werden Classifier benötigt, welche die konsumierten Nachrichtenartikel automatisch thematisch einordnen und zusätzlich ob dies Meinungsartikel sind. 
Hierfür habe ich einen eigenen Trainingsdatensatz gescraped, welcher pro Classifier aus über 100.000 Nachrichtenartikeln der größten News Outlets Deutschlands besteht.
Nach Bereinigung dieser Daten wurden diese zum fine tunen von BERT Modellen genutzt, welche später zur Anwendung kamen. Zusätzlich wurden noch weitere Überblicke durch BERT Topic und Word Clouds erstellt.

**Ordnerstruktur:**

**Analysis_final:** Finale Analyse der Daten. (aufgrund Datenschutz diese Daten nicht öffentlich zugänglich)

**BERT Classifier:** Erstellung zweier Classifier, welche news nach Thema Migration und Meinungsartikel einordnen.

**BERT Topic Models:** Nutzung des BERT Modells um automatisch thematische Subgruppen zu generieren

**Web_scraping:** Web scraping scripts, um Nachrichtenartikel unterschiedlicher news outlets zu scrapen (als Trainingsdatensatz für BERT Classifier).

**Word Clouds:** Deskriptive Darstellung wie oft welche Art von Entities in fake vs. conventional news auftauchen (Thema Migrationsartikel)
