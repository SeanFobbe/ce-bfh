# ACHTUNG: Weiterentwicklung ab sofort auf Codeberg

Die Weiterentwicklung dieses Projekts findet ab sofort auf Codeberg statt: https://codeberg.org/seanfobbe/ce-bfh

Das GitHub repository ist nur noch als Archivfassung verfügbar.


# Corpus der Entscheidungen des Bundesfinanzhofs (CE-BFH)


## Überblick

Das **Corpus der Entscheidungen des Bundesfinanzhofs (CE-BFH)** ist eine möglichst vollständige Sammlung der vom Bundesfinanzhof (BFH) veröffentlichten Entscheidungen. Der Datensatz nutzt als seine Datenquelle die [amtliche Entscheidungsdatenbank](https://www.bundesfinanzhof.de/) des Bundesfinanzhofs und wertet diese vollständig aus.

Alle mit diesem Skript erstellten Datensätze werden dauerhaft kostenlos und urheberrechtsfrei auf Zenodo, dem wissenschaftlichen Archiv des CERN, veröffentlicht. Alle Versionen sind mit einem separaten und langzeit-stabilen (persistenten) Digital Object Identifier (DOI) versehen.

Aktuellster, funktionaler und zitierfähiger Release des Datensatzes: <https://doi.org/10.5281/zenodo.3942742>


## Funktionsweise

Primäre Endprodukte des Skripts sind folgende ZIP-Archive:

- Der volle Datensatz im CSV-Format (mit zusätzlichen Metadaten)
- Die reinen Metadaten im CSV-Format (wie unter 1, nur ohne Entscheidungsinhalte)
- Alle Entscheidungen im HTML-Format
- Alle Entscheidungen im TXT-Format
- Alle Entscheidungen im PDF-Format
- Nur V-Entscheidungen (BFHE, amtliche Sammlung) im PDF-Format
- Alle Analyse-Ergebnisse (Tabellen als CSV, Grafiken als PDF und PNG)


Alle Ergebnisse werden im Ordner `output/` abgelegt. Zusätzlich werden für alle ZIP-Archive kryptographische Signaturen (SHA2-256 und SHA3-512) berechnet und in einer CSV-Datei hinterlegt. 



## Systemanforderungen

- [Docker](https://docs.docker.com/get-docker/)
- [Docker Compose](https://docs.docker.com/compose/install/)
- 4 GB Speicherplatz auf Festplatte
- Multi-core CPU empfohlen (8 cores/16 threads für die Referenzdatensätze). 


In der Standard-Einstellung wird das Skript vollautomatisch die maximale Anzahl an Rechenkernen/Threads auf dem System zu nutzen. Die Anzahl der verwendeten Kerne kann in der Konfigurationsatei angepasst werden. Wenn die Anzahl Threads auf 1 gesetzt wird, ist die Parallelisierung deaktiviert.



## Anleitung


### Schritt 1: Ordner vorbereiten

Kopieren Sie bitte den gesamten Source Code in einen leeren Ordner (!), beispielsweise mit:

```
$ git clone https://codeberg.org/seanfobbe/ce-bfh
```

Verwenden Sie immer einen separaten und *leeren* Ordner für die Kompilierung. Die Skripte löschen innerhalb von bestimmten Unterordnern (`files/`, `temp/`, `analysis` und `output/`) alle Dateien die den Datensatz verunreinigen könnten --- aber auch nur dort.



### Schritt 2: Docker Image erstellen

Ein Docker Image stellt ein komplettes Betriebssystem mit der gesamten verwendeten Software automatisch zusammen. Nutzen Sie zur Erstellung des Images einfach:

```
$ bash docker-build-image.sh
```




### Schritt 3: Datensatz kompilieren

Falls Sie zuvor den Datensatz schon einmal kompiliert haben (ob erfolgreich oder erfolglos), können Sie mit folgendem Befehl alle Arbeitsdaten im Ordner löschen:

```
$ Rscript delete_all_data.sh
```

Den vollständigen Datensatz kompilieren Sie mit folgendem Skript:

```
$ bash docker-run-project.sh
```





### Ergebnis

Der Datensatz und alle weiteren Ergebnisse sind nun im Ordner `output/` abgelegt.






## Pipeline visualisieren

Sie können die Pipeline visualisieren, aber nur nachdem sie die zentrale .Rmd-Datei mindestens einmal gerendert haben:

```
> targets::tar_glimpse()     # Nur Datenobjekte
> targets::tar_visnetwork()  # Alle Objekte
```





## Troubleshooting

Hilfreiche Befehle um Fehler zu lokalisieren und zu beheben.

```
> tar_progress()  # Zeigt Fortschritt und Fehler an
> tar_meta()      # Alle Metadaten
> tar_meta(fields = "warnings", complete_only = TRUE)  # Warnungen
> tar_meta(fields = "error", complete_only = TRUE)  # Fehlermeldungen
> tar_meta(fields = "seconds")  # Laufzeit der Targets
```





## Projektstruktur

Die folgende Struktur erläutert die wichtigsten Bestandteile des Projekts. Während der Kompilierung werden weitere Ordner erstellt (`files/`, `temp/`, `analysis/` und `output/`). Die Endergebnisse werden alle in `output/` abgelegt.

 
``` 
.
├── buttons                    # Buttons (nur optische Bedeutung)
├── CHANGELOG.md               # Alle Änderungen
├── config.toml                # Zentrale Konfigurations-Datei
├── data                       # Datensätze, auf denen die Pipeline aufbaut
├── delete_all_data.sh         # Löscht den Datensatz und Zwischenergebnisse
├── docker-build-image.sh      # Docker Image erstellen
├── docker-compose.yaml        # Konfiguration für Docker
├── docker-delete_all_data.sh  # Löscht den Datensatz und Zwischenergebnisse via Docker
├── Dockerfile                 # Definition des Docker Images
├── docker-run-project.sh      # Docker Image erstellen und Datensatz vie Docker kompilieren
├── etc                        # Konfigurations-Dateien
├── functions                  # Wichtige Schritte der Pipeline
├── gpg                        # Persönlicher Public GPG-Key für Seán Fobbe
├── LICENSE                    # Lizenz für Source Code
├── pipeline.Rmd               # Zentrale Definition der Pipeline
├── README.md                  # Bedienungsanleitung
├── reports                    # Markdown-Dateien
├── run_project.R              # Kompiliert den gesamten Datensatz
└── tex                        # LaTeX-Templates


``` 



 
## Weitere Open Access Veröffentlichungen (Fobbe)

Website — https://www.seanfobbe.de

Open Data  —  https://zenodo.org/communities/sean-fobbe-data/

Source Code  —  https://zenodo.org/communities/sean-fobbe-code/

Volltexte regulärer Publikationen  —  https://zenodo.org/communities/sean-fobbe-publications/



## Kontakt

Fehler gefunden? Anregungen? Kommentieren Sie gerne im Issue Tracker auf Codeberg oder schreiben Sie mir eine E-Mail an [fobbe-data@posteo.de](fobbe-data@posteo.de)
