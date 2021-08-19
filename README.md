*Die Seite ist noch im Aufbau und Sätze können im Nichts enden - sorry dafür*

# Wo ist was?

- [Hilfe! Wo finde ich was?](#hilfe-wo-finde-ich-was)
- [Youtube Playlisten](#youtube-playlisten)
	-  [Grundlagen in R (Level 1)](#grundlagen-in-r-level-1)
	-  [Programmieren in R (Level 2)](#programmieren-in-r-level-2)
	-  [Spielweise in R (Level 3)](#spielweise-in-r-level-3)
	-  [Grundlagen in Statistik und Data Science (Level 1)](#grundlagen-in-statistik-und-data-science-level-1)
	-  [Vertiefung in Statistik und Data Science (Level 2)](#vertiefung-in-statistik-und-data-science-level-2)
- [Vollständige Vorlesungen](#vollständige-vorlesungen)
	- [Statistik für Biowissenschaften I (WiSe 2020/21)](#statistik-für-biowissenschaften-i-wise-202021)
	- [Statistik für Biowissenschaften II (SoSe 2021)](#statistik-für-biowissenschaften-ii-sose-2021)
- [Literatur & Referenzen](#literatur)

# Hilfe! Wo finde ich was?

Hallo erstmal! Freut mich, dass du an meinen (Lehr)Materialien und Lehrvideos Interesse hast. Im Folgenden gibt es eine Auflistung von YouTube Playlisten, die es dir erlauben meinen Kursen besser zu folgen oder aber nachzuarbeiten. Über diesem Text findest du alle Dateien, die in den YouTube Videos angelegt und benutzt werden. Jedenfalls die meisten. 

Also dann Orientieren wir uns einmal. Grob gibt es nun zwei Möglichkeiten oder aber natürlich ein Misch-Masch daraus: 

1. Du hörst gar keine Vorlesung oder Kurs bei mir und bist einfach so hier gelandet. Das freut mich natürlich. Ich hoffe ich kann dir die Statistik näherbringen oder zu mindestens das erklären was du brauchst;
2. Du nimmst an einer meiner Vorlesungen oder Kursen teil und möchtest etwas wiederholen oder vorab lernen damit du mir besser Folgen kannst. Denn ist ja auch möglich sich Teile der Inhalte schon vorher anzuschauen. Häufig habe ich nicht die Zeit in alle Details der R Grundlagen einzugehen. Hier findest du dann die Videos, die nochmal alles langsam erklären.

## Ich höre keine Vorlesung / Kurs aktuell bei dir

Wenn du vollständige Vorlesungen nachhören willst, dann bist du unter dem Abschnitt [Vollständige Vorlesungen](#vollständige-vorlesungen) am Besten aufgehoben. Dort gibt es zwei Vorlesungen, die unabhängig von meinen aktuellen Kursen dir einen Einblick in die Statistik / Data Science geben. Statistik für Biowissenschaften I (WiSe 2020/21) und Statistik für Biowissenschaften II (SoSe 2021) bauen aufeinander auf. Beide Vorlesungen beinhalten R Code sowie weitreichende Erklärungen. **Wenn** du kein Kurs von mir hörst, sind die beiden Vorlesungen dein roter Faden. Die Vorlesungen geben aber **keine** Einführung in R. Da sei dir dann die YouTube Playlist [Grundlagen in R (Level 1)](#grundlagen-in-r-level-1) ans Herz gelegt.

## Ich bin in einer deiner Vorlesungen / Kurse

Ja, das macht die Sachlage *etwas* komplizierter. Aber auch hier gibt es Hilfe. Denn ich weiß natürlich nicht wo dir der Schuh zwickt. Du hast aber den Vorteil mich direkt in der Vorlesung einmal nach einem passenden Video zu fragen, wenn du mit ein bisschen Suchen nicht das passende findest. Ich versuche möglichst präzise in den Titeln und der Beschreibung zu sein. Manchmal hilft auch die Kapitel sich anzuschauen. **Wenn was fehlt, bitte sag mir Bescheid! Dann mache ich ein Video darüber. Danke für deine Mithilfe!**

Wenn du mit nicht bei der R Programmierung folgen kannst, dann solltest du dir die [Grundlagen in R (Level 1)](#grundlagen-in-r-level-1) Videos anschauen. In den ersten Videos erfährst du, wie man grundsätzlich mit R umgeht und was es für Besonderheiten in der Programmierung gibt, die dir vielleicht jetzt Rätsel aufgeben.

Wenn du ein Thema wiederholen willst oder die die Grundlagen zu schnell gingen, dann suche doch einmal in den [Grundlagen in Statistik und Data Science (Level 1)](#grundlagen-in-statistik-und-data-science-level-1) Videos für das passende Thema.

# Youtube Playlisten

**Hilfe sind das viele Videos**. Ja, da kommt schon was zusammen. Aber viele Videos sind nicht so lang oder aber durch die Kapitel unter den Videos sortiert. Das Raussuchen und sich mit den Videos beschäftigen gehört auch zum Lernprozess mit dazu und somit zum Teil auch gewollt. 

### R Codeblöcke

Du findest unter den GitHub Links jeweils die Seite, wo der R Code steht. Eigentlich ist es nur ein Link zu den Verzeichnissen weiter oben. Der R Code sieht so aus:
```r
## Das hier ist ein Kommentar
library(tidyverse) ## Laden eines zusätzlichen Paketes
```
Du kannst den Code einfach in R kopieren und schauen was er macht. Achtung, leider ist bei den komplizierten Beispielen nicht immer alles gleich lauffähig. Du musst die Pakete dann mit `install.packages(tidyverse)` installieren und mit `library(tidyverse)` laden. Mehr dazu in den Videos zu [Grundlagen in Statistik und Data Science (Level 1)](#grundlagen-in-statistik-und-data-science-level-1).

### Variablennamen, Klassennamen und Skalenniveau

Variablenname | Beispiel | R spezifisch | Infomatik allgemein | Skalenniveau 
--- | --- | --- | ---  | ---
treatment | "placebo", "aspirin" | character | character/string | 
dosis | low, mid, high | ordered |  | categorical / discrete
field | mainz, berlin, kiel | factor |  | categorical / discrete
age | 14, 83, 23, 45 | integer | integer | continuous
response | 12.3, 10.3, 9.1, 6.1 | numeric | double | continuous
dosis | 2 | 3 | 4 | g
age | 2 | 3 | 4 | g
age | 2 | 3 | 4 | g


[Tutorial: tbl_summary](http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html) - _R package gtsummary_

### Grundlagen in R (Level 1)
[Grundlagen in R ( Level 1)](https://www.youtube.com/playlist?list=PLe51bCp9JvEFUnFqaJG5aRmON9i1ZbOYC) - _Youtube Playlist_

[Skripte und Daten](https://github.com/jkruppa/teaching/tree/main/Grundlagen%20in%20R%20(Level%201)#readme) - _GitHub_

### Programmieren in R (Level 2) 
[Programmieren in R (Level 2)](https://www.youtube.com/playlist?list=PLe51bCp9JvEFp6T3BKhWGoC2mF-ID31co) - _Youtube Playlist_

[Skripte und Daten](https://github.com/jkruppa/teaching/tree/main/Programmieren%20in%20R%20(Level%202)#readme) - _GitHub_

### Spielweise in R (Level 3)
[Spielweise in R (Level 3)](https://www.youtube.com/playlist?list=PLe51bCp9JvEFZeYClBKad6yurjUzc8jXp) - _Youtube Playlist_

[Skripte und Daten](https://github.com/jkruppa/teaching/tree/main/Spielweise%20in%20R%20(Level%203)#readme) - _GitHub_

### Grundlagen in Statistik und Data Science (Level 1)
[Grundlagen in Statistik und Data Science (Level 1)](https://www.youtube.com/playlist?list=PLe51bCp9JvEHA3HK3OJfjyENEja0Omser) - _Youtube Playlist_

[Skripte und Daten](https://github.com/jkruppa/teaching/tree/main/Grundlagen%20in%20Statistik%20und%20Data%20Science%20(Level%201)#readme) - _GitHub_

### Vertiefung in Statistik und Data Science (Level 2)
[Vertiefung in Statistik und Data Science (Level 2)](https://www.youtube.com/playlist?list=PLe51bCp9JvEGYfNSmEPWBj6JGpNf8JNu6) - _Youtube Playlist_

[Skripte und Daten](https://github.com/jkruppa/teaching/tree/main/Vertiefung%20in%20Statistik%20und%20Data%20Science%20(Level%202)#readme) - _GitHub_

# Vollständige Vorlesungen

### Statistik für Biowissenschaften I (WiSe 2020/21)
[Statistik für Biowissenschaften I (WiSe 2020/21)](https://www.youtube.com/playlist?list=PLe51bCp9JvEHELp0HFqtAvUQg_VSkDc9n) - _Youtube Playlist_

### Statistik für Biowissenschaften II (SoSe 2021)
[Statistik für Biowissenschaften II (SoSe 2021)](https://www.youtube.com/playlist?list=PLe51bCp9JvEGVmH5pkJUnj-7Y-uK3_yV_) - _Youtube Playlist_

# Literatur

Bitte für Literaturempfehlungen mich nochmal ansprechen. Ich möchte hier kein Review über verschiedene Bücher schreiben. Die angegebenen spielgeln meine Lehrphilosophie wieder oder finde ich sehr empfehlenswert. Häufig sind die Bücher auch als eBook über Universitätsbüchereinen zu erhalten.

## Eigene Referenzen
- **Kruppa, J.**, Rohmann, J., Herrmann, C., Sieg, M., Rubarth, K., & Piper, S. (2021). What statistics instructors need to know about concept acquisition to make statistics stick. Journal of University Teaching & Learning Practice, 18(2), 02.
- Herrmann, C., Berger, U., Weiß, C., Burkholder, I., Rauch, G., & **Kruppa, J.** (2021) Zeig mir Health Data Science!.
- **Kruppa, J.**, & Kiehne, B. Statistik lebendig lehren durch Storytelling und forschungsbasiertes Lernen (2020). Beiträge zu Praxis, Praxisforschung und Forschung, 501.
- Rauch, G., Grittner, U., Neumann, K., Herrmann, C. & **Kruppa, J.** (2020). Medizinische Statistik für Dummies. Wiley-VCH.

## Weitere Quellen
- One to rule them all: https://www.tidyverse.org/ und natürlich das Buch dazu auch Online: https://r4ds.had.co.nz/
- Wickham, H., & Grolemund, G. (2016). R for data science: import, tidy, transform, visualize, and model data. O'Reilly Media, Inc. [Online hier: https://r4ds.had.co.nz/]
- Dormann, C. F. (2013). Parametrische Statistik. Springer Berlin Heidelberg. [Online hier: http://www.biom.uni-freiburg.de/mitarbeiter/dormann/dormann2012statistikskript.pdf]
