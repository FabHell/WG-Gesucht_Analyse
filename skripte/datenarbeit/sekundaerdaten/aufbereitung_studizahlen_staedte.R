

#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######      AGGREGATION ANZAHL STUDIS      #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


## Laden Pakete ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(DBI)


## Laden und aufbereiten Daten -------------------------------------------------

kontextdaten_städte <- read_excel("Daten/Kontextdaten_Städte/statistischer-bericht-studierende-hochschulen-endg-2110410257005.xlsx", 
                                  sheet = "csv-21311-12") %>%
  janitor::clean_names() 

kontextdaten_städte_aufb_1 <- kontextdaten_städte %>% 
  filter(hochschulstandort != "Zusammen" & hochschulart != "Hochschulen insgesamt") %>%
  mutate(
    studierende_studierende_deutsche_insgesamt = as.numeric(studierende_studierende_deutsche_insgesamt),
    studierende_studierende_auslaender_insgesamt = as.numeric(studierende_studierende_auslaender_insgesamt),
    studierende_studierende_maennlich = as.numeric(studierende_studierende_maennlich),
    studierende_studierende_weiblich = as.numeric(studierende_studierende_weiblich)
    )
  

## Aggregieren auf Stadtebene --------------------------------------------------

kontextdaten_städte_aufb_2 <- kontextdaten_städte_aufb_1 %>%
  filter(hochschulstandort != "Zusammen" & hochschulart != "Hochschulen insgesamt") %>%
  mutate(stadt = case_when(
    
    # UNIVERSITÄTEN
    hochschulstandort == "U Freiburg i.Br." ~ "Freiburg im Breisgau",
    hochschulstandort == "U Heidelberg in Heidelberg" ~ "Heidelberg",
    hochschulstandort == "U Heidelberg in Mannheim" ~ "Mannheim",
    hochschulstandort == "Karlsruher Institut für Technologie (KIT) - Bereich Hochschule (U)" ~ "Karlsruhe",
    hochschulstandort == "U Konstanz" ~ "Konstanz",
    hochschulstandort == "U Mannheim" ~ "Mannheim",
    hochschulstandort == "U Stuttgart" ~ "Stuttgart",
    hochschulstandort == "U Tübingen" ~ "Tübingen",
    hochschulstandort == "Freie Hochschule Stuttgart, Seminar für Waldorfpädagogik (Priv. U)" ~ "Stuttgart",
    hochschulstandort == "H für jüdische Studien Heidelberg (Kirchl. U)" ~ "Heidelberg",
    
    hochschulstandort == "U Augsburg" ~ "Augsburg",
    hochschulstandort == "U Erlangen-Nürnberg in Erlangen" ~ "Erlangen",
    hochschulstandort == "U Erlangen-Nürnberg in Nürnberg" ~ "Nürnberg",
    hochschulstandort == "Technische Universität Nürnberg" ~ "Nürnberg",
    hochschulstandort == "U München in München" ~ "München",
    hochschulstandort == "TU München in München" ~ "München",
    hochschulstandort == "U der Bundeswehr München (FB Universitätsstudiengänge)" ~ "München",
    hochschulstandort == "U der Bundeswehr München (FB Fachhochschulstudiengänge)" ~ "München",
    hochschulstandort == "H für Politik München (U)" ~ "München", 
    hochschulstandort == "U Regensburg" ~ "Regensburg",
    hochschulstandort == "Charlotte Fresenius Hochschule Wiesbaden in München (Priv. U)" ~ "München",
    
    hochschulstandort == "FU Berlin" ~ "Berlin",
    hochschulstandort == "TU Berlin" ~ "Berlin",
    hochschulstandort == "Charité - Universitätsmedizin Berlin" ~ "Berlin",
    hochschulstandort == "Humboldt-Universität Berlin" ~ "Berlin",
    hochschulstandort == "ESCP Europe Wirtschaftshochschule Berlin (Priv. U)" ~ "Berlin",
    hochschulstandort == "Europ. School of Management and Technology, Berlin (Priv. U)" ~ "Berlin",
    hochschulstandort == "Hertie School of Governance Berlin (Priv. U)" ~ "Berlin",
    hochschulstandort == "Steinbeis Hochschule in Berlin (Priv. U)" ~ "Berlin",
    hochschulstandort == "International Psychoanalytic University Berlin (Priv. U)" ~ "Berlin",
    hochschulstandort == "Psychologische Hochschule Berlin (Priv. U)" ~ "Berlin",
    hochschulstandort == "Bard College Berlin, A Liberal Arts University (Priv. U)" ~ "Berlin",
    hochschulstandort == "German International University Berlin (Priv. U)" ~ "Berlin",
    
    hochschulstandort == "U Potsdam" ~ "Potsdam",
    hochschulstandort == "HMU Health and Medical University Erfurt in Potsdam (Priv. U)" ~ "Potsdam",
    
    hochschulstandort == "U Bremen" ~ "Bremen",
    hochschulstandort == "Constructor University Bremen gGmbH (Priv. U)" ~ "Bremen",
    
    hochschulstandort == "U Hamburg" ~ "Hamburg",
    hochschulstandort == "Technische Universität Hamburg" ~ "Hamburg",
    hochschulstandort == "HafenCity Universität Hamburg" ~ "Hamburg",
    hochschulstandort == "Helmut-Schmidt-Universität Hamburg" ~ "Hamburg",
    hochschulstandort == "Bucerius Law School Hamburg (Priv. U)" ~ "Hamburg",
    hochschulstandort == "KLU Kühne Logistics University (Priv. U)" ~ "Hamburg",
    hochschulstandort == "Charlotte Fresenius Hochschule Wiesbaden in Hamburg (Priv. U)" ~ "Hamburg",
    
    hochschulstandort == "TU Darmstadt" ~ "Darmstadt",
    hochschulstandort == "U Frankfurt a.M." ~ "Frankfurt am Main",
    hochschulstandort == "U Gießen" ~ "Gießen",
    hochschulstandort == "U Kassel in Kassel (ohne Kunsthochschule)" ~ "Kassel",
    hochschulstandort == "U Kassel in Kassel (Kunsthochschule)" ~ "Kassel",
    hochschulstandort == "U Marburg" ~ "Marburg",
    hochschulstandort == "Frankfurt School of Finance & Management-HfB (Priv. U)" ~ "Frankfurt am Main",
    hochschulstandort == "Charlotte Fresenius Hochschule Wiesbaden in Wiesbaden (Priv. U)" ~ "Wiesbaden",
    
    hochschulstandort == "U Greifswald" ~ "Greifswald",
    hochschulstandort == "U Rostock" ~ "Rostock",
    
    hochschulstandort == "TU Braunschweig" ~ "Braunschweig",
    hochschulstandort == "U Göttingen" ~ "Göttingen",
    hochschulstandort == "U Hannover" ~ "Hannover",
    hochschulstandort == "Medizinische H Hannover (U)" ~ "Hannover",
    hochschulstandort == "Tierärztliche H Hannover (U)" ~ "Hannover",
    hochschulstandort == "U Oldenburg" ~ "Oldenburg",
    hochschulstandort == "U Osnabrück" ~ "Osnabrück",
    
    hochschulstandort == "TH Aachen (U)" ~ "Aachen",
    hochschulstandort == "U Bielefeld" ~ "Bielefeld",
    hochschulstandort == "U Bochum" ~ "Bochum",
    hochschulstandort == "U Bonn" ~ "Bonn",
    hochschulstandort == "TU Dortmund" ~ "Dortmund",
    hochschulstandort == "U Düsseldorf" ~ "Düsseldorf",
    hochschulstandort == "U Duisburg-Essen in Essen" ~ "Essen",
    hochschulstandort == "U Köln" ~ "Köln",
    hochschulstandort == "Deutsche Sporthochschule Köln (U)" ~ "Köln",
    hochschulstandort == "U Münster" ~  "Münster",
    hochschulstandort == "U Paderborn" ~ "Paderborn",
    hochschulstandort == "U Wuppertal" ~ "Wuppertal",
    hochschulstandort == "Charlotte Fresenius Hochschule Wiesbaden in Köln (Priv. U)" ~ "Köln",
    hochschulstandort == "Deutsche Hochschule der Polizei, Münster (U)" ~ "Münster",
    
    hochschulstandort == "Rheinland-Pfälzische Technische Universität in Kaiserslautern" ~ "Kaiserslautern",
    hochschulstandort == "Universität Koblenz" ~ "Koblenz",
    hochschulstandort == "U Mainz in Mainz" ~ "Mainz",
    hochschulstandort == "U Trier" ~ "Trier",
    
    hochschulstandort == "U des Saarlandes Saarbrücken in Saarbrücken" ~ "Saarbrücken",
    
    hochschulstandort == "TU Chemnitz" ~ "Chemnitz",
    hochschulstandort == "TU Dresden in Dresden" ~ "Dresden",
    hochschulstandort == "U Leipzig" ~ "Leipzig",
    hochschulstandort == "DIU-Dresden International University (Priv. U)" ~ "Dresden",
    hochschulstandort == "HHL Leipzig Graduate School of Management (Priv. U)" ~ "Leipzig",
    
    hochschulstandort == "U Halle in Halle" ~ "Halle (Saale)",
    hochschulstandort == "U Magdeburg" ~ "Magdeburg",
    
    hochschulstandort == "EUF Europa-Universität Flensburg" ~ "Flensburg",
    hochschulstandort == "U Kiel" ~ "Kiel",
    hochschulstandort == "U Lübeck" ~ "Lübeck",
    
    hochschulstandort == "U Erfurt" ~ "Erfurt",
    hochschulstandort == "U Jena" ~ "Jena",
    hochschulstandort == "HMU Health and Medical University Erfurt in Erfurt (Priv. U)" ~ "Erfurt",
    
    # PÄDAGOGISCHE HOCHSCHULEN
    hochschulstandort == "PH Freiburg i.Br." ~ "Freiburg im Breisgau",
    hochschulstandort == "PH Heidelberg" ~ "Heidelberg",
    hochschulstandort == "PH Karlsruhe" ~ "Karlsruhe",
    
    # THEOLOGISCHE HOCHSCHULEN
    hochschulstandort == "H für Philosophie München (Kirchl.-Theol. H)" ~ "München",
    hochschulstandort == "Phil.-Theol. H Frankfurt a.M. (Kirchl.-Theol. H)" ~ "Frankfurt am Main",
    hochschulstandort == "Freie Theologische H (FTH) Gießen (Priv.-Theol. H)" ~ "Gießen",
    hochschulstandort == "Evangelische Hochschule Tabor in Marburg (Priv.-Theol. H)" ~ "Marburg",
    hochschulstandort == "Phil.-Theol. H Münster (Kirchl.-Theol. H)" ~ "Münster",
    hochschulstandort == "Theol. Fakultät Paderborn (Kirchl.-Theol. H)" ~ "Paderborn",
    hochschulstandort == "Kölner H für Katholische Theologie (Kirchl.-Theol. H)" ~ "Köln",
    hochschulstandort == "Kirchliche Hochschule Wuppertal (Kirchl.-Theol. H)" ~ "Wuppertal",
    hochschulstandort == "Theol. Fakultät Trier (Kirchl.-Theol. H)" ~ "Trier",
    
    # KUNSTHOCHSCHULEN
    hochschulstandort == "Staatl. H für Musik Freiburg i.Br. (Kunst-H)" ~ "Freiburg im Breisgau",
    hochschulstandort == "Staatl. Akademie der Bildenden Künste Karlsruhe (Kunst-H)" ~ "Karlsruhe",
    hochschulstandort == "Staatl. H für Gestaltung Karlsruhe (Kunst-H)" ~ "Karlsruhe",
    hochschulstandort == "Staatl. H für Musik Karlsruhe (Kunst-H)" ~ "Karlsruhe",
    hochschulstandort == "Staatl. H für Musik und Darstellende Kunst Mannheim (Kunst-H)" ~ "Mannheim",
    hochschulstandort == "Staatl. Akademie der Bildenden Künste Stuttgart (Kunst-H)" ~ "Stuttgart",
    hochschulstandort == "Staatl. H für Musik und Darstellende Kunst Stuttgart (Kunst-H)" ~ "Stuttgart",
    
    hochschulstandort == "Akademie der Bildenden Künste München (Kunst-H)" ~ "München",
    hochschulstandort == "H für Fernsehen und Film München (Kunst-H)" ~ "München",
    hochschulstandort == "H für Musik und Theater München (Kunst-H)" ~ "München",
    hochschulstandort == "Akademie der Bildenden Künste Nürnberg (Kunst-H)" ~ "Nürnberg",
    hochschulstandort == "H für Musik Nürnberg (Kunst-H)" ~ "Nürnberg",
    hochschulstandort == "H für kath. Kirchenmusik und Musikpädagogik, Regensburg (Kirchl. Kunst-H)" ~ "Regensburg",
    
    hochschulstandort == "U der Künste Berlin (Kunst-H)" ~ "Berlin",
    hochschulstandort == "Weißensee Kunsthochschule Berlin (Kunst-H)" ~ "Berlin",
    hochschulstandort == "H für Musik Berlin (Kunst-H)" ~ "Berlin",
    hochschulstandort == "H für Schauspielkunst Berlin (Kunst-H)" ~ "Berlin",
    hochschulstandort == "Barenboim-Said Akademie Berlin (Priv. Kunst-H)" ~ "Berlin",
    
    hochschulstandort == "H für Künste Bremen (Kunst-H)" ~ "Bremen",
    
    hochschulstandort == "H für Bildende Künste Hamburg (Kunst-H)" ~ "Hamburg",
    hochschulstandort == "H für Musik und Theater Hamburg (Kunst-H)" ~ "Hamburg",
    
    hochschulstandort == "H für Bildende Künste - Städelschule Frankfurt a.M. (Kunst-H)" ~ "Frankfurt am Main",
    hochschulstandort == "H für Musik und Darstellende Kunst Frankfurt a.M. (Kunst-H)" ~ "Frankfurt am Main",

    hochschulstandort == "H für Musik und Theater Rostock (Kunst-H)" ~ "Rostock",

    hochschulstandort == "H für Bildende Künste Braunschweig (Kunst-H)" ~ "Braunschweig",
    hochschulstandort == "Hochschule für Musik, Theater und Medien Hannover (Kunst-H)" ~ "Hannover",
    
    hochschulstandort == "Kunstakademie Düsseldorf (Kunst-H)" ~ "Düsseldorf",
    hochschulstandort == "Robert-Schumann-H Düsseldorf (Kunst-H)" ~ "Düsseldorf",
    hochschulstandort == "Folkwang U der Künste Essen in Bochum (Kunst-H)" ~ "Bochum",
    hochschulstandort == "Folkwang U der Künste Essen in Essen (Kunst-H)" ~ "Essen",
    hochschulstandort == "KH für Medien Köln (Kunst-H)" ~ "Köln",
    hochschulstandort == "H für Musik und Tanz Köln in Aachen (Kunst-H)" ~ "Aachen",
    hochschulstandort == "H für Musik und Tanz Köln in Köln (Kunst-H)" ~ "Köln",
    hochschulstandort == "H für Musik und Tanz Köln in Wuppertal (Kunst-H)" ~ "Wuppertal",
    hochschulstandort == "Kunstakademie Münster (Kunst-H)" ~ "Münster",
    hochschulstandort == "Hochschule der bildenden Künste (HBK) Essen  (Priv. Kunst-H)" ~ "Essen",
    
    hochschulstandort == "H der Bildenden Künste Saarbrücken (Kunst-H)" ~ "Saarbrücken",
    hochschulstandort == "Hochschule für Musik Saarbrücken (Kunst-H)" ~ "Saarbrücken",
    
    hochschulstandort == "H für Bildende Künste Dresden (Kunst-H)" ~ "Dresden",
    hochschulstandort == "H für Kirchenmusik der Evang.-Luth. Landeskirche Sachsens, Dresden (Kirchl. Kunst-H)" ~ "Dresden",
    hochschulstandort == "H für Musik Dresden (Kunst-H)" ~ "Dresden",
    hochschulstandort == "Palucca Hochschule für Tanz Dresden (Kunst-H)" ~ "Dresden",
    hochschulstandort == "H für Graphik und Buchkunst Leipzig (Kunst-H)" ~ "Leipzig",
    hochschulstandort == "H für Musik und Theater Leipzig (Kunst-H)" ~ "Leipzig",
    
    hochschulstandort == "Burg Giebichenstein Kunsthochschule Halle (Kunst-H)" ~ "Halle (Saale)",
    hochschulstandort == "Evang. H für Kirchenmusik Halle (Kirchl. Kunst-H)" ~ "Halle (Saale)",
    
    hochschulstandort == "Muthesius Kunsthochschule Kiel (Kunst-H)" ~ "Kiel",
    hochschulstandort == "Musikhochschule Lübeck (Kunst-H)" ~ "Lübeck",
    
    # FACHHOCHSCHULEN
    hochschulstandort == "H Karlsruhe (FH)" ~ "Karlsruhe",
    hochschulstandort == "H Konstanz (FH)" ~ "Konstanz",
    hochschulstandort == "Hochschule für angewandte Wissenschaften der Bundesagentur für Arbeit in Mannheim (FH)" ~ "Mannheim",
    hochschulstandort == "H Mannheim (FH)" ~ "Mannheim",
    hochschulstandort == "Duale Hochschule Baden-Württemberg Karlsruhe" ~ "Karlsruhe",
    hochschulstandort == "Duale Hochschule Baden-Württemberg Mannheim" ~ "Mannheim",
    hochschulstandort == "Duale Hochschule Baden-Württemberg Stuttgart" ~ "Stuttgart",
    hochschulstandort == "Hochschule für Technik Stuttgart (FH)" ~ "Stuttgart",
    hochschulstandort == "H für Ökonomie und Management Essen in Mannheim (Priv. FH)" ~ "Mannheim",
    hochschulstandort == "H für Ökonomie und Management Essen in Stuttgart (Priv. FH)" ~ "Stuttgart",
    hochschulstandort == "H für Ökonomie und Management Essen in Karlsruhe (Priv. FH)" ~ "Karlsruhe",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Mannheim (Priv. FH)" ~ "Mannheim",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Stuttgart (Priv. FH)" ~ "Stuttgart",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Freiburg (Priv. FH)" ~ "Freiburg im Breisgau",
    hochschulstandort == "Hochschule Macromedia für angewandte Wissenschaften Stuttgart, Campus Stuttgart (Priv. FH)" ~ "Stuttgart",
    hochschulstandort == "Hochschule Macromedia für angewandte Wissenschaften Stuttgart, Campus Freiburg i. Br. (Priv. FH)" ~ "Freiburg im Breisgau",
    hochschulstandort == "International School of Management Dortmund in Stuttgart (Priv. FH)" ~ "Stuttgart",
    hochschulstandort == "SRH University of Applied Sciences Heidelberg in Heidelberg (Priv. FH)" ~ "Heidelberg",
    hochschulstandort == "Karlshochschule International University, Karlsruhe (Priv. FH)" ~ "Karlsruhe",
    hochschulstandort == "Hochschule der Wirtschaft für Management (HdWM) Mannheim (Priv. FH)" ~ "Mannheim",
    hochschulstandort == "Hochschule für Kommunikation und Gestaltung in Stuttgart (Priv. FH)" ~ "Stuttgart",
    hochschulstandort == "Hochschule Fresenius Heidelberg (Priv. FH)" ~ "Heidelberg",
    hochschulstandort == "Allensbach Hochschule Konstanz (Priv. FH)" ~ "Konstanz",
    hochschulstandort == "Merz Akademie Hochschule für Gestaltung, Kunst und Medien, Stuttgart (Priv. FH)" ~ "Stuttgart",
    hochschulstandort == "IB Hochschule für Gesundheit und Soziales Berlin in Stuttgart (Priv. FH)" ~ "Stuttgart",
    hochschulstandort == "Evang. Hochschule Freiburg (Kirchl. FH)" ~ "Freiburg im Breisgau",
    hochschulstandort == "Kath. Hochschule Freiburg i.Br., Campus Freiburg (Kirchl. FH)" ~ "Freiburg im Breisgau",
    hochschulstandort == "Kath. Hochschule Freiburg i.Br., Campus Stuttgart (Kirchl. FH)" ~ "Stuttgart",
    
    hochschulstandort == "Technische Hochschule Augsburg (FH)" ~ "Augsburg",
    hochschulstandort == "Hochschule für angewandte Wissenschaften München (FH)" ~ "München",
    hochschulstandort == "Ostbayerische Technische Hochschule Regensburg (FH)" ~ "Regensburg",
    hochschulstandort == "Mediadesign Hochschule Berlin in München (Priv. FH)" ~ "München",
    hochschulstandort == "IB Hochschule für Gesundheit und Soziales Berlin in München (Priv. FH)" ~ "München",
    hochschulstandort == "HDBW Hochschule der Bayerischen Wirtschaft für angewandte Wissenschaften in München (Priv. FH)" ~ "München",
    hochschulstandort == "H für Ökonomie und Management Essen in Augsburg (Priv. FH)" ~ "Augsburg",
    hochschulstandort == "H für Ökonomie und Management Essen in München (Priv. FH)" ~ "München",
    hochschulstandort == "H für Ökonomie und Management Essen in Nürnberg (Priv. FH)" ~ "Nürnberg",
    hochschulstandort == "Hochschule Fresenius Idstein in München (Priv. FH)" ~ "München",
    hochschulstandort == "Internationale Hochschule SDI München-Hochschule für angewandte Wissenschaften (Priv. FH)" ~ "München",
    hochschulstandort == "Hochschule Macromedia für angewandte Wissenschaften Stuttgart, Campus München (Priv. FH)" ~ "München",
    hochschulstandort == "Munich Business School München (Priv. FH)" ~ "München",
    hochschulstandort == "HSD Hochschule Döpfer Potsdam in Regensburg (Priv. FH)" ~ "Regensburg",
    hochschulstandort == "Evang. Hochschule Nürnberg (Kirchl. FH)" ~ "Nürnberg",
    hochschulstandort == "Katholische Stiftungshochschule München in München (Kirchl. FH)" ~ "München",
    hochschulstandort == "IU Internationale Hochschule Erfurt in München (Priv. FH)" ~ "München",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Nürnberg (Priv. FH)" ~ "Nürnberg",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Augsburg (Priv. FH)" ~ "Augsburg",
    hochschulstandort == "International School of Management Dortmund in München (Priv. FH)" ~ "München",

    hochschulstandort == "Alice Salomon Hochschule Berlin (FH)" ~ "Berlin",
    hochschulstandort == "Berliner Hochschule für Technik (FH)" ~ "Berlin",
    hochschulstandort == "H für Technik und Wirtschaft Berlin (FH)" ~ "Berlin",
    hochschulstandort == "HWR Berlin, Fachbereich Wirtschaft (FH)" ~ "Berlin",
    hochschulstandort == "HWR Berlin, Fachbereich  Duales Studium (FH)" ~ "Berlin",
    hochschulstandort == "HWR Berlin, Fachbereich Verwaltung, Recht, Polizei (FH)" ~ "Berlin",
    hochschulstandort == "Touro College Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "BSP Business and Law School - Hochschule für Management und Recht Berlin in Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "bbw Hochschule Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "Akkon Hochschule Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "Hochschule Macromedia für angewandte Wissenschaften Stuttgart, Campus Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "Medical School Berlin, H für Gesundheit und Medizin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "Digital Business University Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "IB Hochschule für Gesundheit und Soziales Berlin in Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "Media University of Applied Sciences Berlin, Campus Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "H für Ökonomie und Management Essen in Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "Deutsche Hochschule für Gesundheit und Sport Berlin in Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "Mediadesign Hochschule Berlin in Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "FH des Mittelstandes (FHM) in Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "Humanistische Hochschule Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "SRH University of Applied Sciences Heidelberg in Berlin (Priv. FH)" ~ "Berlin", 
    hochschulstandort == "CODE University of Applied Sciences Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "Europäische FH (EUFH) in Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "International School of Management Dortmund in Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "Hochschule für Soziale Arbeit und Pädagogik (HSAP), Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "VICTORIA - Internationale Hochschule Berlin in Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "Berlin International University of Applied Sciences (Priv. FH)" ~ "Berlin",
    hochschulstandort == "Quadriga Hochschule Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "Hochschule Fresenius Idstein in Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "Evangelische Hochschule Berlin (Kirchl. FH)" ~ "Berlin",
    hochschulstandort == "University of Europe for Applied Sciences Potsdam in Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Berlin (Priv. FH)" ~ "Berlin",
    hochschulstandort == "Katholische Hochschule für Sozialwesen Berlin (Kirchl. FH)" ~ "Berlin",

    hochschulstandort == "FH Potsdam" ~ "Potsdam",
    hochschulstandort == "Fachhochschule für Sport und Management Potsdam (Priv. FH)" ~ "Potsdam",
    hochschulstandort == "HSD Hochschule Döpfer Potsdam in Potsdam (Priv. FH)" ~ "Potsdam",
    hochschulstandort == "University of Europe for Applied Sciences Potsdam in Potsdam (Priv. FH)" ~ "Potsdam",
    hochschulstandort == "GU- Deutsche Hochschule für angewandte Wissenschaften, Potsdam (Priv. FH)" ~ "Potsdam",
    hochschulstandort == "XU Exponential University Potsdam (Priv. FH)" ~ "Potsdam",
    hochschulstandort == "Gisma University of Applied Sciences Potsdam (Priv. FH)" ~ "Potsdam",

    hochschulstandort == "H Bremen (FH)" ~ "Bremen",
    hochschulstandort == "APOLLON H der Gesundheitswirtschaft Bremen (Priv. FH)" ~ "Bremen",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Bremen (Priv. FH)" ~ "Bremen",
    hochschulstandort == "H für Ökonomie und Management Essen in Bremen (Priv. FH)" ~ "Bremen",
    
    hochschulstandort == "H für Angewandte Wissenschaften Hamburg (FH)" ~ "Hamburg",
    hochschulstandort == "Berufliche Hochschule Hamburg (FH)" ~ "Hamburg",
    hochschulstandort == "HSBA Hamburg School of Business Admin. (Priv. FH)" ~ "Hamburg",
    hochschulstandort == "Evang. H für Soziale Arbeit und Diakonie, Hamburg (Kirchl. FH)" ~ "Hamburg",
    hochschulstandort == "Northern Business School Hamburg (Priv. FH)" ~ "Hamburg",
    hochschulstandort == "Hochschule Fresenius Idstein in Hamburg (Priv. FH)" ~ "Hamburg",
    hochschulstandort == "Brand University Hamburg (Priv. FH)" ~ "Hamburg",
    hochschulstandort == "International School of Management Dortmund in Hamburg (Priv. FH)" ~ "Hamburg",
    hochschulstandort == "MSH Medical School Hamburg (Priv. FH)" ~ "Hamburg",
    hochschulstandort == "SRH University of Applied Sciences Heidelberg in Hamburg (Priv. FH)" ~ "Hamburg",
    hochschulstandort == "H für Ökonomie und Management Essen in Hamburg (Priv. FH)" ~ "Hamburg",
    hochschulstandort == "BSP Business and Law School - Hochschule für Management und Recht Berlin in Hamburg (Priv. FH)" ~ "Hamburg",
    hochschulstandort == "IB Hochschule für Gesundheit und Soziales Berlin in Hamburg (Priv. FH)" ~ "Hamburg",
    hochschulstandort == "University of Europe for Applied Sciences Potsdam in Hamburg (Priv. FH)" ~ "Hamburg",
    hochschulstandort == "Hochschule Macromedia für angewandte Wissenschaften Stuttgart, Campus Hamburg (Priv. FH)" ~ "Hamburg",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Hamburg (Priv. FH)" ~ "Hamburg",

    hochschulstandort == "h_da - H Darmstadt in Darmstadt (FH)" ~ "Darmstadt",
    hochschulstandort == "Frankfurt University of Applied Sciences (FH)" ~ "Frankfurt am Main",
    hochschulstandort == "Technische Hochschule Mittelhessen (THM) in Gießen (FH)" ~ "Gießen",
    hochschulstandort == "Wilhelm Büchner Hochschule Darmstadt (Priv. Fern-FH)" ~ "Darmstadt",
    hochschulstandort == "Provadis School of Intern. Management and Technology, Frankfurt a.M. (Priv. FH)" ~ "Frankfurt am Main",
    hochschulstandort == "Tomorrow University of Applied Sciences, Frankfurt a. M. (Priv.FH)" ~ "Frankfurt am Main",
    hochschulstandort == "Hochschule Fresenius Idstein in Frankfurt (Priv. FH)" ~ "Frankfurt am Main",
    hochschulstandort == "Hochschule Fresenius Idstein in Wiesbaden (Priv. FH)" ~ "Wiesbaden",
    hochschulstandort == "UoL-University of Labour Frankfurt (Priv. FH)" ~ "Frankfurt am Main",
    hochschulstandort == "DIPLOMA - FH Nordhessen in Kassel (Priv. FH)" ~ "Kassel",
    hochschulstandort == "Media University of Applied Sciences Berlin, Campus Frankfurt (Priv. FH)" ~ "Frankfurt am Main",
    hochschulstandort == "Evangelische Hochschule Darmstadt (EHD) in Darmstadt (Kirchl. FH)" ~ "Darmstadt",
    hochschulstandort == "CVJM-Hochschule Kassel (Priv. FH)" ~ "Kassel",
    hochschulstandort == "H für Ökonomie und Management Essen in Kassel (Priv. FH)" ~ "Kassel",
    hochschulstandort == "H für Ökonomie und Management Essen in Frankfurt (Priv. FH)" ~ "Frankfurt am Main",
    hochschulstandort == "Hochschule Macromedia für angewandte Wissenschaften Stuttgart, Campus Frankfurt a. M. (Priv. FH)" ~ "Frankfurt am Main",
    hochschulstandort == "International School of Management Dortmund in Frankfurt (Priv. FH)" ~ "Frankfurt am Main",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Frankfurt (Priv. FH)" ~ "Frankfurt am Main",
    
    hochschulstandort == "FH des Mittelstandes (FHM) in Rostock (Priv. FH)" ~ "Rostock",
    hochschulstandort == "Europäische FH (EUFH) in Rostock (Priv. FH)" ~ "Rostock",
    
    hochschulstandort == "Hochschule Hannover (FH)" ~ "Hannover",
    hochschulstandort == "Hochschule Hildesheim/Holzminden/Göttingen in Göttingen (FH)" ~ "Göttingen",
    hochschulstandort == "Hochschule Wilhelmshaven/Oldenburg/Elsfleth in Oldenburg (FH)" ~ "Oldenburg",
    hochschulstandort == "Hochschule Osnabrück in Osnabrück (FH)" ~ "Osnabrück",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Hannover (Priv. FH)" ~ "Hannover",
    hochschulstandort == "Private Hochschule Göttingen (Priv. FH)" ~ "Göttingen",
    hochschulstandort == "Fachhochschule für die Wirtschaft Hannover (FHDW) (Priv. FH)" ~ "Hannover",
    hochschulstandort == "H für Ökonomie und Management Essen in Hannover (Priv. FH)" ~ "Hannover",
    hochschulstandort == "FH des Mittelstandes (FHM) in Hannover (Priv. FH)" ~ "Hannover",
    hochschulstandort == "Leibniz - Fachhochschule Hannover (Priv. FH)" ~ "Hannover",
    
    hochschulstandort == "FH Aachen in Aachen" ~ "Aachen",
    hochschulstandort == "Hochschule Bielefeld in Bielefeld (FH)" ~ "Bielefeld",
    hochschulstandort == "Hochschule Bochum (FH) in Bochum" ~ "Bochum",
    hochschulstandort == "Hochschule für Gesundheit Bochum (FH)" ~ "Bochum",
    hochschulstandort == "FH Dortmund" ~ "Dortmund",
    hochschulstandort == "H Düsseldorf (FH)" ~ "Düsseldorf",
    hochschulstandort == "Technische Hochschule Köln (FH) in Köln" ~ "Köln",
    hochschulstandort == "FH Münster in Münster" ~ "Münster",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Dortmund (Priv. FH)" ~ "Dortmund",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Düsseldorf (Priv. FH)" ~ "Düsseldorf",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Essen (Priv. FH)" ~ "Essen",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Köln (Priv. FH)" ~ "Köln",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Münster (Priv. FH)" ~ "Münster",
    hochschulstandort == "IB Hochschule für Gesundheit und Soziales Berlin in Köln (Priv. FH)" ~ "Köln",
    hochschulstandort == "Mediadesign Hochschule Berlin in Düsseldorf (Priv. FH)" ~ "Düsseldorf",
    hochschulstandort == "FH des Mittelstandes (FHM) in Bielefeld (Priv. FH)" ~ "Bielefeld",
    hochschulstandort == "FH des Mittelstandes (FHM) in Köln (Priv. FH)" ~ "Köln",
    hochschulstandort == "Fliedner Fachhochschule Düsseldorf (Priv. FH)" ~ "Düsseldorf",
    hochschulstandort == "Kolping H, Köln (Priv. FH)" ~ "Köln",
    hochschulstandort == "TH Georg Agricola Bochum (Priv. FH)" ~ "Bochum",
    hochschulstandort == "INU - Innovative University of Applied Sciences, Köln (Priv. FH)" ~ "Köln",
    hochschulstandort == "EBZ Business School Bochum (Priv. FH)" ~ "Bochum",
    hochschulstandort == "H für Finanzwirtschaft & Management Bonn (Priv. FH)" ~ "Bonn",
    hochschulstandort == "Europäische FH (EUFH) in Köln (Priv. FH)" ~ "Köln",
    hochschulstandort == "International School of Management Dortmund in Dortmund (Priv. FH)" ~ "Dortmund",
    hochschulstandort == "International School of Management Dortmund in Köln (Priv. FH)" ~ "Köln",
    hochschulstandort == "H für Ökonomie und Management Essen in Aachen (Priv. FH)" ~ "Aachen",
    hochschulstandort == "H für Ökonomie und Management Essen in Bochum (Priv. FH)" ~ "Bochum",
    hochschulstandort == "H für Ökonomie und Management Essen in Bonn (Priv. FH)" ~ "Bonn",
    hochschulstandort == "H für Ökonomie und Management Essen in Dortmund (Priv. FH)" ~ "Dortmund",
    hochschulstandort == "H für Ökonomie und Management Essen in Düsseldorf (Priv. FH)" ~ "Düsseldorf",
    hochschulstandort == "H für Ökonomie und Management Essen in Essen (Priv. FH)" ~ "Essen",
    hochschulstandort == "H für Ökonomie und Management Essen in Köln (Priv. FH)" ~ "Köln",
    hochschulstandort == "H für Ökonomie und Management Essen in Münster (Priv. FH)" ~ "Münster",
    hochschulstandort == "H für Ökonomie und Management Essen in Wuppertal (Priv. FH)" ~ "Wuppertal",
    hochschulstandort == "IST-Hochschule für Management Düsseldorf (Priv. FH)" ~ "Düsseldorf",
    hochschulstandort == "Hochschule Macromedia für angewandte Wissenschaften Stuttgart, Campus Köln (Priv. FH)" ~ "Köln",
    hochschulstandort == "CBS International Business School, Campus Köln (Priv. FH)" ~ "Köln",
    hochschulstandort == "Hochschule Fresenius Idstein in Köln (Priv. FH)" ~ "Köln",
    hochschulstandort == "Hochschule Fresenius Idstein in Düsseldorf (Priv. FH)" ~ "Düsseldorf",
    hochschulstandort == "Rheinische Hochschule Köln in Köln (Priv. FH)" ~ "Köln",
    hochschulstandort == "HSD Hochschule Döpfer Potsdam in Köln (Priv. FH)" ~ "Köln",
    hochschulstandort == "FH der Wirtschaft Paderborn in Bielefeld (Priv. FH)" ~ "Bielefeld",
    hochschulstandort == "FH der Wirtschaft Paderborn in Paderborn (Priv. FH)" ~ "Paderborn",
    hochschulstandort == "Media University of Applied Sciences Berlin, Campus Köln (Priv. FH)" ~ "Köln",
    hochschulstandort == "Evang. Hochschule Rheinland-Westfalen-Lippe, Bochum (Kirchl. FH)" ~ "Bochum",
    hochschulstandort == "Kath. Hochschule Nordrhein-Westfalen in Aachen (Kirchl. FH)" ~ "Aachen",
    hochschulstandort == "Kath. Hochschule Nordrhein-Westfalen in Köln (Kirchl. FH)" ~ "Köln",
    hochschulstandort == "Kath. Hochschule Nordrhein-Westfalen in Münster (Kirchl. FH)" ~ "Münster",
    hochschulstandort == "Kath. Hochschule Nordrhein-Westfalen in Paderborn (Kirchl. FH)" ~ "Paderborn",
    
    hochschulstandort == "Hochschule Kaiserslautern in Kaiserslautern (FH)" ~ "Kaiserslautern",
    hochschulstandort == "Hochschule Koblenz (FH) in Koblenz" ~ "Koblenz",
    hochschulstandort == "Hochschule Mainz (FH)" ~ "Mainz",
    hochschulstandort == "Hochschule Trier (FH) in Trier" ~ "Trier",
    hochschulstandort == "Katholische Hochschule Mainz (Kirchl. FH)" ~ "Mainz",
    hochschulstandort == "H für Gesellschaftsgestaltung Koblenz (Priv. FH)" ~ "Koblenz",
    hochschulstandort == "CBS International Business School, Campus Mainz (Priv. FH)" ~ "Mainz",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Mainz (Priv. FH)" ~ "Mainz",
    
    hochschulstandort == "H für Technik und Wirtschaft des Saarlandes Saarbrücken (FH)" ~ "Saarbrücken",
    hochschulstandort == "Deutsche Hochschule für Prävention und Gesundheitsmanagement, Saarbrücken (Priv. FH)" ~ "Saarbrücken",
    hochschulstandort == "H für Ökonomie und Management Essen in Saarbrücken (Priv. FH)" ~ "Saarbrücken",
    
    hochschulstandort == "H für Technik und Wirtschaft Dresden, Hochschule für angewandte Wissenschaften (FH)" ~ "Dresden",
    hochschulstandort == "H für Technik, Wirtschaft und Kultur Leipzig, Hochschule für angewandte Wissenschaften (FH)" ~ "Leipzig",
    hochschulstandort == "Hochschule Macromedia für angewandte Wissenschaften Stuttgart, Campus Leipzig (Priv. FH)" ~ "Leipzig",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Leipzig (Priv. FH)" ~ "Leipzig",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Dresden (Priv. FH)" ~ "Dresden",
    hochschulstandort == "H für Ökonomie und Management Essen in Leipzig (Priv. FH)" ~ "Leipzig",
    hochschulstandort == "SRH University of Applied Sciences Heidelberg in Dresden (Priv. FH)" ~ "Dresden",
    hochschulstandort == "Evangelische Hochschule Dresden in Dresden (Kirchl. FH)" ~ "Dresden",
    hochschulstandort == "Fachhochschule Dresden (Priv. FH)" ~ "Dresden",
    
    hochschulstandort == "H Magdeburg-Stendal in Magdeburg (FH)" ~ "Magdeburg",
    hochschulstandort == "Steinbeis Hochschule in Magdeburg (Priv. FH)" ~ "Magdeburg",
    
    hochschulstandort == "Hochschule Flensburg (FH)" ~ "Flensburg",
    hochschulstandort == "FH Kiel" ~ "Kiel",
    hochschulstandort == "Technische Hochschule Lübeck (FH)" ~ "Lübeck",
    hochschulstandort == "DHSH - Duale Hochschule Schleswig-Holstein, Kiel (Priv. FH)" ~ "Kiel",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Lübeck (Priv. FH)" ~ "Lübeck",
    
    hochschulstandort == "FH Erfurt" ~ "Erfurt",
    hochschulstandort == "Ernst-Abbe-Hochschule Jena (FH)" ~ "Jena",
    hochschulstandort == "IU Internationale Hochschule Erfurt in Erfurt (Priv. FH)" ~ "Erfurt",
    
    # VERWALTUNGSFACHSCHULEN
    hochschulstandort == "Hochschule des Bundes für öffentliche Verwaltung, FB Bundeswehrverwaltung in Mannheim" ~ "Mannheim",
    hochschulstandort == "Hochschule für den öffentlichen Dienst in Bayern Standort München (Archiv- u. Biblioth.) (Verw-FH)" ~ "München",
    hochschulstandort == "Hochschule des Bundes für öffentliche Verwaltung, FB Auswärtige Angelegenheiten in Berlin" ~ "Berlin",
    hochschulstandort == "Hochschule des Bundes für öffentliche Verwaltung, FB Sozialversicherung in Berlin" ~ "Berlin",
    hochschulstandort == "Hochschule des Bundes für öffentliche Verwaltung, FB Nachrichtendienste in Berlin" ~ "Berlin",
    hochschulstandort == "H für öffentliche Verwaltung Bremen (Verw-FH)" ~ "Bremen",
    hochschulstandort == "Akademie der Polizei Hamburg (Verw-FH)" ~ "Hamburg",
    hochschulstandort == "Norddeutsche Akademie für Finanzen und Steuerrecht Hamburg (Verw-FH)" ~ "Hamburg",
    hochschulstandort == "FH für Archivwesen Marburg (Verw-FH)" ~ "Marburg",
    hochschulstandort == "Hessische Hochschule für öffentliches Management und Sicherheit Wiesbaden in Gießen (Verw-FH)" ~ "Gießen",
    hochschulstandort == "Hessische Hochschule für öffentliches Management und Sicherheit Wiesbaden in Kassel (Verw-FH)" ~ "Kassel",
    hochschulstandort == "Hessische Hochschule für öffentliches Management und Sicherheit Wiesbaden in Wiesbaden (Verw-FH)" ~ "Wiesbaden",
    hochschulstandort == "Hochschule des Bundes für öffentliche Verwaltung, FB Kriminalpolizei in Wiesbaden" ~ "Wiesbaden",
    hochschulstandort == "Hochschule des Bundes für öffentl. Verwaltung, FB Landwirtschaftliche Sozialversicherung in Kassel" ~ "Kassel",
    hochschulstandort == "Kommunale H für Verwaltung in Niedersachsen, Hannover (Priv. Verw-FH)" ~ "Hannover",
    hochschulstandort == "Hochschule für Polizei und öffentliche Verwaltung NW in Aachen (Verw-FH)" ~ "Aachen",
    hochschulstandort == "Hochschule für Polizei und öffentliche Verwaltung NW in Bielefeld (Verw-FH)" ~ "Bielefeld",
    hochschulstandort == "Hochschule für Polizei und öffentliche Verwaltung NW in Köln (Verw-FH)" ~ "Köln",
    hochschulstandort == "Hochschule für Polizei und öffentliche Verwaltung NW in Münster (Verw-FH)" ~ "Münster",
    hochschulstandort == "Hochschule für Polizei und öffentliche Verwaltung NW in Dortmund (Verw-FH)" ~ "Dortmund",
    hochschulstandort == "Hochschule des Bundes für öffentliche Verwaltung, FB Finanzen in Münster" ~ "Münster",
    hochschulstandort == "FH für Verwaltung Saarbrücken (Verw-FH)" ~ "Saarbrücken",
    hochschulstandort == "Hochschule des Bundes für öffentliche Verwaltung, FB Bundespolizei in Lübeck" ~ "Lübeck"
    
    )
  ) 

kontextdaten_städte_aggr <- kontextdaten_städte_aufb_2 %>%
  filter(!is.na(stadt)) %>%
  filter(hochschulstandort != "IU Internationale Hochschule Erfurt in Erfurt (Priv. FH)") %>%
  group_by(stadt) %>%
  summarise(studierende_insgesamt = sum(studierende_studierende_insgesamt),
            studierende_deutsche_insgesamt = sum(studierende_studierende_deutsche_insgesamt, na.rm = T),
            studierende_auslaender_insgesamt = sum(studierende_studierende_auslaender_insgesamt, na.rm = T),
            studierende_maennlich = sum(studierende_studierende_maennlich, na.rm = T),
            studierende_weiblich = sum(studierende_studierende_weiblich, na.rm = T),
            anzahl_standorte = n()) %>%
  mutate(studierende_insgesamt_perc = studierende_insgesamt/sum(studierende_insgesamt)*100,
         studierende_deutsche_perc = studierende_deutsche_insgesamt/(studierende_deutsche_insgesamt+studierende_auslaender_insgesamt)*100,
         studierende_maennlich_perc = studierende_maennlich/(studierende_maennlich+studierende_weiblich)*100)


# Daten in Datenbank laden -----------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver             = "ODBC Driver 17 for SQL Server",
                       Server             = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database           = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt            = "No")

dbExecute(con_lokal, "DROP TABLE kontextdaten")

# dbExecute(con_lokal, "
# CREATE TABLE kontextdaten (
#     stadt NVARCHAR(50),
#     anzahl_standorte INT,
#     studierende_insgesamt INT,
#     studierende_insgesamt_perc INT,
#     studierende_deutsche_insgesamt INT,
#     studierende_auslaender_insgesamt INT,
#     studierende_deutsche_perc INT,
#     studierende_maennlich INT,
#     studierende_weiblich INT,
#     studierende_maennlich_perc INT
# )")

dbWriteTable(con_lokal, "kontextdaten", kontextdaten_städte_aggr,
             append = TRUE)

