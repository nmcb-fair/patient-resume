# Mapping table for "Resumé"
map_resume <- data.frame(
  var  = c(
    "Castor Participant ID",
    "Survey Completed On",
    "age",
    "sex",
    "self_bmi",
    "calckps",
    "phq_2_score",
    "calccageaid",
    "dsqsf_cdc_v2",
    "dsqsf_ccc_v1",
    "dsqsf_iom_v1",
    # Diagnosis variables (gmh_diagnosis01#)
    "gmh_diagnosis01#ME/CVS",
    "gmh_diagnosis01#Multiple Sclerose",
    "gmh_diagnosis01#Q-koorts of Q-koortsvermoeidheidssyndroom",
    "gmh_diagnosis01#Ziekte van Lyme of post-behandelingssyndroom van de ziekte van Lyme",
    "gmh_diagnosis01#Post/Long COVID",
    "gmh_diagnosis01#Ik doe mee als gezonde deelnemer",
    # Diagnosis variables (gmh_diagnosis02#) - for Illness sheet
    "gmh_diagnosis02#Actieve ziekte van Graves (overactieve schildklier)",
    "gmh_diagnosis02#Astma en/of chronische bronchitis",
    "gmh_diagnosis02#Andere longziekten (zoals COPD of emfyseem)",
    "gmh_diagnosis02#Beroerte, bloeding in de hersenen of bloedstolsel in de hersenen (zoals TIA, CVA, cerebrale veneuze trombose)",
    "gmh_diagnosis02#Andere hart- en vaatziekten (zoals hartaanval, hartfalen of \"etalagebenen\")",
    "gmh_diagnosis02#Chronische infecties (zoals Hepatitis B, Hepatitis C,  Tuberculose, HIV)",
    "gmh_diagnosis02#Chronische ontstekingsziekte of auto-immuunziekte (zoals Reuma, Lupus, Crohn, Colitus Ulcerosa, ziekte van Wegener, Hashimoto, Sjögren)",
    "gmh_diagnosis02#Diabetes mellitus (suikerziekte)",
    "gmh_diagnosis02#Ernstige bloedarmoede, waarvoor u behandeld moet worden",
    "gmh_diagnosis02#Kanker of kankerbehandeling in de afgelopen 5 jaar",
    "gmh_diagnosis02#Momenteel een ernstige psychische aandoening (zoals depressie stoornis, bipolaire stoornis, angststoornis, dwangstoornis,  psychose, eetstoornis of schizofrenie)",
    "gmh_diagnosis02#Neurologische aandoening (zoals dementie, ziekte van Parkinson, Alzheimer, amyotrofische laterale sclerose (ALS), epilepsie, de ziekte van Huntington)",
    "gmh_diagnosis02#Nierziekte",
    "gmh_diagnosis02#Ziekte van Cushing of ziekte van Addison",
    "gmh_diagnosis02#Bij mij is geen van de bovenstaande aandoeningen vastgesteld",
    # Medication variables (meds01#) - for Medicatie sheet
    "meds01#Antibiotica (zoals Penicilline, Amoxicilline, Doxycycline)",
    "meds01#Anti-depressiva (stemming verhogende medicatie)",
    "meds01#Anti-psychotica (medicijnen om psychische stoornissen te onderdrukken)",
    "meds01#Antivirus-medicijnen (vaak eindigend op \"-vir\", of bijvoorbeeld medicijnen zoals PREP)",
    "meds01#Bloeddruk- en/of hartmedicatie",
    "meds01#Bloedverdunners",
    "meds01#Medicijnen om het immuunsysteem te onderdrukken (zogenaamde immunosuppressiva, vaak eindigend op \"-mab\")",
    "meds01#Orale anticonceptie: \"de pil\"",
    "meds01#Overgangs hormonen",
    "meds01#Plas-medicijnen (Diuretica)",
    "meds01#Sterke pijnstillers (die tot de morfine groep behoren, zoals Oxicodon, Morfine, Fentanyl, Tramadol, Codeïne)",
    "meds01#Andere pijnstillers (zoals Aspirine, Ibuprofen, diclofenac)",
    "meds01#Steroïd-hormonen (zoals Prednison, Cortisol, Testosteron)",
    "meds01#Vaccinaties (zoals de griepprik, reizigersvaccinatie of COVID)",
    "meds01#Ik heb de afgelopen 3 maanden geen van de bovenstaande medicijnen gebruikt",
    # Additional medication text
    "meds02",
    # Visit medication variables (new Castor structure)
    "visit_meds01#bloedverdunners",
    "visit_meds01#antibiotica",
    "visit_meds01#antivirale middelen",
    "visit_meds01#vaccinaties",
    "visit_meds01#pijnstillers, anders dan paracetamol",
    "visit_meds01#plaspillen",
    "visit_meds01#ACE-remmer",
    "visit_meds01#therapeutische steroïden",
    "visit_meds01#anders",
    "visit_meds01#De deelnemer heeft de afgelopen 3 maanden geen van bovenstaande medicijnen gebruikt",
    "visit_meds02"
  ),
  cell = c(
    "C2",      # Castor Participant ID
    "C3",      # Survey Completed On
    "E3",      # age
    "F3",      # sex
    "D15",     # self_bmi
    "D16",     # calckps
    "D17",     # phq_2_score
    "D18",     # calccageaid
    "E11",     # dsqsf_cdc_v2
    "E10",     # dsqsf_ccc_v1
    "E13",     # dsqsf_iom_v1
    # Diagnosis variables - add to list in C4:G4
    "C4:G4",   # gmh_diagnosis01#ME/CVS
    "C4:G4",   # gmh_diagnosis01#Multiple Sclerose
    "C4:G4",   # gmh_diagnosis01#Q-koorts of Q-koortsvermoeidheidssyndroom
    "C4:G4",   # gmh_diagnosis01#Ziekte van Lyme...
    "C4:G4",   # gmh_diagnosis01#Post/Long COVID
    "C4:G4",   # gmh_diagnosis01#Ik doe mee als gezonde deelnemer
    # Diagnosis variables - add to Illness sheet
    "Illness", # gmh_diagnosis02# variables go to Illness sheet
    "Illness",
    "Illness",
    "Illness",
    "Illness",
    "Illness",
    "Illness",
    "Illness",
    "Illness",
    "Illness",
    "Illness",
    "Illness",
    "Illness",
    "Illness",
    "Illness",
    # Medication variables - add to Medicatie sheet
    "Medicatie", # meds01# variables go to Medicatie sheet
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie", # meds01#Ik heb de afgelopen 3 maanden geen van de bovenstaande medicijnen gebruikt
    # Additional medication text
    "Medicatie", # meds02 goes to Medicatie sheet
    # Visit medication variables
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie",
    "Medicatie"  # visit_meds02 goes to Medicatie sheet
  ),
  op = c(
    NA,                    # Castor Participant ID
    NA,                    # Survey Completed On
    NA,                    # age
    "vrouw_man",           # sex
    "no_decimal",          # self_bmi
    NA,                    # calckps
    NA,                    # phq_2_score
    NA,                    # calccageaid
    "ja_nee",              # dsqsf_cdc_v2
    "ja_nee",              # dsqsf_ccc_v1
    "ja_nee",              # dsqsf_iom_v1
    # Diagnosis variables - extract text after # and add to list
    "add_to_list",         # gmh_diagnosis01#ME/CVS
    "add_to_list",         # gmh_diagnosis01#Multiple Sclerose
    "add_to_list",         # gmh_diagnosis01#Q-koorts...
    "add_to_list",         # gmh_diagnosis01#Ziekte van Lyme...
    "add_to_list",         # gmh_diagnosis01#Post/Long COVID
    "add_to_list",         # gmh_diagnosis01#Ik doe mee als gezonde deelnemer
    # Diagnosis variables - extract text and add to Illness sheet
    "add_to_sheet",        # gmh_diagnosis02# variables
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    # Medication variables - extract text and add to Medicatie sheet
    "add_to_sheet",        # meds01# variables
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",        # meds01#Ik heb de afgelopen 3 maanden geen van de bovenstaande medicijnen gebruikt
    # Additional medication text - add directly
    "add_text_to_sheet",   # meds02 (add text directly, not extract from var name)
    # Visit medication variables
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_to_sheet",
    "add_text_to_sheet"    # visit_meds02 (add text directly, not extract from var name)
  ),
  stringsAsFactors = FALSE
)
