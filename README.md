#' Sushmitha Summary 

#' This function returns a list of standardized ARD (Automatic Reporting Device) variables
#' that can be used for checking ARD or PKDEF (Pharmacokinetics Definition File) variables.
#'
#' Function Parameters:
#' 
#' - ardname: A character string specifying the name of the ARD variable to retrieve.
#'            This could be ARD (Automatic Reporting Device) or PKDEF (Pharmacokinetics Definition File).
#' - ardtype: Specifies the type of variables to retrieve. It can be one of the following:
#'   - all: Return all variables.
#'   - standard: Return standard (green) variables.
#'   - additional: Return additional (yellow) variables.
#'   - optional: Return optional (orange) variables.
#'
#' Purpose:
#' This function allows users to retrieve different sets of ARD variables, 
#' which are classified by their type (standard, additional, or optional). 
#' This is useful for checking or validating the ARD or PKDEF files in the context 
#' of Non-Compartmental Analysis (NCA) or other pharmacokinetic processes.
#'
#' Examples:
#' The following examples demonstrate how to use this function:
#' 
#' - Retrieve all ARD variables.
#' - Retrieve a specific ARD variable by name (e.g., PKPTMS).
#' - Retrieve variables based on their classification (e.g., standard, optional, etc.).
#'
#' Annotations:
#' 
#' - @keywords nca opennca: Tags the function with relevant keywords for pharmacokinetic analysis and NCA.
#' - @export: Makes the function available for use when the package is loaded.
#' - @examples: Provides sample usage of the function to help users understand how to call it.
#'
#' Summary of how it works:
#' 
#' The function filters ARD or PKDEF variables based on the input parameters (ardname or ardtype).
#' If no specific name or type is provided, the function likely defaults to returning all available ARD variables.
#' This function is useful in pharmacokinetics, particularly when dealing with data files 
#' that require specific variable checks or validation.
#'






#' ardVariables
#'
#' function returns a list of standardized ARD variables
#' to use in checking ARD or PKDEF file variables
#'
#' @param ardname - ard,pkdef
#' @param ardtype - all, standard (green), additional (yellow), optional (orange)
#'
#' @keywords nca opennca
#'
#' @export
#'
#' @examples
#' # print all ardVariables
#' (ardvar <- ardVariables())
#'
#' # print ardVariable named "PKPTMS"
#' (ardvar <- ardVariables(ardname="PKPTMS"))
#'
#' # print all ardVariables using the ardtype argument set to "all"
#' (ardvar <- ardVariables(ardtype="all"))
#'
#' # print all ardVariables using the ardtype argument set to "standard"
#' (ardvar <- ardVariables(ardtype="standard"))
#'
#' # print all ardVariables using the ardtype argument set to "optional"
#' (ardvar <- ardVariables(ardtype="optional"))
#'
#' # print all ardVariables using the ardtype argument set to "additional"
#' (ardvar <- ardVariables(ardtype="additional"))
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
ardVariables <- function(ardname=NULL, ardtype="all") {
  functionName <- as.list(sys.call())[[1]]
  
  ## Standard variables - green
  result =              tibble(class="standard", variable="STUDY", pkdefvariable="STUDY ID", source="PK definition file", description="Protocol ID", valueNull=FALSE, sdeidvar=TRUE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="SITEID", pkdefvariable="CENTER NUMBER", source="PK definition file", description="Site or Center ID.", valueNull=FALSE, sdeidvar=TRUE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="SUBJID", pkdefvariable="SSID", source="PK definition file", description="Subject ID", valueNull=FALSE, sdeidvar=TRUE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="RAND", pkdefvariable="RANDOMIZATION NUMBER", source="PK definition file", description="Randomization Number", valueNull=FALSE, sdeidvar=TRUE, datatype="NUMBER")
  result <- result %>% add_row(class="standard", variable="TREATXT", pkdefvariable="TREATMENT DESCRIPTION", source="PK definition file", description="Treatment Description", valueNull=FALSE, sdeidvar=TRUE, datatype="VARCHAR2 (4000 Byte)")
  result <- result %>% add_row(class="standard", variable="TRTCD", pkdefvariable="TREATMENT CODE", source="PK definition file", description="Treatment Code", valueNull=FALSE, sdeidvar=TRUE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="PKCOLL", pkdefvariable="COLLECTION", source="PK definition file", description="Sample Collection Type (Interval or Point only)", valueNull=FALSE, sdeidvar=TRUE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="PKBDFLD", pkdefvariable="MATRIX", source="PK definition file", description="Matrix [Blood, Plasma, Tissue,Urine]", valueNull=FALSE, sdeidvar=TRUE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="PKTERM", pkdefvariable="ANALYTE NAME", source="PK definition file", description="Analyte Name", valueNull=FALSE, sdeidvar=TRUE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="standard", variable="PERIODU", pkdefvariable="PERIOD UNIT", source="PK definition file", description="Peiod Unit", valueNull=TRUE, sdeidvar=TRUE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="PERIOD", pkdefvariable="PERIOD", source="PK definition file", description="Period Identifier", valueNull=TRUE, sdeidvar=TRUE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="VISITU", pkdefvariable="VISIT UNIT", source="PK definition file", description="Visit Unit", valueNull=FALSE, sdeidvar=TRUE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="VISIT", pkdefvariable="VISIT", source="PK definition file", description="Visit Identifier", valueNull=FALSE, sdeidvar=TRUE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="PHASE", pkdefvariable="PHASE", source="PK definition file", description="Study Phase or Cycle", valueNull=TRUE, sdeidvar=TRUE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="PCANMETH", pkdefvariable="PCANMETH", source="PK Supplemental File", description="Analysis Method", valueNull=TRUE, sdeidvar=FALSE, datatype="CHAR")
  result <- result %>% add_row(class="standard", variable="PCMETHOD", pkdefvariable="PCMETHOD", source="PK Supplemental File", description="Bioanalytical Method", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2(200 Byte)")
  result <- result %>% add_row(class="standard", variable="PCNAM", pkdefvariable="PCNAM", source="PK Supplemental File", description="Bioanalytical Method", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2(200 Byte)")
  result <- result %>% add_row(class="standard", variable="PCLLOQ", pkdefvariable="PCLLOQ", source="PK Supplemental File", description="Bioanalytical  Assay LLOQ", valueNull=TRUE, sdeidvar=FALSE, datatype="NUMBER")
  result <- result %>% add_row(class="standard", variable="PCSTRESU", pkdefvariable="PCSTRESU", source="PK Supplemental File", description="Unit of standardized concentration result", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2(100 Byte)")
  result <- result %>% add_row(class="standard", variable="PKUSMID", pkdefvariable="PKUSMID SAMPLE ID", source="PK definition file/PIMS barcode", description="Unique Sample ID", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="PKACES", pkdefvariable="PFIZER/VENDOR ACCESSION", source="PK definition file", description="Vendor Sample Accession ID", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="PKSCOM", pkdefvariable="PFIZER COMMENTS", source="PK definition file", description="Pfizer Sponsor Comments", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (4000 Byte)")
  result <- result %>% add_row(class="standard", variable="PKAACES", pkdefvariable="ANALYTICAL LABORATORY ACCESSION", source="PK definition file", description="Analytical Laboratory Accession Number", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="NTPDU", pkdefvariable="TIME POST DOSE UNIT", source="PK definition file", description="Nominal Time Post Dose ", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="PKPTMR", pkdefvariable="TIME POST DOSE OR INTERVAL", source="PK definition file", description="Nominal TPD or Interval Value", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="NTPDMN", pkdefvariable="MINUTE NOMINAL", source="PK definition file", description="Nominal TPD Minute Value", valueNull=TRUE, sdeidvar=FALSE, datatype="NUMBER")
  result <- result %>% add_row(class="standard", variable="PKCNC", pkdefvariable="ANALYTE RESULT", source="PK definition file", description="Concentration Result", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="PKCNCU", pkdefvariable="ANALYTE UNIT", source="PK definition file", description="Concentration Unit", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="PKAMT", pkdefvariable="AMOUNT", source="PK definition file", description="Amount - Urine Volume; Tissue Weight", valueNull=TRUE, sdeidvar=FALSE, datatype="NUMBER")
  result <- result %>% add_row(class="standard", variable="PKAMTU", pkdefvariable="AMOUNT UNIT", source="PK definition file", description="Amount Unit - Urine Volume Units; Tissue Weight Units", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="PKSAMQA", pkdefvariable="QA STATUS", source="PK definition file", description="Sample QA Status", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="standard", variable="PKCOML", pkdefvariable="CENTRAL LABORATORY COMMENTS", source="PK definition file", description="Central Lab Sample Comment or Code", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (4000 Byte)")
  result <- result %>% add_row(class="standard", variable="PKACOM", pkdefvariable="ANALYTICAL LABORATORY COMMENTS", source="PK definition file", description="Analytical Lab Sample Comment or Code", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (4000 Byte)")
  result <- result %>% add_row(class="standard", variable="PKCOMC", pkdefvariable="", source="Database/Raw PK CRF domain", description="PK CRF Comment", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (4000 Byte)")
  result <- result %>% add_row(class="standard", variable="WT", pkdefvariable="", source="Database/Raw vitals domain", description="Subject Weight (KG) at baseline derived", valueNull=TRUE, sdeidvar=FALSE, datatype="NUMBER")
  result <- result %>% add_row(class="standard", variable="WTU", pkdefvariable="", source="Database/Raw vitals domain", description="Subject Weight Unit  (KG) at baseline derived", valueNull=TRUE, sdeidvar=FALSE, datatype="NUMBER")
  result <- result %>% add_row(class="standard", variable="WTUNI", pkdefvariable="", source="Database/Raw vitals domain", description="Subject Raw Weight Unit at baseline", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="WTRAW", pkdefvariable="", source="Database/Raw vitals domain", description="Subject Raw Weight Value at baseline", valueNull=TRUE, sdeidvar=FALSE, datatype="NUMBER")
  result <- result %>% add_row(class="standard", variable="AGEDERU", pkdefvariable="", source="Database/Raw vitals domain", description="Baseline Derived Age Units", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="AGEDER", pkdefvariable="", source="Database/Raw vitals domain", description="Baseline Derived Age", valueNull=TRUE, sdeidvar=FALSE, datatype="NUMBER")
  result <- result %>% add_row(class="standard", variable="PKATM", pkdefvariable="", source="Database/Raw PK CRF domain", description="Actual Time of Start of Sample Collection", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte) [HH24:MM]")
  result <- result %>% add_row(class="standard", variable="PKADT", pkdefvariable="", source="Database/Raw PK CRF domain", description="Actual Date of Start of Sample Collection", valueNull=TRUE, sdeidvar=FALSE, datatype="DATE [YYYY-MM-DD]")
  result <- result %>% add_row(class="standard", variable="PKSMND", pkdefvariable="", source="Database/Raw PK CRF domain", description="PK Sample Record Not Done Status", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="standard", variable="PKND", pkdefvariable="", source="Database/Raw PK CRF domain", description="PK Sample CRF Module/Page Not Done Status", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="standard", variable="PKSMVLU", pkdefvariable="", source="Database/Raw PK CRF domain", description="Matrix Volume Units", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="standard", variable="PKSMVL", pkdefvariable="", source="Database/Raw PK CRF domain", description="Matrix Volume", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="standard", variable="PKATME", pkdefvariable="", source="Database/Raw PK CRF domain", description="Actual Start Time of Sample Collection", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte) [HH24:MM]")
  result <- result %>% add_row(class="standard", variable="PKADTE", pkdefvariable="", source="Database/Raw PK CRF domain", description="Actual End Time of Sample Collection", valueNull=TRUE, sdeidvar=FALSE, datatype="DATE [YYYY-MM-DD]")
  result <- result %>% add_row(class="standard", variable="DOSE", pkdefvariable="", source="Database/Raw Dosing", description="Dose", valueNull=TRUE, sdeidvar=FALSE, datatype="NUMBER")
  result <- result %>% add_row(class="standard", variable="DOSEUNI", pkdefvariable="", source="Database/Raw Dosing Domain", description="Dose Unit", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2")
  result <- result %>% add_row(class="standard", variable="DRGDATE", pkdefvariable="", source="Database/Raw Dosing Domain", description="Date of Dosing Event", valueNull=TRUE, sdeidvar=FALSE, datatype="DATE [YYYY-MM-DD]")
  result <- result %>% add_row(class="standard", variable="DOSETIM", pkdefvariable="", source="Database/Raw Dosing Domain", description="Time of Dosing Event", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte) [HH24:MM]")
  result <- result %>% add_row(class="standard", variable="ACTTRT", pkdefvariable="", source="Randomization file/treatment map", description="Actual Treatment", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="standard", variable="ACTTRTC", pkdefvariable="", source="Randomization file/treatment map", description="Actual Treatment Code", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="ACTTRTS", pkdefvariable="", source="Randomization file/treatment map", description="Actual Treatment Sequence", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="standard", variable="ACSTDTF", pkdefvariable="", source="Database/Raw Dosing Domain", description="Dosing Start Date for IV ", valueNull=TRUE, sdeidvar=FALSE, datatype="DATE [YYYY-MM-DD]")
  result <- result %>% add_row(class="standard", variable="ACSTTMF", pkdefvariable="", source="Database/Raw Dosing Domain", description="Dosing Start Time for IV", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte) [HH24:MM]")
  result <- result %>% add_row(class="standard", variable="ACENDTF", pkdefvariable="", source="Database/Raw Dosing Domain", description="Dosing Stop Time for IV", valueNull=TRUE, sdeidvar=FALSE, datatype="DATE [YYYY-MM-DD]")
  result <- result %>% add_row(class="standard", variable="ACENTMF", pkdefvariable="", source="Database/Raw Dosing Domain", description="Dosing Stop Date for IV", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte) [HH24:MM]")
  result <- result %>% add_row(class="standard", variable="DOF", pkdefvariable="", source="Derived", description="Duration of Infusion", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="DOFU", pkdefvariable="", source="Derived", description="Duration of Infusion Unit", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="ROUTE", pkdefvariable="", source="Database/Raw Dosing Domain", description="Route of Administration [Oral, IV, SC]", valueNull=FALSE, sdeidvar=FALSE, datatype="VARCHAR2(100 Byte)")
  result <- result %>% add_row(class="standard", variable="FLGEMESIS", pkdefvariable="", source="", description="Flag indicating that emesis occurred prior to or during dosing interval\n0 = No Emesis has occurred prior or during the dosing interval for a subject\n 1 = Emesis has occurred prior or during the dosing interval for a subject", valueNull=FALSE, sdeidvar=FALSE, datatype="NUMBER")
  result <- result %>% add_row(class="standard", variable="FLGSMPLPOSTDOSE", pkdefvariable="", source="", description="FLGSMPLPOSTDOSE a dataset flag that is used to indicate whether a sample planned to be taken prior to the planned dose, is actually taken following the planned dose. When available it is incorporated into the analysis dataset.\n 0 = pk sample occurs prior to the planned dose rather than prior\n 1 = pk sample occurs following to the planned dose rather than as planned prior to the dose", valueNull=FALSE, sdeidvar=FALSE, datatype="NUMBER")
  result <- result %>% add_row(class="standard", variable="HT", pkdefvariable="", source="Database/Raw vitals domain", description="Baseline Subject Height", valueNull=TRUE, sdeidvar=FALSE, datatype="NUMBER")
  result <- result %>% add_row(class="standard", variable="HTUNI", pkdefvariable="", source="Database/Raw vitals domain", description="Subject Raw Height Unit", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="HTRAW", pkdefvariable="", source="Database/Raw vitals domain", description="Subject Raw Height Value", valueNull=TRUE, sdeidvar=FALSE, datatype="NUMBER")
  result <- result %>% add_row(class="standard", variable="RACEOTH", pkdefvariable="", source="Database/Raw demographic domain", description="Race Other - Text for other race", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="RACES", pkdefvariable="", source="Database/Raw demographic domain", description="Subject Race", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="SEX", pkdefvariable="", source="Database/Raw demographic domain", description="Subject Gender / Sex", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="RACIALD", pkdefvariable="", source="Database/Raw demographic domain", description="Racial Designation", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="ETHNIC", pkdefvariable="", source="Database/ vitals domain?", description="Subject Ethnicity", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="standard", variable="SMOKSTA", pkdefvariable="", source="Database/Raw demographic domain", description="No longer loaded in eNCA. Legacy field.", valueNull=TRUE, sdeidvar=FALSE, datatype="Not Defined")
  ## Additional - yellow
  result <- result %>% add_row(class="additional", variable="PKCNCN", pkdefvariable="", source="openNCA derived", description="Numeric value of supplied concentration value (PKCNC), adjusted so that LLOQ/BLQ values (<xx.xx) are set to zero", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="additional", variable="PKPTM", pkdefvariable="", source="openNCA derived", description="Nominal Time Post Dose", valueNull=FALSE, sdeidvar=FALSE, datatype="Not Defined")
  result <- result %>% add_row(class="additional", variable="PKPTMI", pkdefvariable="", source="openNCA derived", description="Nominal Time Post Dose", valueNull=FALSE, sdeidvar=FALSE, datatype="Not Defined")
  result <- result %>% add_row(class="additional", variable="PKPTMS", pkdefvariable="", source="openNCA derived", description="Planned PK Sample Time Start", valueNull=TRUE, sdeidvar=FALSE, datatype="Not Defined")
  result <- result %>% add_row(class="additional", variable="PKPTME", pkdefvariable="", source="openNCA derived", description="For 'interval' samples if PKPTMR isn't formated as an interval string, i.e. '(0H)-(6H)'. This alternative would be recorded as a numeric value (in string format), i.e. '6' would represent the end of the interval example in this description. Units recorded in PKPTMU value", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="additional", variable="PKPTMU", pkdefvariable="", source="openNCA derived", description="PK Planned Time Unit for either PKPTMR or PKPTME", valueNull=FALSE, sdeidvar=FALSE, datatype="VARCHAR2(100 Byte)")
  result <- result %>% add_row(class="additional", variable="PKATPDE", pkdefvariable="", source="openNCA derived", description="For 'interval' samples if PKPTMR isn't formated as an interval string, i.e. '(0H)-(6H)'. This alternative would be recorded as a numeric value (in string format), i.e. '6' , or '6.25' would represent the end of the interval example in this description. Units recorded in PKPTMU value", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="additional", variable="PKATPDEU", pkdefvariable="", source="openNCA derived", description="0", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="additional", variable="PKATPD", pkdefvariable="", source="openNCA derived", description="Actual Time Post Dose or Actual Post Dose Start (Value)", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  result <- result %>% add_row(class="additional", variable="PKATPDU", pkdefvariable="", source="openNCA derived", description="Actual Time Post Dose Start (Units) (Datafile provided; eNCA produced)", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")
  ## Optional - orange
  result <- result %>% add_row(class="optional", variable="COLLDATE", pkdefvariable="", source="Database/PK CRF?", description="Collection DATE OF CRF Page", valueNull=TRUE, sdeidvar=FALSE, datatype="DATE [YYYY-MM-DD]")
  result <- result %>% add_row(class="optional", variable="PKSMMSU", pkdefvariable="", source="Database/PK CRF?", description="Matrix Mass Units", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="optional", variable="PKSMMS", pkdefvariable="", source="Database/PK CRF?", description="Matrix Mass", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="optional", variable="TREATSEQ", pkdefvariable="", source="treatmap", description="Treatment Sequence #", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2(100 Byte)")
  result <- result %>% add_row(class="optional", variable="UDSDEID", pkdefvariable="", source="", description="This field permits the PK Analyst to create a different encoding than the automatically generated 15 field derived SDEID to reassign data to individual profiles. ", valueNull=TRUE, sdeidvar=FALSE, datatype="NUMBER")
  result <- result %>% add_row(class="optional", variable="FU", pkdefvariable="", source="", description="Fraction of the drug circulating free/unbound in blood.", valueNull=TRUE, sdeidvar=FALSE, datatype="NUMBER")
  result <- result %>% add_row(class="optional", variable="ACTDTLS", pkdefvariable="", source="", description="Reason Preassigned Treatment Not Given", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2(4000 Byte)")
  result <- result %>% add_row(class="optional", variable="EXENDTF", pkdefvariable="", source="Database/Raw Dosing Domain", description="Date of last treatment Char", valueNull=TRUE, sdeidvar=FALSE, datatype="DATE [YYYY-MM-DD]")
  result <- result %>% add_row(class="optional", variable="EXENTMF", pkdefvariable="", source="Database/Raw Dosing Domain", description="Time of last treatment Char", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte) [HH24:MM]")
  result <- result %>% add_row(class="optional", variable="EXSTDTF", pkdefvariable="", source="Database/Raw Dosing Domain", description="Date of first treatment Char", valueNull=TRUE, sdeidvar=FALSE, datatype="DATE [YYYY-MM-DD]")
  result <- result %>% add_row(class="optional", variable="EXSTTMF", pkdefvariable="", source="Database/Raw Dosing Domain", description="Time of first treatment Char", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte) [HH24:MM]")
  result <- result %>% add_row(class="optional", variable="DOSEP", pkdefvariable="", source="", description="Planned Dose (Drug name + treatment) (Its the Actual Dose captured in PIMS) (Not to be confused with the 'Planned' name used)", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2(200 Byte)")
  result <- result %>% add_row(class="optional", variable="DOSFRM", pkdefvariable="", source="Database/Raw Dosing Domain", description="Formulation of Dose", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2(4000 Byte)")
  result <- result %>% add_row(class="optional", variable="DOSPND", pkdefvariable="", source="", description="Planned Dose Not Done", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="optional", variable="DSPTM", pkdefvariable="", source="", description="Planned/Nominal Timepoint", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="optional", variable="INFVLF", pkdefvariable="", source="Database/Raw Dosing Domain", description="Volume Infused Char", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="optional", variable="INFVLU", pkdefvariable="", source="Database/Raw Dosing Domain", description="Volume Infused Units", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="optional", variable="SOLCNCF", pkdefvariable="", source="", description="Concentration of Solution Char", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="optional", variable="SOLCNU", pkdefvariable="", source="", description="Concentration of Solution Units", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="optional", variable="SOLVLF", pkdefvariable="", source="", description="Volume of Solution Administered Char", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="optional", variable="SOLVLU", pkdefvariable="", source="", description="Administered Solution Vol Units", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte)")
  result <- result %>% add_row(class="optional", variable="MENDTF", pkdefvariable="", source="", description="Stop Date of Meal Char", valueNull=TRUE, sdeidvar=FALSE, datatype="DATE [YYYY-MM-DD]")
  result <- result %>% add_row(class="optional", variable="MENTMF", pkdefvariable="", source="", description="Stop Time of Meal Char", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte) [HH24:MM]")
  result <- result %>% add_row(class="optional", variable="MSTDTF", pkdefvariable="", source="", description="Start Date of Meal Char", valueNull=TRUE, sdeidvar=FALSE, datatype="DATE [YYYY-MM-DD]")
  result <- result %>% add_row(class="optional", variable="MSTTMF", pkdefvariable="", source="", description="Start Time of Meal Char", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2 (200 Byte) [HH24:MM]")
  result <- result %>% add_row(class="optional", variable="TOLD", pkdefvariable="", source="", description="Time of last dose for a concentration time profile", valueNull=TRUE, sdeidvar=FALSE, datatype="NUMBER")
  result <- result %>% add_row(class="optional", variable="DATAPREPFILECOM", pkdefvariable="", source="", description="2017-07-20/TGT/ Updated upon consultation with Stuart Pearce. DATAPREPFILECOM will capture an overall data preparation comment for the file. ", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2(4000)")
  result <- result %>% add_row(class="optional", variable="DATAPREPROWCOM", pkdefvariable="", source="", description="Added in V3.0 of eNCA Data Standard\n Comments as noted by the data preparer\n For Production and Legacy LCD and legacy LPD files, every row may have a value but the value MUST be the same since ONLY the VALUE entered on the first will take effect.\n APS, CRDC or external CROs will not populate this field for Production LCD Files.  If this field is populated in error, the autoloader will not read/apply this field for Production LCD Files.  The autoloader code for Production LCD Files will stop at the creation of an Analysis Dataset within eNCA.\nThis field should ONLY be applicable to files for Legacy Data loading and NOT for production data loading for parameters.  There is no production data loading for PK parameters. If this file is inadvertently included with a production LCD, it should not be loaded. Loading of data from external vendors, when applicable, will be treated as for Legacy Data loading.\n2017-07-20/TGT/ Updated upon consultation with Stuart Pearce. These will be record by record individual comments that the data programmer can add to the data file directly", valueNull=TRUE, sdeidvar=FALSE, datatype="VARCHAR2(4000)")
  result <- result %>% add_row(class="optional", variable="DATASTATUS", pkdefvariable="", source="", description="QA status of supplied data such as DRAFT or FINAL\n Note that this does not refer to the QA status of the bioanalytical result (see PKSAMQA) but rather the CRF data merged/added to the file.\n For Legacy LPD and VPD Files, every row will have a value but the value MUST be the same since ONLY the VALUE entered on the first record will take effect.\n This field should ONLY be applicable to files for Legacy Data loading and NOT for production data loading for parameters.  There is no production data loading for PK parameters. If this file is inadvertently included with a production LCD, it should not be loaded. Loading of data from external vendors, when applicable, will be treated as for Legacy Data loading.\n Note for PKPView, DATASTATUS in the view will be updated when PK analyst updates data status on the Analysis Page of eNCA", valueNull=FALSE, sdeidvar=FALSE, datatype="VARCHAR2 (100 Byte)")

# sushmitha Comments
# If the 'ardname' argument is not NULL, this section filters the 'result' 
# data frame to return only the row(s) where the 'variable' column matches 
# the value of 'ardname'.
  
  if(!is.null(ardname)) {
    result <- result %>% filter(variable==ardname)
  }


  # # If 'ardtype' is not set to "all", this section filters the 'result' 
# data frame to return only the row(s) where the 'class' column matches 
# the specified 'ardtype' (e.g., "standard", "optional", "additional").

  if(ardtype!="all") {
    result <- result %>% filter(class==ardtype)
  }

  # Finally, the function returns the filtered 'result' data frame.
  return(result)
}
