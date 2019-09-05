# danl
private danl repo

README Contents:
INTRO. General comments
A-I. Notes regarding scripts for:
    a) tidying raw data (1 specific to each qualtrics questionnaire)
    b) scaling tidy data and writing out csv and sav files (finalized)
    
#####################################################################
# Data was extracted from qualtrics on March 8, 2019. ###############
#-------------------------------------------------------------------#
# All dates are converted to type "Date", with internal calculation #
# of days proceeding from origin of 01/01/1970 ######################
#####################################################################
INTRO - General Comments
I. If reading in scaled .csv files, use StringsAsFactors = FALSE
II. If reading in a .csv file, the date will have reverted back to 
    character format. To fix this, reference the first couple chunks
    of code contained in the scaling script for that form.
(III. added a variable to indicate if more than 20% of data is missing
  a: recompute means as "adjusted" excluding the above participants)
IV. The following participants also have PACCT subject IDs
  a. EL144 = PA051
  b. EL145 = PA052
  c. EL146 = PA058
  d. EL147 = PA059
  e. EL148 = PA061
  f. EL149 = PA109
  g. EL140 = PA090*
  h. EL141 = PA091*
  
*These two participants' data is complete in ELFK, but some PACCT data needs to be
merged from ELFK to the PACCT database.

A. Notes specific to the Security Scale
  1. Mock IDs removed:
    a) "111"
    b) "999"
    c) "fgh"
  2. Duplicated subject entered data:
    a) "EL001"
    b) "EL017"
  3. SS_tidy data does not contain the PAACT merge, this occurs
      during scaling of data (see "SS scaling.Rmd")
  4. IMPORT ALL .CSV with argument: stringsAsFactors = FALSE

B. Notes specific to the SCARED Scale
  1. Mock IDs removed:
    a) "111"
    b) "999"
  2. Duplicated subject IDs to delete:
    a) "el041", "EL127"
  3. The following subjects entered data twice (with the same ID); retain the OLDEST:
    a) "EL104", "EL144", "098"
  4. Fix typo for one participant where letter "l" was used instead of # "1"
    a) "el06lp" needs to be changed to "EL061" [COPY AND PASTE THIS if necessary]
  5. EL016 and EL016_2:
    a) EL016_2 is the correct entry for this participant.
    
C. Notes Specific to the Motivation Scale
  1. Duplicate entries
    a) "EL025" filled out the survey twice - delete the newer entry.
  2. Mock/test data
    a) Entry with subject ID "999"
  3. Qualtrics prints out a range of numbers from 0 to 10
    a) Instead, this is shifted so that it is coded from 1 to 11, so subtract 1.
    
D. Notes Specific to the Grit Child version
  1. several blank entries (4 in quantity) were deleted from the 
  subject ID field
  2. [also true for parent Grit] Recoding necessary before "scoring" to correct Qualtrics coding error
    a) Recode to accomplish the following:
      i) Greater agreement with positively worded items is coded with higher numbers
      ii) Greater agreement with negatively worded items will produce lower numbers
      iii) Higher mean score will reflect higher grit
      iv) Lower mean scores will reflect lower grit
    b) Flip or "reverse-code" the positively-worded items.
    c) Simply rename the negatively-worded items to indicate they are (already) reverse-coded
  
E. Notes specific to the DSM Child cross-cutting KSADS sympots
  1. Binary variables are recoded from qualtrics output such that
    "No=2" becomes "No=0".
  (2. in qualtrics output, "Yes" is already coded 1)
  3. Qualtrics output doesn't need to be coded, but shifted [subtract "1" from each value]
  
F. LEQ - Qualtrics data is split between qualtrics output, and a seperate
hand-entered table. They should be merged.
  1. Finalized to be merged folder contains the hand-coded responses, in the format of the qualtrics version, so some participants' data was added in seperately that way, partially adapted from a slighyl different form.

G. Some children filled out the parent version of the puberty scale.
  1. Script searches through and classifies parent and child report.
  2. When merging in paact participants, this creates a duplicate within the parent reports.
    i) EL149- Retain the entry that was filled out earlier, based on the child's height was 2 inches shorter.
    ii) EL144- same decision rule as above
    iii) EL145- same decision rule as above

H. Notes specific to the IAI
  1. For PACCT participants, the following questions of the IAI are found in the PACCT CGH
    a) IAI_12 = CGH_7
    b) IAI_13 = CGH_8A_EXPLAIN
    c) IAI_16 = CGH_13
    d) IAI_17 = CGH_17A_EXPLAIN
    e) RecordedDate
  
I. Notes Specific to the Grit Parent version
  2. [also true for parent Grit] Recoding necessary before "scoring" to correct Qualtrics coding error
    a) Recode to accomplish the following:
      i) Greater agreement with positively worded items is coded with higher numbers
      ii) Greater agreement with negatively worded items will produce lower numbers
      iii) Higher mean score will reflect higher grit
      iv) Lower mean scores will reflect lower grit
    b) Flip or "reverse-code" the positively-worded items.
    c) Simply rename the negatively-worded items to indicate they are (already) reverse-coded

J. Notes specific to the AIOI    
  a) Two participants entered identifiable data into the text fields
    intended for their "other" education if applicable.
    (i) This has been removed in finalized data.
  b) several duplicated entries were found and removed.

K. Notes specific to the CQ
  a) For the one same-sex couple (EL143), an additional code was added for CQ_1
    i) "Mother and mother" is coded 5
  b) For the other two questions, the same-sex couple chose answers equal to the answer choices available to the rest of the participants, and their answers were thus coded the same.
    i) For CQ_2 "both parents" is coded .5 for EL143 as well as the rest of subjects
    ii) For CQ_3 "one of their primary caregivers" for EL143 is coded 1, consisten with the coding of "one parent" for the rest of subjects
  c) The data for the first 31 subjects was manually coded and reproduced in the this form's tidying script. It is then merged into the Qualtrics data before the final dataframe is written out.
  
L. Notes specific to the RADS
  a) The parent of EL146 and EL147 filled out the form under the ID EL147
  b) The second entry was determined to be in reference to EL146
    i) The IDENT_SUBID for this entry was changed to EL146
    
M. Notes specific to the RPQ
  a) The parent of EL144 and EL145 filled out the form for EL145 three times.
  b) The second entry, which was filled out immediately after that for EL144, is retained.
    i) This is consistent with Michelle L's suggestion/notes.
  c) The numeric questions were reversed by Qualtrics, and shifted +1
    i) These items were thus reversed (and shifted down 1)
    ii) The new range is 0 to 3
    iii) Higher scores reflect a higher degree of problems in relationships 

N. Notes specific to the FQ
  a) For the duplicate entry for EL040, take the second entry
    i) This is contradictory to the usual rule, but data was more complete.
    
O. Notes specific to the STAI
  a) The STAI-State was only collected for the first ~30 participants
    i) Further, the raw data asked the state questions two times.
    ii) Repeated questions are deleted in the tidy and scored data sets.
  b) The STAI-Trait questions were asked for the remainder of participants
  c) The STAI-State questions for most paticipants will thus remain `NA`
  
P. Notes specific to the BDI
  a) ELFK-only participants completed a modified BDI-1A
    i) The question pertaining to suicidality was removed
    ii) Two questions were created pertaining to weight gain and sleep hygeine
    iii) The sum score did not incorporate the two new items
  b) PACCT participants had completed the BDI-2
  c) Sum scores from both samples were pooled and z-scores generated
  
Q. Notes specific to the CRPR
  a) Entry "EL041P" was determined to be a typo for participant "EL042"
  b) We found that entries within families, between siblings, were highly correlated,
  and often identical at the level of the subscale scores. Thus, for parents who answered the survey more than once using the subject ID of only kid both times, we duplicated this enry for all siblings and relabelled the copies with the IDs of the other children.
  c) One duplicate of EL096 was determined to be a typo, but it could not be determined what the subject id should have been. Thus, this entry was deleted, and the other was retained, and was determined to be the correct data for EL096.
  
  
**PPDS was filled out by parent, for both the child and the parent. Difficult to determine which responses are (a) filled out by a parent on a child's behalf, or (b) filled out by a parent, with information/responses based on the parent themself.

To answer some IAI questions, the KSADS excel sheet contains the "adoption story"


All surveys in data_entry_merge>finalized copied to be merged
KSADS,  was coded by hand and has one tab with info that belongs on the data entry manual, and the data itself which needs to be merged in in a slightly different way.
