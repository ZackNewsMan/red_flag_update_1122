library(tidyverse)

################ Red flag key code from the state courts: #################

              # Event Code	Event Code Description
              # ERPD	Extreme Risk PO Denied        
              # ERPO	Extreme Risk PO Granted       
              # ERPV	Extreme Risk PO Vacated       
              # MRNW	Motion for ERPO Renewal       
              # MTRM	Motn to Terminate Extr Risk PO
              # OMTD	Ord to Terminate ERPO-Denied  
              # OMTG	Ord to Terminate ERPO-Granted 
              # ORND	Order ERPO Renewal - Denied   
              # ORNG	Order ERPO Renewal - Granted  
              # PPXO	Petition Extreme Risk P O     
              # PTXO	Petn Temp Extreme Risk P O    
              # TRPD	Temp Extreme Risk PO Denied   
              # TRPO	Temp Extreme Risk PO Granted  
              # TRPV	Temp Extreme Risk PO Vacated  

library(readr)
red_flag_010120_112222 <- read_csv("H:/Criminal cases of interest/Red flag gun law/red_flag_010120_112222.csv")
View(red_flag_010120_112222)

red <- red_flag_010120_112222

# How many red flag cases from 01/01/20 to 11/22/22?

red %>% 
  distinct(case_number, .keep_all = TRUE)

  # 359 unique cases

    # Verified by SQL, 359 cases:
      # SELECT DISTINCT(case_number)
      # FROM red

    # Double checked, no repeats and 359 rows:

      # SELECT DISTINCT(case_number), county_name, count(case_number)
      # FROM uniq_cases
      # GROUP BY case_number
      # order by count(case_number) DESC

# Need to pull all year-long and temp orders into one pool 

  # TRPO (14-day red flag)
  
  red %>% 
    filter(event_code == "TRPO") %>% 
    distinct(case_number, .keep_all = TRUE) %>% 
    View()
  
    # 220 TRPO cases
      # Verified by SQL, 220 TRPO cases:
        # CREATE TABLE temp_approved AS
        # SELECT DISTINCT(case_number), county_name      
        # FROM red
        # WHERE event_code = "TRPO"
  
      # SQL adds one row when I add event year. I think it is because there is one case that had something occur in multiple years so SQL has added it in twice. 
        # I will need to account for this and isolate if necessary. 
  
      temp_approved <- red %>% 
        filter(event_code == "TRPO") %>% 
        distinct(case_number, .keep_all = TRUE) %>% 
        View()
      
      
  # And year-long (ERPO)
      
      red %>% 
        filter(event_code == "ERPO") %>% 
        distinct(case_number, .keep_all = TRUE) %>% 
        View()      
      
      # 168 with a year-long order
        # Same thing in SQL, 168 year-long orders
          # CREATE TABLE year_approved AS
          # SELECT DISTINCT(case_number), county_name
          # FROM red
          # WHERE event_code = "ERPO"
      
          # Goes up to 178 in SQL when I add event dtae and year. I think same thing happening as with temp orders (see line 36)
            # This code increases it:
              # SELECT DISTINCT(case_number), district_nbr, county_name, case_file_date, case_file_year, event_file_date, event_file_year, event_code, event_code_description
              # FROM red
              # WHERE event_code = "ERPO"
          
      year_approved <- red %>% 
        filter(event_code == "ERPO") %>% 
        distinct(case_number, .keep_all = TRUE)
      
 # Now let's put them together
      
      temp_approved %>% full_join(year_approved) %>% 
        View()     
      
      # Worked great! 388 rows (220 temp + 168 year = 388) and a peek under the hood shows both ERPO and TRPO cases in that data frame. 
      
          all_approved <- temp_approved %>% full_join(year_approved)
          
          all_approved %>% write_csv("all_approved.csv", na = "")
     
      # Will manually put them together and do DISTINCT in SQL to verify
          
              year_approved %>% write_csv("year_approved.csv", na = "")
              
              temp_approved %>% write_csv("temp_approved.csv", na = "")    
                   
      # And only pull distinct cases
      
      all_approved %>% 
        distinct(case_number, .keep_all = TRUE) %>% 
        View()
      
      # 228 cases with some sort of approved red flag order 
        # Verified in SQL (228 cases) and after a manual merge:
          # CREATE TABLE uniq_approved AS
          # SELECT DISTINCT(case_number), county_name
          # FROM all_approved_check
        
      uniq_all_approved <- all_approved %>% 
        distinct(case_number, .keep_all = TRUE)  

# How many cases have a approval?
    # 228 cases with some sort of approved red flag order 
    # 359 unique cases
    
      (228/359)*100

    # 64% of cases have an approval from 2020 to 2022

  # 168 cases with a year-long order
      
      (168/359)*100   
      
    # 47% of the cases had a year-long order preventing firearm ownership.   

################### County -- cases and approval #################
      
      
# Approvals per county
      
      uniq_all_approved %>% 
        group_by(county_name) %>% 
        summarize(count_approved = n()) %>% 
        arrange(desc(count_approved))    
      
      # Denver has the most by leaps and bounds -- 91 approvals
         # Verified by SQL:
            # SELECT county_name, count(case_number)
            # FROM uniq_approved
            # GROUP BY county_name
            # ORDER BY count(case_number) DESC
          
      uniq_county_approval <-  uniq_all_approved %>% 
        group_by(county_name) %>% 
        summarize(count_approved = n()) %>% 
        arrange(desc(count_approved)) 

# Cases per county 
  
      red %>% 
        distinct(case_number, .keep_all = TRUE) %>% 
        group_by(county_name) %>% 
        summarize(count_cases = n()) %>% 
        arrange(desc(count_cases))     
  
      # Denver leads with 104 cases, El Paso County with 53
      
        # Verified with SQL:
          # SELECT county_name, count(case_number)
          # FROM uniq_cases
          # GROUP BY county_name
          # ORDER BY count(case_number) DESC
     
      uniq_county_cases <- red %>% 
        distinct(case_number, .keep_all = TRUE) %>% 
        group_by(county_name) %>% 
        summarize(count_cases = n()) %>% 
        arrange(desc(count_cases))     
      
  # Left join counties with counties with approved case 
      
      left_join(uniq_county_cases, uniq_county_approval, by = "county_name") %>% 
        View()
      
    # Worked well but I want to see if I can calculate the percentage  
      
     county_cases_approval <- left_join(uniq_county_cases, uniq_county_approval, by = "county_name")
      
     county_cases_approval %>% 
     mutate(approval_perc = ((count_approved/count_cases)*100)) %>% 
     View()
      
     # Worked! :) 
      
     perc_county_cases_approval <- county_cases_approval %>% 
       mutate(approval_perc = ((count_approved/count_cases)*100))
      
     perc_county_cases_approval %>% write_csv("perc_county_cases_approval.csv", na = "")

     # Will need to roll some counties in to get an accurate picture of counties that may have multiple courts in the data (i.e. Boulder/Longmont and Arapahoe County).
     
    
##################### Renewal applied for and granted ###############
     
    # Application for renewal: MRNW
     
     red %>% 
       filter(event_code == "MRNW") %>% 
       distinct(case_number, .keep_all = TRUE) %>% 
       View()
     
      # 24 cases where applied for a renewal
        # Verified in SQL, 24 rows
         # SELECT DISTINCT(case_number), county_name
         # FROM red
         # WHERE event_code = "MRNW"
         
    # App for renewal granted: ORNG

     red %>% 
       filter(event_code == "ORNG") %>% 
       distinct(case_number, .keep_all = TRUE) %>% 
       View()
     
     # 18 where it was approved 
      # Verified in SQL, 18 rows
       # SELECT DISTINCT(case_number), county_name
       # FROM red
       # WHERE event_code = "ORNG"
        
     # Finding renewal percentage:
      # 24 cases where applied for a renewal
      # 18 where it was approved
     
        (18/24)*100
     
        # 75% of the cases with an application for a renewal was approved. 
     
    # Percentages of cases with a renewal app:
     # 359 unique cases
     # 24 cases where applied for a renewal
     
      (24/359)*100
     
      # 7% of red flag cases had an application for a renewal.      
            
########### Cases and approval per year ###########      
      
  # Will code this if there's time    
      
      
      
      
      
            
        
      
      
      