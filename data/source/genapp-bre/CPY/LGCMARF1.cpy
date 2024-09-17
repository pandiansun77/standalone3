      ******************************************************************        
      *                                                                *        
      * LICENSED MATERIALS - PROPERTY OF IBM                           *        
      *                                                                *        
      * "RESTRICTED MATERIALS OF IBM"                                  *        
      *                                                                *        
      * CB12                                                           *        
      *                                                                *        
      * (C) COPYRIGHT IBM CORP. 2011, 2013 ALL RIGHTS RESERVED         *        
      *                                                                *        
      * US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION,      *        
      * OR DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE                   *        
      * CONTRACT WITH IBM CORPORATION                                  *        
      *                                                                *        
      *                                                                *        
      *               COPYBOOK for COMMAREA structure                  *        
      *                                                                *        
      *   This commarea can be used for all functions                  *        
      *                                                                *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           03 CA-REQUEST-ID            PIC X(6).                                
           03 CA-RETURN-CODE           PIC 9(2).                                
           03 CA-CUSTOMER-NUM          PIC 9(10).                               
      *    Fields used in INQ, UPD, ADD & DELETE policy                         
           03 CA-POLICY-REQUEST.                                                
              05 CA-POLICY-NUM         PIC 9(10).                               
      *       Common policy details                                             
              05 CA-POLICY-COMMON.                                              
                 07 CA-ISSUE-DATE      PIC X(10).                               
                 07 CA-EXPIRY-DATE     PIC X(10).                               
                 07 CA-LASTCHANGED     PIC X(26).                               
                 07 CA-BROKERID        PIC 9(10).                               
                 07 CA-BROKERSREF      PIC X(10).                               
                 07 CA-PAYMENT         PIC 9(6).                                
      *       Endowment policy description                                      
              05 CA-ENDOWMENT.                                                  
                 07 CA-E-WITH-PROFITS    PIC X.                                 
                 07 CA-E-EQUITIES        PIC X.                                 
                 07 CA-E-MANAGED-FUND    PIC X.                                 
                 07 CA-E-FUND-NAME       PIC X(10).                             
                 07 CA-E-TERM            PIC 99.                                
                 07 CA-E-SUM-ASSURED     PIC 9(6).                              
                 07 CA-E-LIFE-ASSURED    PIC X(31).                             
                 07 CA-E-PADDING-DATA    PIC X(32348).                          
